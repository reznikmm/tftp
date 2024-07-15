--  SPDX-FileCopyrightText: 2024 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
----------------------------------------------------------------

package body TFTP is

   procedure Check_Send_Status
     (Self   : in out Reader'Class;
      Status : Net.Error_Code);

   procedure Send_ACK
     (Self   : in out Reader'Class;
      Packet : in out Net.Buffers.Buffer_Type;
      To     : Net.Sockets.Sockaddr_In;
      Block  : Net.Uint16;
      Status : out Net.Error_Code);

   procedure Send_Error
     (Self   : in out Reader'Class;
      Packet : in out Net.Buffers.Buffer_Type;
      To     : Net.Sockets.Sockaddr_In);

   ----------
   -- Bind --
   ----------

   procedure Bind
     (Self  : access Reader'Class;
      Ifnet : access Net.Interfaces.Ifnet_Type'Class;
      Addr  : Net.Sockets.Sockaddr_In) is
   begin
      Self.Our_Port := Addr.Port;
      Net.Sockets.Udp.Bind (Self, Ifnet, Addr);
   end Bind;

   -----------------------
   -- Check_Send_Status --
   -----------------------

   procedure Check_Send_Status
     (Self   : in out Reader'Class;
      Status : Net.Error_Code)
   is
      use type Net.Error_Code;
   begin
      if Status /= Net.EOK then
         Self.Failure ((Send_Status => True, ACK_Send_Status => Status));
      end if;
   end Check_Send_Status;

   ---------------
   -- Read_File --
   ---------------

   procedure Read_File
     (Self   : in out Reader'Class;
      Host   : Net.Ip_Addr;
      Name   : String;
      Port   : Net.Uint16 := Net.Headers.To_Network (69);
      Status : out Net.Error_Code)
   is
      Packet : Net.Buffers.Buffer_Type;
   begin
      Net.Buffers.Allocate (Packet);

      if Packet.Is_Null then
         Status := Net.ENOBUFS;

      else
         Packet.Set_Type (Net.Buffers.UDP_PACKET);
         Packet.Put_Uint16 (1);  --  Read request (RRQ)
         Packet.Put_String (Name, With_Null => True);
         Packet.Put_String ("octet", With_Null => True);

         Self.Expect_Block := 1;
         Self.Host := (Port => 0, Addr => Host);  --  Port to be agreed

         Self.Send
           (To     => (Port, Host),
            Packet => Packet,
            Status => Status);
      end if;
   end Read_File;

   -------------
   -- Receive --
   -------------

   overriding procedure Receive
     (Self   : in out Reader;
      From   : Net.Sockets.Sockaddr_In;
      Packet : in out Net.Buffers.Buffer_Type)
   is
      use type Net.Uint16;

      Status : Net.Error_Code;
      Opcode : Net.Uint16;
      Block  : Net.Uint16;
      Error  : Net.Uint16;
      EOF    : constant Boolean := Packet.Available < 512 + 4;
   begin
      if Packet.Available >= 4 then
         Opcode := Packet.Get_Uint16;

         case Opcode is
            when 3 =>  --  Data
               Block := Packet.Get_Uint16;

               if Self.Host.Port not in 0 | From.Port then
                  Self.Send_Error (Packet, From);

               elsif Block = Self.Expect_Block then
                  if Block = 1 then
                     Self.Host.Port := From.Port;
                  end if;

                  Reader'Class (Self).Receive (Packet, EOF);

                  Self.Send_ACK (Packet, From, Block, Status);
                  Self.Check_Send_Status (Status);

                  Self.Expect_Block := Self.Expect_Block + 1;

               elsif Block = Self.Expect_Block - 1 then

                  Self.Send_ACK (Packet, From, Block, Status);
                  Self.Check_Send_Status (Status);
               end if;

            when 5 =>  --  Error
               if Self.Host.Port in 0 | From.Port then
                  Error := Packet.Get_Uint16;
                  Reader'Class (Self).Failure ((False, Error));
               else
                  Self.Send_Error (Packet, From);
               end if;

            when others =>
               null;  --  discard unexpected packet
         end case;
      else
         null;  --  discard bad packet
      end if;

      Packet.Release;
   end Receive;

   --------------
   -- Send_ACK --
   --------------

   procedure Send_ACK
     (Self   : in out Reader'Class;
      Packet : in out Net.Buffers.Buffer_Type;
      To     : Net.Sockets.Sockaddr_In;
      Block  : Net.Uint16;
      Status : out Net.Error_Code) is
   begin
      Packet.Set_Data_Size (0);
      Packet.Put_Uint16 (4);  --  ACK
      Packet.Put_Uint16 (Block);

      Self.Send (To, Packet, Status);
   end Send_ACK;

   ----------------
   -- Send_Error --
   ----------------

   procedure Send_Error
     (Self   : in out Reader'Class;
      Packet : in out Net.Buffers.Buffer_Type;
      To     : Net.Sockets.Sockaddr_In)
   is
      Ignore : Net.Error_Code;
   begin
      Packet.Set_Data_Size (0);
      Packet.Put_Uint16 (5);  --  ERROR
      Packet.Put_Uint16 (5);  --  Unknown transfer ID.
      Packet.Put_String ("Bad TID", With_Null => True);

      Self.Send (To, Packet, Ignore);
   end Send_Error;

   -------------
   -- Timeout --
   -------------

   procedure Timeout
     (Self   : in out Reader'Class;
      Status : out Net.Error_Code)
   is
      use type Net.Uint16;
      Packet : Net.Buffers.Buffer_Type;
   begin
      Net.Buffers.Allocate (Packet);

      if Packet.Is_Null then
         Status := Net.ENOBUFS;
      else
         Self.Send_ACK (Packet, Self.Host, Self.Expect_Block - 1, Status);
      end if;
   end Timeout;

end TFTP;
