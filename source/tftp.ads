--  SPDX-FileCopyrightText: 2024 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
----------------------------------------------------------------

with Net;
with Net.Buffers;
with Net.Headers;
with Net.Interfaces;
with Net.Sockets.Udp;

package TFTP is

   type Reader is abstract tagged limited private;

   procedure Bind
     (Self  : access Reader'Class;
      Ifnet : access Net.Interfaces.Ifnet_Type'Class;
      Addr  : Net.Sockets.Sockaddr_In);

   procedure Read_File
     (Self   : in out Reader'Class;
      Host   : Net.Ip_Addr;
      Name   : String;
      Port   : Net.Uint16 := Net.Headers.To_Network (69);
      Status : out Net.Error_Code);

   procedure Timeout
     (Self   : in out Reader'Class;
      Status : out Net.Error_Code);

   procedure Receive
     (Self   : in out Reader;
      Packet : in out Net.Buffers.Buffer_Type;
      EOF    : Boolean) is abstract;

   type Error_Code (Send_Status : Boolean := False) is record
      case Send_Status is
         when True =>
            ACK_Send_Status : Net.Error_Code;
         when False =>
            TFTP_Error : Net.Uint16;
      end case;
   end record;

   procedure Failure
     (Self  : in out Reader;
      Error : Error_Code) is null;

private

   type Reader is abstract new Net.Sockets.Udp.Socket with record
      Host         : Net.Sockets.Sockaddr_In;
      Expect_Block : Net.Uint16;
      Our_Port     : Net.Uint16;
   end record;

   overriding procedure Receive
     (Self   : in out Reader;
      From   : Net.Sockets.Sockaddr_In;
      Packet : in out Net.Buffers.Buffer_Type);

end TFTP;
