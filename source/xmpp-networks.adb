------------------------------------------------------------------------------
--                                                                          --
--                                 AXMPP                                    --
--                                                                          --
------------------------------------------------------------------------------
--                                                                          --
-- Copyright © 2010 Alexander Basov <coopht@gmail.com>                      --
--                                                                          --
-- This is free software;  you can  redistribute it and/or modify it under  --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. UIM is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License distributed with UIM;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
------------------------------------------------------------------------------
--
--  <Unit> XMPP.Networks
--  <ImplementationNotes>
--
------------------------------------------------------------------------------
--  $Revision$ $Author$
--  $Date$
------------------------------------------------------------------------------
with Ada.Characters.Conversions;
with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Unchecked_Conversion;

use Ada.Exceptions;
use Ada.Text_IO;

package body XMPP.Networks is

   use type Ada.Streams.Stream_Element_Offset;

   -------------------
   --  Reader_Task  --
   -------------------
   task type Reader_Task (Object : not null access Network) is
      entry Start;
      entry Stop;
   end Reader_Task;

   -------------------
   --  Reader_Task  --
   -------------------
   --  FIXME: correct task termination using Stop entry
   task body Reader_Task is
   begin
      accept Start do
         Object.Time_To_Stop := False;
         Create_Selector (Object.Selector);
         Empty (Object.RSet);
         Empty (Object.WSet);
      end Start;

      loop
         exit when Object.Time_To_Stop;

         select
            accept Stop do
               Close_Selector (Object.Selector);
               Object.Time_To_Stop := True;
               return;
            end Stop;
         or
            delay 0.0;

            Object.Recieve;
         end select;
      end loop;

   exception
      when E : others =>
         Ada.Text_IO.Put_Line
           (Exception_Name (E) & ": " & Exception_Message (E));
   end Reader_Task;

   type Reader_Task_Access is access Reader_Task;

   RT : Reader_Task_Access;

   ------------
   --  Send  --
   ------------
   procedure Send (Self   : not null access Network'Class;
                   Data   : Ada.Streams.Stream_Element_Array)
   is
   begin
      Self.Channel.Write (Data);
   exception
      when E : others =>
         Ada.Text_IO.Put_Line
           (Exception_Name (E) & ": " & Exception_Message (E));
   end Send;

   ---------------
   --  Connect  --
   ---------------
   procedure Connect (Self : not null access Network'Class;
                      Host : in Wide_Wide_String;
                      Port : in Natural)
   is
   begin
      Create_Socket (Self.Sock);
      Self.Addr :=
        Sock_Addr_Type'(Addr   => Inet_Addr
                          (Ada.Characters.Conversions.To_String (Host)),
                        Port   => Port_Type (Port),
                        Family => Family_Inet);

      Set_Socket_Option (Self.Sock,
                         Socket_Level,
                         (Reuse_Address, True));

      Connect_Socket (Self.Sock, Self.Addr);
      Self.Channel := Stream (Self.Sock);

      delay 0.2;
      Self.On_Connect;

   exception
      when E : others =>
         Ada.Text_IO.Put_Line
           (Exception_Name (E) & ": " & Exception_Message (E));
   end Connect;

   ------------------
   --  Disconnect  --
   ------------------
   procedure Disconnect (Self : not null access Network'Class) is
   begin
      if RT /= null then
         Self.Time_To_Stop := True;
         Abort_Selector (Self.Selector);

         Close_Socket (Self.Sock);
         Self.On_Disconnect;
      end if;

   exception
      when E : others =>
         Ada.Text_IO.Put_Line
           (Exception_Name (E) & ": " & Exception_Message (E));
   end Disconnect;

   ------------
   --  Idle  --
   ------------
   procedure Idle (Self : in out Network) is
   begin
      RT := new Reader_Task (Self'Unchecked_Access);
      RT.Start;
   end Idle;

   ---------------
   --  Recieve  --
   ---------------
   procedure Recieve (Self : not null access Network'Class) is
   begin
      Set (Self.RSet, Self.Sock);
      Put_Line ("Waiting for data in select");

      --  multiplexed i/o, like select in C
      Check_Selector (Self.Selector, Self.RSet, Self.WSet, Self.Status);

      case Self.Status is
         when Completed =>
            if Is_Set (Self.WSet, Self.Sock) then
               Ada.Text_IO.Put_Line ("Wset is ready");
            end if;

            if Is_Set (Self.RSet, Self.Sock) then
               --  XXX : think about more correct threading model
               Self.Read_Data;
            end if;

         when Expired =>
            Put_Line ("Expired");
            return;

         when Aborted =>
            if Self.Time_To_Stop then
               Put_Line ("Aborted: OK!");
            end if;
            return;

      end case;

      --  Set (Self.RSet, Self.Sock);
      --  Empty (Self.RSet);

   exception
      when E : others =>
         Ada.Text_IO.Put_Line
           (Exception_Name (E) & ": " & Exception_Message (E));
   end Recieve;

   -------------------
   --  Get_Channel  --
   -------------------
   function Get_Channel (Self : not null access Network'Class)
     return Stream_Access
   is
   begin
      return Self.Channel;
   end Get_Channel;

   ------------------
   --  Get_Socket  --
   ------------------
   function Get_Socket (Self : not null access Network'Class)
      return Socket_Type
   is
   begin
      return Self.Sock;
   end Get_Socket;

   -----------------
   --  Read_Data  --
   -----------------
   procedure Read_Data (Self : not null access Network'Class) is
      X : GNAT.Sockets.Request_Type (GNAT.Sockets.N_Bytes_To_Read);

   begin
      --  Getting how much data available in Socket
      GNAT.Sockets.Control_Socket (Self.Get_Socket, X);

      delay (0.1);

      --  debug
      Ada.Text_IO.Put_Line ("Data_Size for reading :" & X.Size'Img);

      declare
         SEA :  Ada.Streams.Stream_Element_Array
           (1 .. Ada.Streams.Stream_Element_Offset (X.Size));
         Offset : Ada.Streams.Stream_Element_Count;

      begin
         Ada.Streams.Read (Self.Channel.all, SEA, Offset);
         Ada.Text_IO.Put_Line ("Offset : " & Offset'Img);
         Self.On_Recieve (SEA, Offset);
      end;

   end Read_Data;

   -------------------------------
   --  To_Stream_Element_Array  --
   -------------------------------
   function To_Stream_Element_Array (Value : String)
      return Ada.Streams.Stream_Element_Array
   is
      subtype Source is String (Value'Range);
      subtype Result is Ada.Streams.Stream_Element_Array
        (Ada.Streams.Stream_Element_Offset (Value'First)
           .. Ada.Streams.Stream_Element_Offset (Value'Last));

      function To_Array is
         new Ada.Unchecked_Conversion (Source, Result);
   begin
      return To_Array (Value);
   end To_Stream_Element_Array;

end XMPP.Networks;
