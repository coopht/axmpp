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
with XMPP.Logger;

package body XMPP.Networks is

   use XMPP.Logger;

   use type Ada.Streams.Stream_Element_Offset;

   function "+" (Item : String) return Wide_Wide_String
     renames Ada.Characters.Conversions.To_Wide_Wide_String;

   -------------------
   --  Reader_Task  --
   -------------------
   task type Reader_Task (Object : not null access Network) is
      entry Stop;
   end Reader_Task;

   -------------------
   --  Reader_Task  --
   -------------------
   --  FIXME: correct task termination using Stop entry
   task body Reader_Task is
      Time_To_Stop : Boolean := False;

   begin
      loop
         exit when Time_To_Stop;

         select
            accept Stop do
               Time_To_Stop := True;
            end Stop;
         or
            delay 0.0;

            exit when not Object.Recieve;
         end select;

      end loop;

      Object.Task_Stopped;

   exception
      when E : others =>
         Log (+Ada.Exceptions.Exception_Information (E));
   end Reader_Task;

   type Reader_Task_Access is access Reader_Task;

   RT : Reader_Task_Access;

   ---------------
   --  Connect  --
   ---------------
   procedure Connect
    (Self : not null access Network'Class;
     Host : String;
     Port : Natural) is
      No_Block : GNAT.Sockets.Request_Type (GNAT.Sockets.Non_Blocking_IO);

   begin
      Create_Socket (Self.Sock);
      Self.Addr :=
        Sock_Addr_Type'(Addr   =>
                          Addresses (GNAT.Sockets.Get_Host_By_Name (Host), 1),
                        Port   => Port_Type (Port),
                        Family => Family_Inet);

      Set_Socket_Option (Self.Sock,
                         Socket_Level,
                         (Reuse_Address, True));

      Connect_Socket (Self.Sock, Self.Addr);
      Self.Channel := Stream (Self.Sock);

      delay 0.2;

      No_Block.Enabled := True;

      --  Setting non-blocking IO
      GNAT.Sockets.Control_Socket (Self.Get_Socket, No_Block);

      Create_Selector (Self.Selector);
      Empty (Self.RSet);
      Empty (Self.WSet);

      Self.On_Connect;

   exception
      when E : others =>
         Log (+Ada.Exceptions.Exception_Information (E));
   end Connect;

   ------------------
   --  Disconnect  --
   ------------------
   procedure Disconnect (Self : not null access Network'Class) is
      pragma Unreferenced (Self);
   begin
      if RT /= null then
         RT.Stop;
      end if;
   exception
      when E : others =>
         Log  (+Ada.Exceptions.Exception_Information (E));
   end Disconnect;

   -------------------
   --  Get_Channel  --
   -------------------
   function Get_Channel (Self : not null access Network'Class)
      return Stream_Access is
   begin
      return Self.Channel;
   end Get_Channel;

   ------------------
   --  Get_Socket  --
   ------------------
   function Get_Socket (Self : not null access Network'Class)
      return Socket_Type is
   begin
      return Self.Sock;
   end Get_Socket;

   ------------
   --  Idle  --
   ------------
   procedure Idle (Self : in out Network) is
   begin
      RT := new Reader_Task (Self'Unchecked_Access);
   end Idle;

   -----------------
   --  Read_Data  --
   -----------------
   not overriding function Read_Data (Self : not null access Network)
     return Boolean is
     Buffer : Ada.Streams.Stream_Element_Array (1 .. 4096);
     Last   : Ada.Streams.Stream_Element_Count := 0;

   begin
      GNAT.Sockets.Receive_Socket (Self.Sock, Buffer, Last);

      Log ("XMPP.Networks.Read_Data: Offset = "
             & Ada.Streams.Stream_Element_Count'Wide_Wide_Image (Last));

      Self.On_Recieve (Buffer (1 .. Last));
      return True;
   exception
      when E : others =>
         Log  (+Ada.Exceptions.Exception_Information (E));
         Self.On_Recieve (Buffer (1 .. 1));
         return False;
   end Read_Data;

   -------------------------
   --  Read_Data_Wrapper  --
   -------------------------
   function Read_Data_Wrapper (Self : not null access Network'Class)
      return Boolean is
      --  for debug
      X  : GNAT.Sockets.Request_Type (GNAT.Sockets.N_Bytes_To_Read);
   begin
      delay (0.1);

      --  Getting how much data available in Socket
      GNAT.Sockets.Control_Socket (Self.Get_Socket, X);

      if X.Size = 0 then
         return False;
      else
         return Self.Read_Data;
      end if;
   end Read_Data_Wrapper;

   ---------------
   --  Recieve  --
   ---------------
   function Recieve (Self : not null access Network'Class) return Boolean is
   begin
      Set (Self.RSet, Self.Sock);
      Log ("Waiting for data in select");

      --  multiplexed i/o, like select in C
      Check_Selector (Self.Selector, Self.RSet, Self.WSet, Self.Status);

      case Self.Status is
         when Completed =>
            if Is_Set (Self.RSet, Self.Sock) then
               return Self.Read_Data_Wrapper;
            else
               return False;
            end if;

         when Expired =>
            return True;

         when Aborted =>
            return False;

      end case;

      --  Set (Self.RSet, Self.Sock);
      --  Empty (Self.RSet);

   exception
      when E : others =>
         Log  (+Ada.Exceptions.Exception_Information (E));
         return False;
   end Recieve;

   ------------
   --  Send  --
   ------------
   procedure Send
    (Self   : not null access Network'Class;
     Data   : Ada.Streams.Stream_Element_Array;
     Via_TLS : Boolean := False) is
   begin
      if not Via_TLS then
         Self.Channel.Write (Data);

      else
         Log ("Sendinging data via TLS");

         declare
            Tmp : Ada.Streams.Stream_Element_Array := Data;

            E   : constant GNAT.Sockets.Vector_Element :=
              (Base   => Tmp (Tmp'First)'Unchecked_Access,
               Length => Tmp'Length);

            P : GNAT.Sockets.Vector_Type (0 .. 0);
            L : Ada.Streams.Stream_Element_Count := 1;

         begin
            P (0) := E;
            GNUTLS.Record_Send (Self.TLS, P, L);
         end;
      end if;

   exception
      when E : others =>
         Log  (+Ada.Exceptions.Exception_Information (E));
   end Send;

   -----------------------
   --  Set_TLS_Session  --
   -----------------------
   procedure Set_TLS_Session
     (Self : not null access Network'Class;
      S    : GNUTLS.Session) is
   begin
      Self.TLS := S;
   end Set_TLS_Session;

   --------------------
   --  Task_Stopped  --
   --------------------
   procedure Task_Stopped (Self : not null access Network'Class) is
   begin
      Close_Selector (Self.Selector);
      Close_Socket (Self.Sock);
      Self.On_Disconnect;
   end Task_Stopped;

end XMPP.Networks;
