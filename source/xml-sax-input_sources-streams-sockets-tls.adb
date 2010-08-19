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
--  <Unit> XML.SAX.Input_Sources.Streams.Sockets.TLS
--  <ImplementationNotes>
--
------------------------------------------------------------------------------
--  $Revision$ $Author$
--  $Date$
------------------------------------------------------------------------------
with Ada.Text_IO;

with GNUTLS;

with League.Strings.Internals;
with Matreshka.Internals.Strings.Operations;
with Matreshka.Internals.Utf16;

with XMPP.Sessions;

package body XML.SAX.Input_Sources.Streams.Sockets.TLS is


   --------------------------
   --  Is_TLS_Established  --
   --------------------------
   function Is_TLS_Established (Self : TLS_Socket_Input_Source) return Boolean
   is
   begin
      return Self.TLS_State = TLS;
   end Is_TLS_Established;

   ----------
   -- Next --
   ----------

   overriding procedure Next
    (Self        : in out TLS_Socket_Input_Source;
     Buffer      : in out
       not null Matreshka.Internals.Strings.Shared_String_Access;
     End_Of_Data : out Boolean)
   is
      use type Matreshka.Internals.Utf16.Utf16_String_Index;

      Old_Unused : constant Matreshka.Internals.Utf16.Utf16_String_Index
        := Buffer.Unused;
      Old_Length : constant Natural := Buffer.Length;
      New_Data   : Matreshka.Internals.Strings.Shared_String_Access;

   begin
      Socket_Input_Source (Self).Next (Buffer, End_Of_Data);
      New_Data :=
        Matreshka.Internals.Strings.Operations.Slice
         (Buffer,
          Old_Unused,
          Buffer.Unused - Old_Unused,
          Buffer.Length - Old_Length);
      XMPP.Sessions.Put_Line (League.Strings.Internals.Wrap (New_Data));
   end Next;

   ------------
   --  Read  --
   ------------
   overriding procedure Read
    (Self        : in out TLS_Socket_Input_Source;
     Buffer      : out Ada.Streams.Stream_Element_Array;
     Last        : out Ada.Streams.Stream_Element_Offset;
     End_Of_Data : out Boolean) is

      X : GNAT.Sockets.Request_Type (GNAT.Sockets.N_Bytes_To_Read);

   begin
      --  Getting how much data available in Socket
      GNAT.Sockets.Control_Socket (Self.Socket, X);
      if X.Size > 0 then
         case Self.TLS_State is
            when Raw =>
               GNAT.Sockets.Receive_Socket (Self.Socket, Buffer, Last);

            when Handshake =>
               begin
                  GNUTLS.Handshake (Self.TLS_Session);
                  Self.TLS_State := TLS;
                  Ada.Text_IO.Put_Line ("Handshake complete");
                  Self.Object.On_Connect;

               exception
                  when others =>
                     Ada.Text_IO.Put_Line
                       (GNUTLS.IO_Direction'Image
                          (GNUTLS.Get_Direction (Self.TLS_Session)));
               end;

            when TLS =>
               declare
                  E : GNAT.Sockets.Vector_Element :=
                    (Base => Buffer (Buffer'First)'Unchecked_Access,
                     Length => Buffer'Length);

                  Vector : GNAT.Sockets.Vector_Type (0 .. 0);

               begin
                  Vector (0) := E;

                  GNUTLS.Record_Recv (Self.TLS_Session, Vector, Last);

                  Ada.Text_IO.Put_Line
                    ("Data of "
                     & Ada.Streams.Stream_Element_Offset'Image (Last)
                     & " received from GNUTLS.Record_Recv");

                  declare
                     Result : String (1 .. Integer (Last));

                  begin
                     for J in 1 .. Last loop
                        Result (Integer (J)) := Character'Val (Buffer (J - 1));
                     end loop;

                     Ada.Text_IO.Put_Line
                       ("Recieved from GNUTLS.Record_Recv : " & Result);
                  end;
               end;
         end case;

      else
         Last := Buffer'First - 1;
      end if;

      End_Of_Data := False;
   end Read;

   -----------------------
   --  Start_Handshake  --
   -----------------------

   procedure Start_Handshake (Self : in out TLS_Socket_Input_Source) is
   begin
      Self.TLS_State := Handshake;
      GNUTLS.Handshake (Self.TLS_Session);
      Self.TLS_State := TLS;

   exception
      when others =>
         Ada.Text_IO.Put_Line
           (GNUTLS.IO_Direction'Image
              (GNUTLS.Get_Direction (Self.TLS_Session)));
   end Start_Handshake;

   ---------------------------
   --  Set_TLS_Established  --
   ---------------------------
--     procedure Set_TLS_Established (Self  : in out TLS_Socket_Input_Source;
--                                    Value : Boolean) is
--     begin
--        Self.TLS_Established := Value;
--     end Set_TLS_Established;

   -----------------------
   --  Set_TLS_Session  --
   -----------------------
   procedure Set_TLS_Session (Self  : in out TLS_Socket_Input_Source;
                              S     : GNUTLS.Session) is
   begin
      Self.TLS_Session := S;
   end Set_TLS_Session;

end XML.SAX.Input_Sources.Streams.Sockets.TLS;
