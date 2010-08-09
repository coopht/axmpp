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
with Ada.Text_IO;
with Ada.Wide_Wide_Text_IO;

with League.Strings;
with XML.SAX.Readers;

with XMPP.Networks;
with XMPP.Objects;
with XMPP.Null_Objects;

package body XMPP.Sessions is

   use type Ada.Streams.Stream_Element_Offset;
   use League.Strings;

   JID      : Universal_String := To_Universal_String ("uim-test");
   Host     : Universal_String := To_Universal_String ("zion");
   Password : Universal_String := To_Universal_String ("123");
   --  Host     : Universal_String := To_Universal_String ("jabber.ru");
   --  Password : Universal_String := To_Universal_String ("123456");

   Null_X   : XMPP.Null_Objects.XMPP_Null_Object;

   X : XMPP.Objects.XMPP_Object'Class := Null_X;

   -------------
   --  Close  --
   -------------
   procedure Close (Self : in out XMPP_Session) is
   begin
      null;
   end Close;

   -----------------
   --  Is_Opened  --
   -----------------
   function Is_Opened (Self : XMPP_Session) return Boolean is
   begin
      return Self.Session_Opened;
   end Is_Opened;

   ------------
   --  Open  --
   ------------
   procedure Open (Self : not null access XMPP_Session) is
   begin
      if not Self.Is_Opened then
         Ada.Wide_Wide_Text_IO.Put_Line ("Connecting");
         Self.Connect ("127.0.0.1", 5222);
         -- Self.Connect ("77.88.57.177", 5222);
         Ada.Wide_Wide_Text_IO.Put_Line ("Starting idle");
         Self.Idle;

         --  After we connected, initialize parser.
         XML.SAX.Simple_Readers.Put_Line := Put_Line'Access;
      end if;
   end Open;

   ------------------
   --  On_Connect  --
   ------------------
   overriding
   procedure On_Connect (Self : not null access XMPP_Session) is
      Open_Stream : Universal_String
        := "<stream:stream "
             & "xmlns:stream='http://etherx.jabber.org/streams' "
             & "version='1.0' "
             & "xmlns='jabber:client' "
             & "to='"
             & Host
             & "' >";

   begin
      --  Sending open stream stanza
      Self.Send
        (XMPP.Networks.To_Stream_Element_Array
           (Ada.Characters.Conversions.To_String
              (Open_Stream.To_Wide_Wide_String)));

      Self.Reader.Set_Content_Handler
        (XML.SAX.Readers.SAX_Content_Handler_Access (Self));
      Self.Source.Set_Socket (Self.Get_Socket);
      Self.Reader.Set_Input_Source (Self.Source'Access);
   end On_Connect;

   ---------------------
   --  On_Disconnect  --
   ---------------------
   overriding
   procedure On_Disconnect (Self : not null access XMPP_Session) is
   begin
      null;
   end On_Disconnect;

   ------------------
   --  On_Recieve  --
   ------------------
   overriding
   procedure On_Recieve (Self   : not null access XMPP_Session;
                         Data   : Ada.Streams.Stream_Element_Array) is

      Result : Wide_Wide_String (Integer (Data'First) .. Integer (Data'Last));

   begin
      Ada.Text_IO.Put_Line (" Data'First = " & Data'First'Img);
      Ada.Text_IO.Put_Line (" Data'Last = " & Data'Last'Img);

      Ada.Wide_Wide_Text_IO.Put (" >>> On_Recieve : ");
      for J in Data'Range  loop
         Result (Integer (J)) := Wide_Wide_Character'Val (Data (J));
      end loop;

      --  if not Self.Is_Opened then
      --     declare
      --        Tmp : Wide_Wide_String :=  Result & "</stream:stream>";

      --     begin
      --        Ada.Wide_Wide_Text_IO.Put_Line (Tmp);


      --     end;

      --     Self.Session_Opened := True;
      --  else
      Ada.Wide_Wide_Text_IO.Put_Line (Result);
      --     Self.Source.Set_String (To_Universal_String (Result));
      --  end if;

      --  Self.Reader.Parse (Self.Source'Access);
   end On_Recieve;

   --------------------------
   --  Set_Stream_Handler  --
   --------------------------
   procedure Set_Stream_Handler
    (Self    : in out XMPP_Session;
     Handler : XMPP.Stream_Handlers.XMPP_Stream_Handler_Access)
   is
   begin
      Self.Stream_Handler := Handler;
   end Set_Stream_Handler;

   --  XML SAX Parser implementation  --

   ------------------
   --  Characters  --
   ------------------
   overriding procedure Characters
     (Self    : in out XMPP_Session;
      Text    : League.Strings.Universal_String;
      Success : in out Boolean)
   is
   begin
      Put_Line ("*** Text = " & Text);
   end Characters;

   -------------------
   --  End_Element  --
   -------------------
   overriding procedure End_Element
     (Self           : in out XMPP_Session;
      Namespace_URI  : League.Strings.Universal_String;
      Local_Name     : League.Strings.Universal_String;
      Qualified_Name : League.Strings.Universal_String;
      Success        : in out Boolean)
   is
   begin
      Put_Line ("<<< End_Element_QN = " & Qualified_Name);
      Success := True;
   end End_Element;

   --------------------
   --  Error_String  --
   --------------------
   overriding function Error_String (Self : XMPP_Session)
                 return League.Strings.Universal_String
   is
   begin
      return X : League.Strings.Universal_String;
   end Error_String;

   ---------------------
   --  Start_Element  --
   ---------------------
   overriding procedure Start_Element
     (Self           : in out XMPP_Session;
      Namespace_URI  : League.Strings.Universal_String;
      Local_Name     : League.Strings.Universal_String;
      Qualified_Name : League.Strings.Universal_String;
      Attributes     : XML.SAX.Attributes.SAX_Attributes;
      Success        : in out Boolean)
   is
   begin
      Put_Line (">>> Start_Element_QN = " & Qualified_Name);
      Success := True;
      --  if (Qualified_Name = "stream:stream") then
      --     null;
      --  end if;
   end Start_Element;

   overriding procedure Error
    (Self       : in out XMPP_Session;
     Occurrence : XML.SAX.Parse_Exceptions.SAX_Parse_Exception;
     Success    : in out Boolean)
   is
   begin
      Put_Line
       ("EEE (Error) " & Occurrence.Message & "'");
   end Error;

   ----------------
   --  Put_Line  --
   ----------------
   procedure Put_Line (Item : League.Strings.Universal_String) is
   begin
      Ada.Wide_Wide_Text_IO.Put_Line (Item.To_Wide_Wide_String);
   end Put_Line;

   -----------------
   --  Read_Data  --
   -----------------
   overriding
   procedure Read_Data (Self   : not null access XMPP_Session)
   is
   begin
      Self.Reader.Parse;
   end Read_Data;

end XMPP.Sessions;
