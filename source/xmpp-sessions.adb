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
--  <Unit> XMPP.Sessions
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
with XMPP.Null_Objects;
with XMPP.Objects;
with XMPP.Streams;
with XMPP.Stream_Features;

package body XMPP.Sessions is

   use League.Strings;

   use type Ada.Streams.Stream_Element_Offset;
   use type XMPP.Objects.Object_Kind;

   JID      : Universal_String := To_Universal_String ("uim-test");
   Host     : Universal_String := To_Universal_String ("zion");
   Password : Universal_String := To_Universal_String ("123");
   Addr     : Universal_String := To_Universal_String ("127.0.0.1");

   --  Connection to Jabber.ru
   --  Host     : Universal_String := To_Universal_String ("jabber.ru");
   --  Password : Universal_String := To_Universal_String ("123456");
   --  Addr     : Universal_String := To_Universal_String ("77.88.57.177");

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
         Self.Connect (Addr.To_Wide_Wide_String, 5222);
         Ada.Wide_Wide_Text_IO.Put_Line ("Starting idle");
         Self.Idle;
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
      --  After we connected, initialize parser.

      Self.Reader.Set_Content_Handler
        (XML.SAX.Readers.SAX_Content_Handler_Access (Self));
      XML.SAX.Simple_Readers.Put_Line := Put_Line'Access;

      Self.Source.Set_Socket (Self.Get_Socket);
      Self.Reader.Set_Input_Source (Self.Source'Access);

      --  Sending open stream stanza
      Self.Send
        (XMPP.Networks.To_Stream_Element_Array
           (Ada.Characters.Conversions.To_String
              (Open_Stream.To_Wide_Wide_String)));
   end On_Connect;

   ---------------------
   --  On_Disconnect  --
   ---------------------
   overriding
   procedure On_Disconnect (Self : not null access XMPP_Session) is
   begin
      null;
   end On_Disconnect;

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
      if Self.Current.Get_Kind /= XMPP.Objects.Null_Object then
         Self.Current.Set_Content (Self.Tag, Text);
      end if;
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
   --  Create_Object  --
   ---------------------
   procedure Create_Object (Self          : in out XMPP_Session;
                            Namespace_URI : Wide_Wide_String;
                            Local_Name    : Wide_Wide_String)
   is
   begin
      if Namespace_URI = "http://etherx.jabber.org/streams" then
         if Local_Name = "stream" then
            Self.Current := new XMPP.Streams.XMPP_Stream;
            return;

         elsif Local_Name = "features" then
            Self.Current := new XMPP.Stream_Features.XMPP_Stream_Feature;
            return;
         end if;
      end if;

      --  Creating Null_Object, if actual object cannot be created.

      Self.Current := Null_X;
      Ada.Wide_Wide_Text_IO.Put_Line
        ("Don't know what kind of element should be crated : "
           & "Namespace_URI : " & Namespace_URI
           & " Local_Name : " & Local_Name);
   end Create_Object;

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
      Ada.Wide_Wide_Text_IO.Put (">>> Start_Element_QN = "
                                   & Qualified_Name.To_Wide_Wide_String & " (");

      for J in 1 .. Attributes.Length loop
         Ada.Wide_Wide_Text_IO.Put
           (Attributes.Local_Name (J).To_Wide_Wide_String
              & "="
              & Attributes.Value (J).To_Wide_Wide_String
              & " ");
      end loop;

      Ada.Wide_Wide_Text_IO.Put_Line (")");

      Self.Tag := Local_Name;

      --  If Object not yet created, then create it
      if Self.Current.Get_Kind = XMPP.Objects.Null_Object then
         Self.Create_Object (Namespace_URI.To_Wide_Wide_String,
                             Local_Name.To_Wide_Wide_String);

         for J in 1 .. Attributes.Length loop
            Self.Current.Set_Content
              (Attributes.Local_Name (J), Attributes.Value (J));
         end loop;

      --  If object was created, then fill it
      else

         Self.Current.Set_Content (Local_Name, Qualified_Name);
      end if;

   end Start_Element;

   -------------
   --  Error  --
   -------------
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
