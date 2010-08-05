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
with Ada.Wide_Wide_Text_IO;

with League.Strings;
with XMPP.Networks;

package body XMPP.Sessions is

   use type Ada.Streams.Stream_Element_Offset;
   use League.Strings;

   JID      : Universal_String := To_Universal_String ("uim-test");

   Host     : Universal_String := To_Universal_String ("zion");

   Password : Universal_String := To_Universal_String ("123");

   procedure Close (Self : in out XMPP_Session) is
   begin
      null;
   end Close;

   function Is_Opened (Self : XMPP_Session) return Boolean is
   begin
      return Self.Is_Opened;
   end Is_Opened;

   ------------
   --  Open  --
   ------------
   procedure Open (Self : in out XMPP_Session) is
   begin
      if not Self.Is_Opened then
         Ada.Wide_Wide_Text_IO.Put_Line ("Connecting");
         Self.Connect ("127.0.0.1", 5222);
         Ada.Wide_Wide_Text_IO.Put_Line ("Starting idle");
         Self.Idle;
      end if;
   end Open;

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
      Self.Send
        (XMPP.Networks.To_Stream_Element_Array
           (Ada.Characters.Conversions.To_String
              (Open_Stream.To_Wide_Wide_String)));
   end On_Connect;

   overriding
   procedure On_Disconnect (Self : not null access XMPP_Session) is
   begin
      null;
   end On_Disconnect;

   overriding
   procedure On_Recieve (Self   : not null access XMPP_Session;
                         Data   : Ada.Streams.Stream_Element_Array;
                         Offset : Ada.Streams.Stream_Element_Count) is

      Result : Wide_Wide_String (1 .. Integer (Offset));

   begin
      Ada.Wide_Wide_Text_IO.Put_Line (" >>> On_Recieve : ");
      for J in 1 .. Offset loop
         Result (Integer (J)) := Wide_Wide_Character'Val (Data (J));
      end loop;

      Ada.Wide_Wide_Text_IO.Put_Line (" >>> Result : "  & Result);
   end On_Recieve;

   ------------------
   --  Characters  --
   ------------------
   overriding procedure Characters
     (Self    : in out SAX_Parser;
      Text    : League.Strings.Universal_String;
      Success : in out Boolean)
   is
   begin
      raise Program_Error with "Not yet implemented";
   end Characters;

   -------------------
   --  End_Element  --
   -------------------
   overriding procedure End_Element
     (Self           : in out SAX_Parser;
      Namespace_URI  : League.Strings.Universal_String;
      Local_Name     : League.Strings.Universal_String;
      Qualified_Name : League.Strings.Universal_String;
      Success        : in out Boolean)
   is
   begin
      raise Program_Error with "Not yet implemented";
   end End_Element;

   --------------------
   --  Error_String  --
   --------------------
   overriding function Error_String (Self : SAX_Parser)
                 return League.Strings.Universal_String
   is
   begin
      return X : League.Strings.Universal_String;
   end Error_String;

   ---------------------
   --  Start_Element  --
   ---------------------
   overriding procedure Start_Element
     (Self           : in out SAX_Parser;
      Namespace_URI  : League.Strings.Universal_String;
      Local_Name     : League.Strings.Universal_String;
      Qualified_Name : League.Strings.Universal_String;
      Attributes     : XML.SAX.Attributes.SAX_Attributes;
      Success        : in out Boolean)
   is
   begin
      raise Program_Error with "Not yet implemented";
   end Start_Element;

   overriding procedure Warning
     (Self       : in out SAX_Parser;
      Occurrence : XML.SAX.Parse_Exceptions.SAX_Parse_Exception;
      Success    : in out Boolean)
   is
   begin
      raise Program_Error with "Not yet implemented";
   end Warning;

   procedure Put_Line (Item : League.Strings.Universal_String) is
   begin
      Ada.Wide_Wide_Text_IO.Put_Line (Item.To_Wide_Wide_String);
   end Put_Line;

end XMPP.Sessions;
