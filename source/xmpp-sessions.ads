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
with Ada.Streams;

with League.Strings;

with XMPP.Networks;
with XMPP.Stream_Handlers;
with XMPP.Raw_Handlers;

with XML.SAX.Attributes;
with XML.SAX.Content_Handlers;
with XML.SAX.Declaration_Handlers;
with XML.SAX.DTD_Handlers;
with XML.SAX.Entity_Resolvers;
with XML.SAX.Error_Handlers;
with XML.SAX.Input_Sources;
with XML.SAX.Lexical_Handlers;
with XML.SAX.Locators;
with XML.SAX.Parse_Exceptions;

package XMPP.Sessions is

   type XMPP_Session is limited new XMPP.Networks.Network with private;

   procedure Open (Self : in out XMPP_Session);

   procedure Close (Self : in out XMPP_Session);

   function Is_Opened (Self : XMPP_Session) return Boolean;

   procedure Set_Stream_Handler
    (Self    : XMPP_Session;
     Handler : not null access XMPP.Stream_Handlers.XMPP_Stream_Handler'Class)
       is null;

   procedure Set_Raw_Handler
    (Self    : XMPP_Session;
     Handler : not null access XMPP.Raw_Handlers.XMPP_Raw_Handler'Class)
       is null;

private

   type SAX_Parser is limited
     new XML.SAX.Content_Handlers.SAX_Content_Handler
       and XML.SAX.Declaration_Handlers.SAX_Declaration_Handler
       and XML.SAX.DTD_Handlers.SAX_DTD_Handler
       and XML.SAX.Entity_Resolvers.SAX_Entity_Resolver
       and XML.SAX.Error_Handlers.SAX_Error_Handler
       and XML.SAX.Lexical_Handlers.SAX_Lexical_Handler with
   record
     Locator : XML.SAX.Locators.SAX_Locator;
   end record;

   overriding procedure Characters
     (Self    : in out SAX_Parser;
      Text    : League.Strings.Universal_String;
      Success : in out Boolean);

   --  overriding procedure Comment
   --   (Self    : in out SAX_Parser;
   --    Text    : League.Strings.Universal_String;
   --    Success : in out Boolean);

   overriding procedure End_Element
     (Self           : in out SAX_Parser;
      Namespace_URI  : League.Strings.Universal_String;
      Local_Name     : League.Strings.Universal_String;
      Qualified_Name : League.Strings.Universal_String;
      Success        : in out Boolean);

   --  overriding procedure Error
   --   (Self       : in out SAX_Parser;
   --    Occurrence : XML.SAX.Parse_Exceptions.SAX_Parse_Exception;
   --    Success    : in out Boolean);

   overriding function Error_String (Self : SAX_Parser)
      return League.Strings.Universal_String;

   --  overriding procedure External_Entity_Declaration
   --   (Self      : in out SAX_Parser;
   --    Name      : League.Strings.Universal_String;
   --    Public_Id : League.Strings.Universal_String;
   --    System_Id : League.Strings.Universal_String;
   --    Success   : in out Boolean);

   --  overriding procedure Fatal_Error
   --   (Self       : in out SAX_Parser;
   --    Occurrence : XML.SAX.Parse_Exceptions.SAX_Parse_Exception;
   --    Success    : in out Boolean);

   --  overriding procedure Ignorable_Whitespace
   --   (Self    : in out SAX_Parser;
   --    Text    : League.Strings.Universal_String;
   --    Success : in out Boolean);

   --  overriding procedure Internal_Entity_Declaration
   --   (Self    : in out SAX_Parser;
   --    Name    : League.Strings.Universal_String;
   --    Value   : League.Strings.Universal_String;
   --    Success : in out Boolean);

   --  overriding procedure Processing_Instruction
   --   (Self    : in out SAX_Parser;
   --    Target  : League.Strings.Universal_String;
   --    Data    : League.Strings.Universal_String;
   --    Success : in out Boolean);

   --  overriding procedure Resolve_Entity
   --   (Self      : in out SAX_Parser;
   --    Public_Id : League.Strings.Universal_String;
   --    System_Id : League.Strings.Universal_String;
   --    Source    : out XML.SAX.Input_Sources.SAX_Input_Source_Access;
   --    Success   : in out Boolean);

   --  overriding procedure Set_Document_Locator
   --   (Self    : in out SAX_Parser;
   --    Locator : XML.SAX.Locators.SAX_Locator);

   overriding procedure Start_Element
     (Self           : in out SAX_Parser;
      Namespace_URI  : League.Strings.Universal_String;
      Local_Name     : League.Strings.Universal_String;
      Qualified_Name : League.Strings.Universal_String;
      Attributes     : XML.SAX.Attributes.SAX_Attributes;
      Success        : in out Boolean);

   --  overriding procedure Unparsed_Entity_Declaration
   --   (Self          : in out SAX_Parser;
   --    Name          : League.Strings.Universal_String;
   --    Public_Id     : League.Strings.Universal_String;
   --    System_Id     : League.Strings.Universal_String;
   --    Notation_Name : League.Strings.Universal_String;
   --    Success       : in out Boolean);

   overriding procedure Warning
     (Self       : in out SAX_Parser;
      Occurrence : XML.SAX.Parse_Exceptions.SAX_Parse_Exception;
      Success    : in out Boolean);

   procedure Put_Line (Item : League.Strings.Universal_String);

   type XMPP_Session is limited new XMPP.Networks.Network with record
      Is_Opened : Boolean := False;
   end record;

   overriding
   procedure On_Connect (Self : not null access XMPP_Session);

   overriding
   procedure On_Disconnect (Self : not null access XMPP_Session);

   overriding
   procedure On_Recieve (Self   : not null access XMPP_Session;
                         Data   : Ada.Streams.Stream_Element_Array;
                         Offset : Ada.Streams.Stream_Element_Count);

end XMPP.Sessions;
