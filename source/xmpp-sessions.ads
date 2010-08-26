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

with GNUTLS;

with League.Strings;

with XMPP.Networks;
with XMPP.Stream_Handlers;
with XMPP.Raw_Handlers;

with XML.SAX.Attributes;
with XML.SAX.Content_Handlers;
with XML.SAX.DTD_Handlers;
with XML.SAX.Declaration_Handlers;
with XML.SAX.Entity_Resolvers;
with XML.SAX.Error_Handlers;
with XML.SAX.Input_Sources.Streams.Sockets.TLS;
with XML.SAX.Lexical_Handlers;
with XML.SAX.Locators;
with XML.SAX.Parse_Exceptions;
with XML.SAX.Simple_Readers;

with XMPP.Challenges;
with XMPP.Null_Objects;
with XMPP.Objects;

package XMPP.Sessions is

   Null_X : XMPP.Objects.XMPP_Object_Access
     := new XMPP.Null_Objects.XMPP_Null_Object;

   type XMPP_Session is limited new XMPP.Networks.Network
     and XML.SAX.Content_Handlers.SAX_Content_Handler
     and XML.SAX.Declaration_Handlers.SAX_Declaration_Handler
     and XML.SAX.DTD_Handlers.SAX_DTD_Handler
     and XML.SAX.Entity_Resolvers.SAX_Entity_Resolver
     and XML.SAX.Error_Handlers.SAX_Error_Handler
     and XML.SAX.Lexical_Handlers.SAX_Lexical_Handler with
   record
     Session_Opened : Boolean := False;
     Stream_Handler : XMPP.Stream_Handlers.XMPP_Stream_Handler_Access;

     Locator : XML.SAX.Locators.SAX_Locator;
     Tag     : League.Strings.Universal_String;

     Source  :
       aliased
         XML.SAX.Input_Sources.Streams.Sockets.TLS.TLS_Socket_Input_Source;
     Reader  : aliased XML.SAX.Simple_Readers.SAX_Simple_Reader;

     Current : XMPP.Objects.XMPP_Object_Access := Null_X;

     TLS_Session     : GNUTLS.Session;
     Credential      : GNUTLS.Certificate_Client_Credentials;
     Authenticated   : Boolean := False;

     JID             : League.Strings.Universal_String;
     Host            : League.Strings.Universal_String;
     Password        : League.Strings.Universal_String;
     Addr            : League.Strings.Universal_String;
   end record;

   type XMPP_Session_Access is access all XMPP_Session;

   --  XMPP Session API

   procedure Open (Self : not null access XMPP_Session);

   procedure Close (Self : in out XMPP_Session);

   function Is_Opened (Self : XMPP_Session) return Boolean;

   procedure Set_Stream_Handler
    (Self    : not null access XMPP_Session;
     Handler : not null access XMPP.Stream_Handlers.XMPP_Stream_Handler'Class);

   procedure Set_Raw_Handler
    (Self    : XMPP_Session;
     Handler : not null access XMPP.Raw_Handlers.XMPP_Raw_Handler)
   is null;

   --  End XMPP Session API

   --  Private API
   --  Private API should not be used by application
   overriding procedure Characters
     (Self    : in out XMPP_Session;
      Text    : League.Strings.Universal_String;
      Success : in out Boolean);

   overriding procedure End_Element
     (Self           : in out XMPP_Session;
      Namespace_URI  : League.Strings.Universal_String;
      Local_Name     : League.Strings.Universal_String;
      Qualified_Name : League.Strings.Universal_String;
      Success        : in out Boolean);

   overriding function Error_String (Self : XMPP_Session)
      return League.Strings.Universal_String;

   overriding procedure Start_Element
     (Self           : in out XMPP_Session;
      Namespace_URI  : League.Strings.Universal_String;
      Local_Name     : League.Strings.Universal_String;
      Qualified_Name : League.Strings.Universal_String;
      Attributes     : XML.SAX.Attributes.SAX_Attributes;
      Success        : in out Boolean);

   overriding procedure Error
    (Self       : in out XMPP_Session;
     Occurrence : XML.SAX.Parse_Exceptions.SAX_Parse_Exception;
     Success    : in out Boolean);

   procedure Put_Line (Item : League.Strings.Universal_String);

   --  Overriding functions from XMPP_Network
   overriding
   procedure On_Connect (Self : not null access XMPP_Session);

   overriding
   procedure On_Disconnect (Self : not null access XMPP_Session);

   overriding
   procedure Read_Data (Self   : not null access XMPP_Session);

   procedure Create_Object (Self          : in out XMPP_Session;
                            Namespace_URI : Wide_Wide_String;
                            Local_Name    : Wide_Wide_String);

   procedure Delete_Object (Self          : in out XMPP_Session;
                            Namespace_URI : Wide_Wide_String;
                            Local_Name    : Wide_Wide_String);

   procedure Send_Wide_Wide_String (Self : in out XMPP_Session;
                                    Str  : Wide_Wide_String);

   procedure Proceed_TLS_Auth (Self : not null access XMPP_Session);

   procedure Proceed_SASL_Auth
     (Self   : not null access XMPP_Session;
      Object : not null XMPP.Challenges.XMPP_Challenge_Access);

   procedure Open_Stream (Self : not null access XMPP_Session);

   procedure Set_JID (Self : in out XMPP_Session;
                      JID  : League.Strings.Universal_String);

   procedure Set_Host (Self : in out XMPP_Session;
                       Host : League.Strings.Universal_String);

   procedure Set_Password (Self     : in out XMPP_Session;
                           Password : League.Strings.Universal_String);

   procedure Set_Host_Addr (Self : in out XMPP_Session;
                            Addr : League.Strings.Universal_String);

   procedure Send_Object (Self   : not null access XMPP_Session;
                          Object : XMPP.Objects.XMPP_Object'Class);

end XMPP.Sessions;
