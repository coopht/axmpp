------------------------------------------------------------------------------
--                                                                          --
--                              AXMPP Project                               --
--                                                                          --
--                           XMPP Library for Ada                           --
--                                                                          --
------------------------------------------------------------------------------
--                                                                          --
-- Copyright © 2011-2018, Alexander Basov <coopht@gmail.com>                --
-- All rights reserved.                                                     --
--                                                                          --
-- Redistribution and use in source and binary forms, with or without       --
-- modification, are permitted provided that the following conditions       --
-- are met:                                                                 --
--                                                                          --
--  * Redistributions of source code must retain the above copyright        --
--    notice, this list of conditions and the following disclaimer.         --
--                                                                          --
--  * Redistributions in binary form must reproduce the above copyright     --
--    notice, this list of conditions and the following disclaimer in the   --
--    documentation and/or other materials provided with the distribution.  --
--                                                                          --
--  * Neither the name of the Alexander Basov, IE nor the names of its      --
--    contributors may be used to endorse or promote products derived from  --
--    this software without specific prior written permission.              --
--                                                                          --
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS      --
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT        --
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR    --
-- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT     --
-- HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,   --
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED --
-- TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR   --
-- PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF   --
-- LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING     --
-- NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS       --
-- SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.             --
--                                                                          --
------------------------------------------------------------------------------
--  $Revision$ $Date$
------------------------------------------------------------------------------

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
with XML.SAX.Lexical_Handlers;
with XML.SAX.Locators;
with XML.SAX.Parse_Exceptions;
with XML.SAX.Pretty_Writers;
with XML.SAX.Simple_Readers;
with XML.SAX.String_Output_Destinations;

with XMPP.IQS;
with XMPP.Challenges;
with XMPP.Objects;
with XMPP.Idle_Tasks;

package XMPP.Sessions is

   procedure Initialize renames XMPP.Networks.Initialize;
   --  This procedure should be called before use of Sessions

   type XMPP_Session is limited
     new XMPP.Networks.Notification
     and XML.SAX.Content_Handlers.SAX_Content_Handler
     and XML.SAX.Declaration_Handlers.SAX_Declaration_Handler
     and XML.SAX.DTD_Handlers.SAX_DTD_Handler
     and XML.SAX.Entity_Resolvers.SAX_Entity_Resolver
     and XML.SAX.Error_Handlers.SAX_Error_Handler
     and XML.SAX.Lexical_Handlers.SAX_Lexical_Handler with private;

   type XMPP_Session_Access is access all XMPP_Session;

   --  XMPP Session API
   --  XXX : API which should be rewritten in proper place

   procedure Bind_Resource (Self        : not null access XMPP_Session;
                            Resource_Id : League.Strings.Universal_String
                              := League.Strings.Empty_Universal_String);
   --  Binds resourse with specified Resource ID.

   procedure Close (Self : in out XMPP_Session);
   --  Closes XMPP session. Application should use this function to end
   --  data exchange with xmpp server

   procedure Discover_Information (Self : in out XMPP_Session;
                                   JID  : League.Strings.Universal_String);
   --  Sending request for getting disco#info

   procedure Discover_Items (Self : in out XMPP_Session;
                             JID  : League.Strings.Universal_String);
   --  Sending request for getting disco#items

   procedure Establish_IQ_Session (Self : not null access XMPP_Session);
   --  Establish real XMPP Session.

   procedure Join_Multi_User_Chat
    (Self      : in out XMPP_Session;
     Room      : League.Strings.Universal_String;
     Server    : League.Strings.Universal_String;
     Nick_Name : League.Strings.Universal_String);
   --  Enters to the multichat Room, on the Server with the specified Nick_Name

   procedure Open (Self : not null access XMPP_Session);
   --  Initiates XMPP session. Application should use this function to start
   --  data exchange with xmpp server

   procedure Request_Roster (Self : not null access XMPP_Session);
   --  Requests roster from server

   procedure Request_Version
    (Self        : not null access XMPP_Session;
     XMPP_Entity : League.Strings.Universal_String);
   --  Requests information about the software application
   --  associated with an XMPP entity

   procedure Set_JID (Self : in out XMPP_Session;
                      JID  : League.Strings.Universal_String);
   --  Sets jabber ID. JID should be set without hostname

   procedure Send_Object (Self   : not null access XMPP_Session;
                          Object : XMPP.Objects.XMPP_Object'Class);
   --  Sends XMPP Object

   procedure Set_Password (Self     : in out XMPP_Session;
                           Password : League.Strings.Universal_String);
   --  Sets passowrd for jabber account

   procedure Set_Raw_Handler
    (Self    : XMPP_Session;
     Handler : not null access XMPP.Raw_Handlers.XMPP_Raw_Handler)
   is null;
   --  If application whants recieve RAW XML data, it should set Raw_Handler

   procedure Set_Resource (Self        : not null access XMPP_Session;
                           Resource_Id : League.Strings.Universal_String);
   --  Function sets resource name which will be binded when client
   --  is connected to server.

   procedure Set_Stream_Handler
    (Self    : not null access XMPP_Session;
     Handler : not null access XMPP.Stream_Handlers.XMPP_Stream_Handler'Class);
   --  Application must set stream handler for the axmpp library.

   procedure Set_Host (Self : not null access XMPP_Session;
                       Host : League.Strings.Universal_String);
   --  Sets xmpp server host

   procedure Set_Port (Self : not null access XMPP_Session;
                       Port : Natural);
   --  Sets xmpp server port

private

   type XMPP_Session is limited new XMPP.Networks.Notification
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
     Text    : League.Strings.Universal_String;

     Network : aliased XMPP.Networks.Network (XMPP_Session'Unchecked_Access);
     Reader  : aliased XML.SAX.Simple_Readers.Simple_Reader;
     Writer  : XML.SAX.Pretty_Writers.XML_Pretty_Writer;
     Output  :
       aliased XML.SAX.String_Output_Destinations.String_Output_Destination;

     Stack   : XMPP.Objects.Object_Vectors.Vector;

     Idle_Task       : XMPP.Idle_Tasks.Reader_Task;
     Authenticated   : Boolean := False;

     JID             : League.Strings.Universal_String;
     Password        : League.Strings.Universal_String;

     Host            : League.Strings.Universal_String;
     Port            : Natural := 5222;

     In_IQ_Mode      : Boolean := False;
     IQ_Header       : XMPP.IQS.XMPP_IQ;
     Resource_Id     : League.Strings.Universal_String
       := League.Strings.To_Universal_String ("axmpp");
   end record;

   --  overriding from SAX.Reader
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

   overriding procedure Fatal_Error
    (Self       : in out XMPP_Session;
     Occurrence : XML.SAX.Parse_Exceptions.SAX_Parse_Exception);

   overriding procedure Start_Element
     (Self           : in out XMPP_Session;
      Namespace_URI  : League.Strings.Universal_String;
      Local_Name     : League.Strings.Universal_String;
      Qualified_Name : League.Strings.Universal_String;
      Attributes     : XML.SAX.Attributes.SAX_Attributes;
      Success        : in out Boolean);

   --  Overriding functions from XMPP_Network
   overriding procedure On_Connect (Self : not null access XMPP_Session);

   overriding procedure On_Disconnect (Self : not null access XMPP_Session);

   overriding function Read_Data (Self : not null access XMPP_Session)
      return Boolean;

   --  Session private functions

   function Is_Opened (Self : XMPP_Session) return Boolean;

   procedure Open_Stream (Self : not null access XMPP_Session);

   procedure Send_Wide_Wide_String (Self : in out XMPP_Session;
                                    Str  : Wide_Wide_String);

   procedure Process_IQ (Self : in out XMPP_Session;
                         IQ   : XMPP.Objects.XMPP_Object_Access);
   --  IQ empty in case of result of session.

   procedure Proceed_TLS_Auth (Self : not null access XMPP_Session);

   procedure Proceed_SASL_Auth
     (Self   : not null access XMPP_Session;
      Object : not null XMPP.Challenges.XMPP_Challenge_Access);

   procedure Disconnect (Self : in out XMPP_Session);

end XMPP.Sessions;
