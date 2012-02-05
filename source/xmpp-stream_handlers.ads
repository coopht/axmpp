------------------------------------------------------------------------------
--                                                                          --
--                              AXMPP Project                               --
--                                                                          --
--                           XMPP Library for Ada                           --
--                                                                          --
------------------------------------------------------------------------------
--                                                                          --
-- Copyright © 2011, Alexander Basov <coopht@gmail.com>                     --
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

with XMPP.IQS;
with XMPP.Messages;
with XMPP.Presences;
with XMPP.Rosters;
with XMPP.Services;
with XMPP.Streams;
with XMPP.Stream_Features;
with XMPP.Versions;

package XMPP.Stream_Handlers is

   type XMPP_Stream_Handler is limited interface;

   type XMPP_Stream_Handler_Access is access all XMPP_Stream_Handler'Class;

   not overriding procedure Connected
     (Self   : in out XMPP_Stream_Handler;
      Object : XMPP.Stream_Features.XMPP_Stream_Feature'Class) is null;
   --  Handler is called after succesfull authentification

   not overriding procedure Disconnected
     (Self   : in out XMPP_Stream_Handler) is null;
   --  Handler is called after axmpp disconnected from the network

   not overriding procedure IQ
     (Self : in out XMPP_Stream_Handler;
      IQ   : XMPP.IQS.XMPP_IQ'Class) is null;
   --  Handler is called when XMPP IQ arrived.
   --  Note, that you can parse IQ manually in your application,
   --  or you can reimplement appropriate handlers

   not overriding procedure Start_Stream
     (Self   : in out XMPP_Stream_Handler;
      Object : XMPP.Streams.XMPP_Stream'Class) is null;
   --  Handler is called after stream started

   not overriding procedure Stream_Features
     (Self   : in out XMPP_Stream_Handler;
      Object : XMPP.Stream_Features.XMPP_Stream_Feature'Class) is null;
   --  Handler is called when stream features arrived.

   not overriding procedure Bind_Resource_State
     (Self   : in out XMPP_Stream_Handler;
      JID    : League.Strings.Universal_String;
      Status : Bind_State) is null;
   --  Handler is called when resusts of resource binding arrived.

   not overriding procedure Session_State
     (Self   : in out XMPP_Stream_Handler;
      Status : Session_State) is null;
   --  Handler is called when information about session state arrived.

   not overriding procedure Error
     (Self : in out XMPP_Stream_Handler) is null;
  --  Handler is called when XMPP error occured.

   not overriding procedure Message
     (Self : in out XMPP_Stream_Handler;
      Msg  : XMPP.Messages.XMPP_Message'Class) is null;
   --  Handler is called when XMPP message arrived.

   not overriding procedure Presence
     (Self : in out XMPP_Stream_Handler;
      Data : XMPP.Presences.XMPP_Presence'Class) is null;
   --  Handler is called when XMPP presence arrived.

   not overriding procedure Roster
     (Self : in out XMPP_Stream_Handler;
      Data : XMPP.Rosters.XMPP_Roster'Class) is null;
   --  Handler is called when XMPP roster arrived.

   not overriding procedure Service_Information
     (Self : in out XMPP_Stream_Handler;
      Info : XMPP.Services.XMPP_Service'Class) is null;
   --  Handler is called, discovered information arrived from XMPP server

   not overriding procedure End_Stream
     (Self : in out XMPP_Stream_Handler) is null;
     --  Handler is called, when end_strem arrived.

   not overriding procedure Version
     (Self    : in out XMPP_Stream_Handler;
      Version : XMPP.Versions.XMPP_Version'Class) is null;
   --  Handler is called, discovered information arrived from XMPP server

   not overriding procedure Version_Request
     (Self    : in out XMPP_Stream_Handler;
      Version : in out XMPP.Versions.XMPP_Version'Class) is null;
   --  Handler is called when sombodies asks our clients version.
   --  Client should fill appropriate fields (Name, Version, OS)
   --  of Version object, or default values will be used.

end XMPP.Stream_Handlers;
