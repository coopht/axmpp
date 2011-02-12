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
--  <Unit> XMPP.Stream_Handlers
--  <ImplementationNotes>
--
------------------------------------------------------------------------------
--  $Revision$ $Author$
--  $Date$
------------------------------------------------------------------------------
with League.Strings;

with XMPP.Binds;
with XMPP.IQ_Sessions;
with XMPP.IQS;
with XMPP.Messages;
with XMPP.Presences;
with XMPP.Rosters;
with XMPP.Services;
with XMPP.Streams;
with XMPP.Stream_Features;

package XMPP.Stream_Handlers is

   type XMPP_Stream_Handler is limited interface;

   type XMPP_Stream_Handler_Access is access all XMPP_Stream_Handler'Class;

   not overriding procedure Connected
     (Self   : in out XMPP_Stream_Handler;
      Object : XMPP.Stream_Features.XMPP_Stream_Feature'Class)
      is null;
   --  Connected handler is called after succesfull authentification

   not overriding procedure Start_Stream
     (Self   : in out XMPP_Stream_Handler;
      Object : XMPP.Streams.XMPP_Stream'Class) is null;

   not overriding procedure Stream_Features
     (Self   : in out XMPP_Stream_Handler;
      Object : XMPP.Stream_Features.XMPP_Stream_Feature'Class)
      is null;

   not overriding procedure Bind_Resource_State
     (Self   : in out XMPP_Stream_Handler;
      JID    : League.Strings.Universal_String;
      Status : XMPP.Binds.Bind_State) is null;

   not overriding procedure Session_State
     (Self   : in out XMPP_Stream_Handler;
      Status : XMPP.IQ_Sessions.Session_State) is null;

   not overriding procedure Error
     (Self : in out XMPP_Stream_Handler) is null;

   not overriding procedure Message
     (Self : in out XMPP_Stream_Handler;
      Msg  : XMPP.Messages.XMPP_Message'Class) is null;

   not overriding procedure Presence
     (Self : in out XMPP_Stream_Handler;
      Data : XMPP.Presences.XMPP_Presence'Class) is null;

   not overriding procedure Roster
     (Self : in out XMPP_Stream_Handler;
      Data : XMPP.Rosters.XMPP_Roster'Class) is null;

   not overriding procedure IQ
     (Self : in out XMPP_Stream_Handler;
      IQ   : XMPP.IQS.XMPP_IQ'Class) is null;

   not overriding procedure Service_Information
     (Self : in out XMPP_Stream_Handler;
      Info : XMPP.Services.XMPP_Service'Class) is null;

   not overriding procedure End_Stream
     (Self : in out XMPP_Stream_Handler) is null;

end XMPP.Stream_Handlers;
