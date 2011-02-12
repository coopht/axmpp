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
--  <Unit> XMPP.Con_Cli_Handlers
--  <ImplementationNotes>
--
------------------------------------------------------------------------------
--  $Revision$ $Author$
--  $Date$
------------------------------------------------------------------------------
with League.Strings;

with XMPP.Binds;
with XMPP.IQ_Sessions;
with XMPP.Presences;
with XMPP.Stream_Handlers;
with XMPP.Streams;
with XMPP.Stream_Features;

limited with Con_Cli;

package Con_Cli_Handlers is

   type Con_Cli_Handler is limited new XMPP.Stream_Handlers.XMPP_Stream_Handler
     with private;

   type Con_Cli_Handler_Access is access all Con_Cli_Handler'Class;

   overriding procedure Connected
     (Self   : in out Con_Cli_Handler;
      Object : XMPP.Stream_Features.XMPP_Stream_Feature'Class);

   overriding procedure Start_Stream
     (Self   : in out Con_Cli_Handler;
      Object : XMPP.Streams.XMPP_Stream'Class);

   overriding procedure Stream_Features
     (Self   : in out Con_Cli_Handler;
      Object : XMPP.Stream_Features.XMPP_Stream_Feature'Class);

   overriding procedure Presence
     (Self : in out Con_Cli_Handler;
      Data : XMPP.Presences.XMPP_Presence'Class);

   procedure Set_Session_Object
     (Self   : in out Con_Cli_Handler;
      Object : not null access Con_Cli.Session'Class);

   procedure Set_Presence (Self : in out Con_Cli_Handler);

   overriding procedure Bind_Resource_State
     (Self   : in out Con_Cli_Handler;
      JID    : League.Strings.Universal_String;
      Status : XMPP.Binds.Bind_State);

   overriding procedure Session_State
     (Self   : in out Con_Cli_Handler;
      Status : XMPP.IQ_Sessions.Session_State);

private

   type Con_Cli_Handler is limited new XMPP.Stream_Handlers.XMPP_Stream_Handler
     with
   record
     Object : access Con_Cli.Session;
   end record;

end Con_Cli_Handlers;
