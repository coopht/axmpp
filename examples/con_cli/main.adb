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
with Con_Cli;
with Con_Cli_Handlers;
with XMPP.Sessions;

with League.Strings;

procedure Main is

   --  Creating XMPP Session object.

   S : constant not null Con_Cli.Session_Access := new Con_Cli.Session;

   --  Creating stream handler object.

   H : constant not null Con_Cli_Handlers.Con_Cli_Handler_Access
     := new Con_Cli_Handlers.Con_Cli_Handler;

begin
   --  Initialize GnuTLS and other stuff
   XMPP.Sessions.Initialize;

   --  Setting jabber id.

   S.Set_JID (League.Strings.To_Universal_String ("uim-test@zion"));

   --  Setting password for jabber id.

   S.Set_Password (League.Strings.To_Universal_String ("123"));

   --  for jabber.ru testing
   --  S.Set_Host (League.Strings.To_Universal_String ("jabber.ru"));
   --  S.Set_Password (League.Strings.To_Universal_String ("123456"));
   --  S.Set_Host_Addr (League.Strings.To_Universal_String ("77.88.57.177"));

   --  Setting stream handler.

   S.Set_Stream_Handler (H);

   --  Setting session object for session handler.
   H.Set_Session_Object (S);

   --  Setting resource name
   S.Set_Resource (League.Strings.To_Universal_String ("con_cli"));

   --  Openning connection to jabber server.
   --  Starting from this moment we will receive overriden events in
   --  Con_Cli_Handler, and for successfull work we should properly react
   --  to them.
   S.Open;
end Main;
