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
--  <Unit> Main
--  <ImplementationNotes>
--
------------------------------------------------------------------------------
--  $Revision$ $Author$
--  $Date$
------------------------------------------------------------------------------
with Con_Cli;
with Con_Cli_Handlers;

with League.Strings;

procedure Main is
   S : not null Con_Cli.Session_Access := new Con_Cli.Session;
   H : not null Con_Cli_Handlers.Con_Cli_Handler_Access
     := new Con_Cli_Handlers.Con_Cli_Handler;

begin
   S.Set_JID (League.Strings.To_Universal_String ("uim-test"));

   --  For local testing
   S.Set_Host (League.Strings.To_Universal_String ("zion"));
   S.Set_Password (League.Strings.To_Universal_String ("123"));
   S.Set_Host_Addr (League.Strings.To_Universal_String ("127.0.0.1"));

   --  for jabber.ru testing
   --  S.Set_Host (League.Strings.To_Universal_String ("jabber.ru"));
   --  S.Set_Password (League.Strings.To_Universal_String ("123456"));
   --  S.Set_Host_Addr (League.Strings.To_Universal_String ("77.88.57.177"));

   S.Set_Stream_Handler (H);
   H.Set_Session_Object (S);
   S.Open;
end Main;
