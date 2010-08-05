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

with XMPP.Networks;
with XMPP.Stream_Handlers;
with XMPP.Raw_Handlers;

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
