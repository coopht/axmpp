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
with XMPP.Streams;
with XMPP.Stream_Features;

package XMPP.Stream_Handlers is

   type XMPP_Stream_Handler is limited interface;

   type XMPP_Stream_Handler_Access is access all XMPP_Stream_Handler'Class;

   not overriding procedure Connected
     (Self   : in out XMPP_Stream_Handler;
      Object : XMPP.Stream_Features.XMPP_Stream_Feature_Access)
      is null;
   --  Connected handler is called after succesfull authentification

   not overriding procedure Start_Stream
     (Self   : in out XMPP_Stream_Handler;
      Object : not null XMPP.Streams.XMPP_Stream_Access) is null;

   not overriding procedure Stream_Features
     (Self   : in out XMPP_Stream_Handler;
      Object : not null XMPP.Stream_Features.XMPP_Stream_Feature_Access)
      is null;

   not overriding procedure Error
     (Self : in out XMPP_Stream_Handler) is null;

   not overriding procedure Message
     (Self : in out XMPP_Stream_Handler) is null;

   not overriding procedure Presence
     (Self : in out XMPP_Stream_Handler) is null;

   not overriding procedure IQ
     (Self : in out XMPP_Stream_Handler) is null;

   not overriding procedure End_Stream
     (Self : in out XMPP_Stream_Handler) is null;

end XMPP.Stream_Handlers;
