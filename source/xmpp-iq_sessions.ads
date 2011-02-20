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
--  <Unit> XMPP.IQ_Sessions
--  <ImplementationNotes>
--
------------------------------------------------------------------------------
--  $Revision$ $Author$
--  $Date$
------------------------------------------------------------------------------
with League.Strings;

with XML.SAX.Pretty_Writers;

with XMPP.IQS;
with XMPP.Objects;

package XMPP.IQ_Sessions is

   Session_Element : constant League.Strings.Universal_String
     := League.Strings.To_Universal_String ("session");

   Session_URI     : constant League.Strings.Universal_String
     := League.Strings.To_Universal_String
         ("urn:ietf:params:xml:ns:xmpp-session");

   type Session_State is (Established, Error);

   type XMPP_IQ_Session is new XMPP.IQS.XMPP_IQ with private;

   type XMPP_IQ_Session_Access is access all XMPP_IQ_Session'Class;

   overriding function Get_Kind (Self : XMPP_IQ_Session)
      return Objects.Object_Kind;

   overriding procedure Serialize
    (Self   : XMPP_IQ_Session;
     Writer : in out XML.SAX.Pretty_Writers.SAX_Pretty_Writer'Class);

   overriding
   procedure Set_Content (Self      : in out XMPP_IQ_Session;
                          Parameter : League.Strings.Universal_String;
                          Value     : League.Strings.Universal_String);

   function Create return not null XMPP_IQ_Session_Access;

private

   type XMPP_IQ_Session is new XMPP.IQS.XMPP_IQ with null record;

end XMPP.IQ_Sessions;
