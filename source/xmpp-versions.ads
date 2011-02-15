------------------------------------------------------------------------------
--                                                                          --
--                                 AXMPP                                    --
--                                                                          --
------------------------------------------------------------------------------
--                                                                          --
-- Copyright © 2011 Alexander Basov <coopht@gmail.com>                      --
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
--  <Unit> XMPP.Versions
--  <ImplementationNotes> Unit implements XEP-0092 extension:
--  XMPP protocol extension for retrieving information about the software
--  application associated with an XMPP entity. The protocol enables one
--  entity to explicitly query another entity, where the response can include
--  the name of the software application, the version of the software
--  application, and the operating system on which the application is running.
--
--  http://xmpp.org/extensions/xep-0092.html
--
------------------------------------------------------------------------------
--  $Revision$ $Author$
--  $Date$
------------------------------------------------------------------------------
with League.Strings;

with XML.SAX.Pretty_Writers;

with XMPP.Objects;

package XMPP.Versions is

   Name_Element    : constant League.Strings.Universal_String
     := League.Strings.To_Universal_String ("name");

   OS_Element      : constant League.Strings.Universal_String
     := League.Strings.To_Universal_String ("os");

   Version_Element : constant League.Strings.Universal_String
     := League.Strings.To_Universal_String ("version");

   Version_URI     : constant League.Strings.Universal_String
     := League.Strings.To_Universal_String ("jabber:iq:version");

   type XMPP_Version is new XMPP.Objects.XMPP_Object with private;

   type XMPP_Version_Access is access all XMPP_Version'Class;

   function Create return XMPP_Version_Access;

   overriding function Get_Kind (Self : XMPP_Version)
      return Objects.Object_Kind;

   overriding procedure Serialize
    (Self   : XMPP_Version;
     Writer : in out XML.SAX.Pretty_Writers.SAX_Pretty_Writer'Class);

   overriding
   procedure Set_Content (Self      : in out XMPP_Version;
                          Parameter : League.Strings.Universal_String;
                          Value     : League.Strings.Universal_String);

   function Get_Name (Self : XMPP_Version)
      return League.Strings.Universal_String;

   function Get_OS (Self : XMPP_Version)
      return League.Strings.Universal_String;

   function Get_Version (Self : XMPP_Version)
      return League.Strings.Universal_String;

private

   type XMPP_Version is new XMPP.Objects.XMPP_Object with
   record
      Name    : League.Strings.Universal_String;
      OS      : League.Strings.Universal_String;
      Version : League.Strings.Universal_String;
   end record;

end XMPP.Versions;
