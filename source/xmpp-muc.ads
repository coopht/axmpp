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
--  <Unit> XMPP.MUC
--  <ImplementationNotes> Implements Multi-User Chat (XEP-0045)
--  http://xmpp.org/extensions/xep-0045.html
--
------------------------------------------------------------------------------
--  $Revision$ $Author$
--  $Date$
------------------------------------------------------------------------------
with League.Strings;

with XML.SAX.Pretty_Writers;

with XMPP.Objects;

package XMPP.MUC is

   MUC_Element : constant League.Strings.Universal_String
     := League.Strings.To_Universal_String ("muc");

   MUC_URI : constant League.Strings.Universal_String
     := League.Strings.To_Universal_String
         ("http://jabber.org/protocol/muc");

   type MUC_Affilation is (Admin, Member, None, Outcast, Owner);
   type MUC_Role is (Moderator, None, Participant, Visitor);

   type MUC_Item is record
      Affilation : MUC_Affilation := None;
      Role       : MUC_Role := None;
   end record;

   type XMPP_MUC is new XMPP.Objects.XMPP_Object with private;

   type XMPP_MUC_Access is access all XMPP_MUC'Class;

   overriding function Get_Kind (Self : XMPP_MUC) return Objects.Object_Kind;

   overriding procedure Serialize
    (Self   : XMPP_MUC;
     Writer : in out XML.SAX.Pretty_Writers.SAX_Pretty_Writer'Class);

   overriding procedure Set_Content
     (Self      : in out XMPP_MUC;
      Parameter : League.Strings.Universal_String;
      Value     : League.Strings.Universal_String);

   function Create return XMPP_MUC_Access;

   procedure Set_Item (Self : in out XMPP_MUC; Item : MUC_Item);

private

   type XMPP_MUC is new XMPP.Objects.XMPP_Object with
   record
      Item : MUC_Item;
   end record;
end XMPP.MUC;
