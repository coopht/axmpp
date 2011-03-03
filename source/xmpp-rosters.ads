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
--  <Unit> XMPP.Rosters
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
with XMPP.Roster_Items;

package XMPP.Rosters is

   Query_Element    : constant League.Strings.Universal_String
     := League.Strings.To_Universal_String ("query");

   Roster_Element : constant League.Strings.Universal_String
     := League.Strings.To_Universal_String ("roster");

   Roster_URI : constant League.Strings.Universal_String
     := League.Strings.To_Universal_String ("jabber:iq:roster");

   type XMPP_Roster is new XMPP.IQS.XMPP_IQ with private;

   type XMPP_Roster_Access is access all XMPP_Roster'Class;

   --  Public API  --

   function Items_Count (Self : XMPP_Roster) return Natural;
   --  Returns number of roster_Items in roster.

   function Item_At (Self : XMPP_Roster; Pos : Natural)
      return not null XMPP.Roster_Items.XMPP_Roster_Item_Access;
   --  Returns roster item at given position.

   procedure Append_Item
     (Self : in out XMPP_Roster;
      Item : not null XMPP.Roster_Items.XMPP_Roster_Item_Access);
   --  Appends item to roster.

   function Create return not null XMPP_Roster_Access;
   --  Returns heap-allocated roster object.

   --  End of public API  --

   --  Private API, should not be used by application
   overriding function Get_Kind (Self : XMPP_Roster)
      return Objects.Object_Kind;

   overriding procedure Serialize
    (Self   : XMPP_Roster;
     Writer : in out XML.SAX.Pretty_Writers.SAX_Pretty_Writer'Class);

   overriding
   procedure Set_Content (Self      : in out XMPP_Roster;
                          Parameter : League.Strings.Universal_String;
                          Value     : League.Strings.Universal_String);
private

   type XMPP_Roster is new XMPP.IQS.XMPP_IQ with
   record
      Items : XMPP.Objects.Object_Vectors.Vector;
   end record;

end XMPP.Rosters;
