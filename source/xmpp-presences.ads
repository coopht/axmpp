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
--  <Unit> XMPP.Presences
--  <ImplementationNotes>
--
------------------------------------------------------------------------------
--  $Revision$ $Author$
--  $Date$
------------------------------------------------------------------------------
with League.Strings;

with XMPP.Objects;

package XMPP.Presences is

   type Show_Kind is (Away, Chat, DND, XA, Online);

   --  type Presence_Type is (Error,
   --                         Probe,
   --                         Subscribe,
   --                         Subscribed,
   --                         Unavailable,
   --                         Unsubscribe,
   --                         Unsubscribed);

   type Priority_Type is new Integer range -129 .. 127;

   type XMPP_Presence is new XMPP.Objects.XMPP_Object with private;

   type XMPP_Presence_Access is access all XMPP_Presence'Class;

   overriding function Get_Kind (Self : XMPP_Presence)
      return Objects.Object_Kind;

   overriding function Serialize (Self : in XMPP_Presence)
      return League.Strings.Universal_String;

   overriding
   procedure Set_Content (Self      : in out XMPP_Presence;
                          Parameter : League.Strings.Universal_String;
                          Value     : League.Strings.Universal_String);

   procedure Set_Show (Self : in out XMPP_Presence; Show : Show_Kind);

   function Get_Show (Self : XMPP_Presence) return Show_Kind;

   procedure Set_Status (Self   : in out XMPP_Presence;
                         Status : League.Strings.Universal_String);

   function Get_Status (Self : XMPP_Presence)
      return League.Strings.Universal_String;

   procedure Set_Priority (Self : in out XMPP_Presence; P : Priority_Type);

   function Get_Priority (Self : XMPP_Presence) return Priority_Type;

   function Get_To (Self : XMPP_Presence)
      return League.Strings.Universal_String;

   function Get_From (Self : XMPP_Presence)
      return League.Strings.Universal_String;

   procedure Set_To (Self  : in out XMPP_Presence;
                     Value : League.Strings.Universal_String);

   procedure Set_From (Self  : in out XMPP_Presence;
                       Value : League.Strings.Universal_String);

   function Create return XMPP_Presence_Access;

private

   type XMPP_Presence is new XMPP.Objects.XMPP_Object with
   record
      To       : League.Strings.Universal_String;
      From     : League.Strings.Universal_String;
      Show     : Show_Kind := Online;
      Status   : League.Strings.Universal_String;
      Priority : Priority_Type := -129;
      --  Type_Of_Presence : Presence_Type := Unavailable;
   end record;
end XMPP.Presences;

