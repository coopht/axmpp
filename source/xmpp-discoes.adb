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
--  <Unit> XMPP.Discoes
--  <ImplementationNotes>
--
------------------------------------------------------------------------------
--  $Revision$ $Author$
--  $Date$
------------------------------------------------------------------------------
with Ada.Characters.Conversions;
with Ada.Wide_Wide_Text_IO;

with League.Strings;

with XMPP.Objects;

package body XMPP.Discoes is

   use League.Strings;

   ----------------
   --  Get_Kind  --
   ----------------
   overriding function Get_Kind (Self : XMPP_Disco) return Objects.Object_Kind
   is
   begin
      return XMPP.Objects.Disco;
   end Get_Kind;

   -----------------
   --  Serialize  --
   -----------------
   overriding function Serialize (Self : in XMPP_Disco)
      return League.Strings.Universal_String
   is
   begin
      case Self.Type_Of_Disco is
         when XMPP.Discoes_Features.Protocol_Disco_Info =>
            return
              To_Universal_String
               ("<query xmlns='http://jabber.org/protocol/disco#info'/>");


         when XMPP.Discoes_Features.Protocol_Disco_Items =>
            return
              To_Universal_String
               ("<query xmlns='http://jabber.org/protocol/disco#items'/>");

         when others =>
            raise Program_Error
              with "Unknow service discovery information type";

      end case;
   end Serialize;

   -------------------
   --  Set_Content  --
   -------------------
   overriding
   procedure Set_Content (Self      : in out XMPP_Disco;
                          Parameter : League.Strings.Universal_String;
                          Value     : League.Strings.Universal_String) is
   begin
      if Parameter = To_Universal_String ("disco#info") then
         Self.Type_Of_Disco := XMPP.Discoes_Features.Protocol_Disco_Info;

      elsif Parameter = To_Universal_String ("disco#items") then
         Self.Type_Of_Disco := XMPP.Discoes_Features.Protocol_Disco_Items;

      --  Added ignoring of tag itself
      elsif Parameter = To_Universal_String ("query") then
         null;

      else
         Ada.Wide_Wide_Text_IO.Put_Line ("XMPP_Disco: Unknown parameter : "
                                           & Parameter.To_Wide_Wide_String);
      end if;
   end Set_Content;

   --------------
   --  Create  --
   --------------
   function Create return not null XMPP_Disco_Access is
   begin
      return new XMPP_Disco;
   end Create;

   ----------------
   --  Get_Type  --
   ----------------
   function Get_Type (Self : XMPP_Disco) return XMPP.Discoes_Features.Feature
   is
   begin
      return Self.Type_Of_Disco;
   end Get_Type;

   ----------------
   --  Set_Type  --
   ----------------
   procedure Set_Type (Self : in out XMPP_Disco;
                       Val  : XMPP.Discoes_Features.Feature) is
   begin
      Self.Type_Of_Disco := Val;
   end Set_Type;

   ----------------------
   --  Get_Identities  --
   ----------------------
   function Get_Identities (Self : XMPP_Disco)
      return XMPP.Discoes_Identities.Identities_Vector is
   begin
      return Self.Identities;
   end Get_Identities;

   --------------------
   --  Get_Features  --
   --------------------
   function Get_Features (Self : XMPP_Disco)
      return XMPP.Discoes_Features.Features_Vector is
   begin
      return Self.Features;
   end Get_Features;

   --------------------
   --  Add_Identity  --
   --------------------
   procedure Add_Identity (Self : in out XMPP_Disco;
                           Val  : XMPP.Discoes_Identities.Identity) is
   begin
      Self.Identities.Append (Val);
   end Add_Identity;

   -------------------
   --  Add_Feature  --
   -------------------
   procedure Add_Feature (Self : in out XMPP_Disco;
                          Val  : XMPP.Discoes_Features.Feature) is
   begin
      Self.Features.Append (Val);
   end Add_Feature;

   -------------------
   --  Add_Feature  --
   -------------------
   procedure Add_Feature (Self : in out XMPP_Disco;
                          Val  : League.Strings.Universal_String) is
   begin
      if Val = To_Universal_String ("http://jabber.org/protocol/commands") then
         Self.Add_Feature (XMPP.Discoes_Features.Protocol_Commands);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/disco#info") then
         Self.Add_Feature (XMPP.Discoes_Features.Protocol_Disco_Info);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/disco#items") then
         Self.Add_Feature (XMPP.Discoes_Features.Protocol_Disco_Items);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub") then
         Self.Add_Feature
           (XMPP.Discoes_Features.Protocol_Pubsub);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#access-authorize") then
         Self.Add_Feature
           (XMPP.Discoes_Features.Protocol_Pubsub_Access_Authorize);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#access-presence") then
         Self.Add_Feature
           (XMPP.Discoes_Features.Protocol_Pubsub_Access_Presence);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#access-whitelist") then
         Self.Add_Feature
           (XMPP.Discoes_Features.Protocol_Pubsub_Access_Whitelist);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#auto-create") then
         Self.Add_Feature (XMPP.Discoes_Features.Protocol_Pubsub_Auto_Create);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#auto-subscribe") then
         Self.Add_Feature
           (XMPP.Discoes_Features.Protocol_Pubsub_Auto_Subscribe);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#collections") then
         Self.Add_Feature (XMPP.Discoes_Features.Protocol_Pubsub_Collections);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#access-open") then
         Self.Add_Feature (XMPP.Discoes_Features.Protocol_Pubsub_Access_Open);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#config-node") then
         Self.Add_Feature (XMPP.Discoes_Features.Protocol_Pubsub_Access_Open);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#create-and-configure") then
         Self.Add_Feature
           (XMPP.Discoes_Features.Protocol_Pubsub_Create_And_Configure);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#create-nodes") then
         Self.Add_Feature (XMPP.Discoes_Features.Protocol_Pubsub_Create_Nodes);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#delete-items") then
         Self.Add_Feature (XMPP.Discoes_Features.Protocol_Pubsub_Delete_Items);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#delete-nodes") then
         Self.Add_Feature (XMPP.Discoes_Features.Protocol_Pubsub_Delete_Nodes);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#filtered-notifications") then
         Self.Add_Feature
           (XMPP.Discoes_Features.Protocol_Pubsub_Filtered_Notifications);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#get-pending") then
         Self.Add_Feature (XMPP.Discoes_Features.Protocol_Pubsub_Get_Pending);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#instant-nodes") then
         Self.Add_Feature
           (XMPP.Discoes_Features.Protocol_Pubsub_Instant_Nodes);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#item-ids") then
         Self.Add_Feature (XMPP.Discoes_Features.Protocol_Pubsub_Item_Ids);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#last-published") then
         Self.Add_Feature
           (XMPP.Discoes_Features.Protocol_Pubsub_Last_Published);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#manage-subscriptions") then
         Self.Add_Feature
           (XMPP.Discoes_Features.Protocol_Pubsub_Manage_Subscription);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#member-affiliation") then
         Self.Add_Feature
           (XMPP.Discoes_Features.Protocol_Pubsub_Member_Affiliation);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#modify-affiliations") then
         Self.Add_Feature
           (XMPP.Discoes_Features.Protocol_Pubsub_Modify_Affiliations);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#multi-subscribe") then
         Self.Add_Feature
           (XMPP.Discoes_Features.Protocol_Pubsub_Multi_Subscribe);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#outcast-affiliation") then
         Self.Add_Feature
           (XMPP.Discoes_Features.Protocol_Pubsub_Outcast_Affiliation);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#owner") then
         Self.Add_Feature (XMPP.Discoes_Features.Protocol_Pubsub_Owner);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#persistent-items") then
         Self.Add_Feature
           (XMPP.Discoes_Features.Protocol_Pubsub_Persistent_Items);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#presence-notifications") then
         Self.Add_Feature
           (XMPP.Discoes_Features.Protocol_Pubsub_Presence_Notifications);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#presence-subscribe") then
         Self.Add_Feature
           (XMPP.Discoes_Features.Protocol_Pubsub_Presence_Subscribe);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#publish") then
         Self.Add_Feature (XMPP.Discoes_Features.Protocol_Pubsub_Publish);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#publisher-affiliation") then
         Self.Add_Feature
           (XMPP.Discoes_Features.Protocol_Pubsub_Publisher_Affiliation);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#purge-nodes") then
         Self.Add_Feature (XMPP.Discoes_Features.Protocol_Pubsub_Purge_Nodes);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#retract-items") then
         Self.Add_Feature
           (XMPP.Discoes_Features.Protocol_Pubsub_Retract_Items);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#retrieve-affiliations") then
         Self.Add_Feature
           (XMPP.Discoes_Features.Protocol_Pubsub_Retrieve_Affiliations);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#retrieve-default") then
         Self.Add_Feature
           (XMPP.Discoes_Features.Protocol_Pubsub_Retrieve_Default);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#retrieve-items") then
         Self.Add_Feature
           (XMPP.Discoes_Features.Protocol_Pubsub_Retrieve_Items);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#retrieve-subscriptions") then
         Self.Add_Feature
           (XMPP.Discoes_Features.Protocol_Pubsub_Retrieve_Subscriptions);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#subscribe") then
         Self.Add_Feature (XMPP.Discoes_Features.Protocol_Pubsub_Subscribe);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#subscription-notifications") then
         Self.Add_Feature
           (XMPP.Discoes_Features.Protocol_Pubsub_Subscription_Notifications);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#subscription-options") then
         Self.Add_Feature
           (XMPP.Discoes_Features.Protocol_Pubsub_Subscription_Options);

      elsif Val = To_Universal_String ("http://jabber.org/protocol/muc") then
         Self.Add_Feature (XMPP.Discoes_Features.Protocol_MUC);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/muc#unique") then
         Self.Add_Feature (XMPP.Discoes_Features.Protocol_Muc_Unique);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/rsm") then
         Self.Add_Feature (XMPP.Discoes_Features.Protocol_RSM);

      elsif Val = To_Universal_String
        ("iq") then
         Self.Add_Feature (XMPP.Discoes_Features.IQ);

      elsif Val = To_Universal_String
        ("jabber:iq:last") then
         Self.Add_Feature (XMPP.Discoes_Features.Jabber_Iq_Last);

      elsif Val = To_Universal_String ("msgoffline") then
         Self.Add_Feature (XMPP.Discoes_Features.Msgoffline);

      elsif Val = To_Universal_String ("vcard-temp") then
         Self.Add_Feature (XMPP.Discoes_Features.Vcard_Temp);

      elsif Val = To_Universal_String ("jabber:iq:register") then
         Self.Add_Feature (XMPP.Discoes_Features.Jabber_Iq_Roster);

      elsif Val = To_Universal_String ("jabber:iq:time") then
         Self.Add_Feature (XMPP.Discoes_Features.Jabber_Iq_Time);

      elsif Val = To_Universal_String ("jabber:iq:version") then
         Self.Add_Feature (XMPP.Discoes_Features.Jabber_Iq_Version);

      elsif Val = To_Universal_String ("presence") then
         Self.Add_Feature (XMPP.Discoes_Features.Presence);

      elsif Val = To_Universal_String ("presence-invisible") then
         Self.Add_Feature (XMPP.Discoes_Features.Presence_Invisible);

      elsif Val = To_Universal_String ("urn:xmpp:time") then
         Self.Add_Feature (XMPP.Discoes_Features.Urn_Xmpp_Time);
      else
         raise Program_Error
           with "XMPP.Discoes:Feature is not implemented : "
                 & Ada.Characters.Conversions.To_String
                    (Val.To_Wide_Wide_String);
      end if;
   end Add_Feature;

end XMPP.Discoes;
