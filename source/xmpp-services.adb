------------------------------------------------------------------------------
--                                                                          --
--                              AXMPP Project                               --
--                                                                          --
--                           XMPP Library for Ada                           --
--                                                                          --
------------------------------------------------------------------------------
--                                                                          --
-- Copyright © 2011-2016, Alexander Basov <coopht@gmail.com>                --
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
with Ada.Characters.Conversions;

with XMPP.Logger;

package body XMPP.Services is

   use League.Strings;

   -------------------
   --  Add_Feature  --
   -------------------
   procedure Add_Feature (Self : in out XMPP_Service;
                          Val  : XMPP.Feature) is
   begin
      Self.Features.Append (Val);
   end Add_Feature;

   -------------------
   --  Add_Feature  --
   -------------------
   procedure Add_Feature (Self : in out XMPP_Service;
                          Val  : League.Strings.Universal_String)
   is
      pragma Style_Checks (Off);
   begin
      if Val = To_Universal_String ("http://jabber.org/protocol/commands") then
         Self.Add_Feature (XMPP.Protocol_Commands);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/disco#info") then
         Self.Add_Feature (XMPP.Protocol_Disco_Info);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/disco#items") then
         Self.Add_Feature (XMPP.Protocol_Disco_Items);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub") then
         Self.Add_Feature (XMPP.Protocol_Pubsub);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#access-authorize") then
         Self.Add_Feature (XMPP.Protocol_Pubsub_Access_Authorize);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#access-presence") then
         Self.Add_Feature (XMPP.Protocol_Pubsub_Access_Presence);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#access-whitelist") then
         Self.Add_Feature (XMPP.Protocol_Pubsub_Access_Whitelist);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#auto-create") then
         Self.Add_Feature (XMPP.Protocol_Pubsub_Auto_Create);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#auto-subscribe") then
         Self.Add_Feature (XMPP.Protocol_Pubsub_Auto_Subscribe);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#collections") then
         Self.Add_Feature (XMPP.Protocol_Pubsub_Collections);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#access-open") then
         Self.Add_Feature (XMPP.Protocol_Pubsub_Access_Open);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#config-node") then
         Self.Add_Feature (XMPP.Protocol_Pubsub_Access_Open);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#create-and-configure") then
         Self.Add_Feature (XMPP.Protocol_Pubsub_Create_And_Configure);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#create-nodes") then
         Self.Add_Feature (XMPP.Protocol_Pubsub_Create_Nodes);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#delete-items") then
         Self.Add_Feature (XMPP.Protocol_Pubsub_Delete_Items);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#delete-nodes") then
         Self.Add_Feature (XMPP.Protocol_Pubsub_Delete_Nodes);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#filtered-notifications") then
         Self.Add_Feature (XMPP.Protocol_Pubsub_Filtered_Notifications);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#get-pending") then
         Self.Add_Feature (XMPP.Protocol_Pubsub_Get_Pending);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#instant-nodes") then
         Self.Add_Feature (XMPP.Protocol_Pubsub_Instant_Nodes);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#item-ids") then
         Self.Add_Feature (XMPP.Protocol_Pubsub_Item_Ids);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#last-published") then
         Self.Add_Feature (XMPP.Protocol_Pubsub_Last_Published);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#manage-subscriptions") then
         Self.Add_Feature (XMPP.Protocol_Pubsub_Manage_Subscription);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#member-affiliation") then
         Self.Add_Feature (XMPP.Protocol_Pubsub_Member_Affiliation);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#modify-affiliations") then
         Self.Add_Feature (XMPP.Protocol_Pubsub_Modify_Affiliations);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#multi-subscribe") then
         Self.Add_Feature (XMPP.Protocol_Pubsub_Multi_Subscribe);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#outcast-affiliation") then
         Self.Add_Feature (XMPP.Protocol_Pubsub_Outcast_Affiliation);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#owner") then
         Self.Add_Feature (XMPP.Protocol_Pubsub_Owner);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#persistent-items") then
         Self.Add_Feature (XMPP.Protocol_Pubsub_Persistent_Items);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#presence-notifications") then
         Self.Add_Feature (XMPP.Protocol_Pubsub_Presence_Notifications);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#presence-subscribe") then
         Self.Add_Feature (XMPP.Protocol_Pubsub_Presence_Subscribe);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#publish") then
         Self.Add_Feature (XMPP.Protocol_Pubsub_Publish);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#publisher-affiliation") then
         Self.Add_Feature (XMPP.Protocol_Pubsub_Publisher_Affiliation);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#purge-nodes") then
         Self.Add_Feature (XMPP.Protocol_Pubsub_Purge_Nodes);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#retract-items") then
         Self.Add_Feature (XMPP.Protocol_Pubsub_Retract_Items);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#retrieve-affiliations") then
         Self.Add_Feature (XMPP.Protocol_Pubsub_Retrieve_Affiliations);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#retrieve-default") then
         Self.Add_Feature (XMPP.Protocol_Pubsub_Retrieve_Default);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#retrieve-items") then
         Self.Add_Feature (XMPP.Protocol_Pubsub_Retrieve_Items);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#retrieve-subscriptions") then
         Self.Add_Feature (XMPP.Protocol_Pubsub_Retrieve_Subscriptions);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#subscribe") then
         Self.Add_Feature (XMPP.Protocol_Pubsub_Subscribe);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#subscription-notifications") then
         Self.Add_Feature (XMPP.Protocol_Pubsub_Subscription_Notifications);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/pubsub#subscription-options") then
         Self.Add_Feature (XMPP.Protocol_Pubsub_Subscription_Options);

      elsif Val = To_Universal_String ("http://jabber.org/protocol/muc") then
         Self.Add_Feature (XMPP.Protocol_MUC);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/muc#unique") then
         Self.Add_Feature (XMPP.Protocol_Muc_Unique);

      elsif Val = To_Universal_String
        ("http://jabber.org/protocol/rsm") then
         Self.Add_Feature (XMPP.Protocol_RSM);

      elsif Val = To_Universal_String
        ("iq") then
         Self.Add_Feature (XMPP.IQ);

      elsif Val = To_Universal_String
        ("jabber:iq:last") then
         Self.Add_Feature (XMPP.Jabber_Iq_Last);

      elsif Val = To_Universal_String ("msgoffline") then
         Self.Add_Feature (XMPP.Msgoffline);

      elsif Val = To_Universal_String ("vcard-temp") then
         Self.Add_Feature (XMPP.Vcard_Temp);

      elsif Val = To_Universal_String ("jabber:iq:register") then
         Self.Add_Feature (XMPP.Jabber_Iq_Roster);

      elsif Val = To_Universal_String ("jabber:iq:time") then
         Self.Add_Feature (XMPP.Jabber_Iq_Time);

      elsif Val = To_Universal_String ("jabber:iq:version") then
         Self.Add_Feature (XMPP.Jabber_Iq_Version);

      elsif Val = To_Universal_String ("presence") then
         Self.Add_Feature (XMPP.Presence);

      elsif Val = To_Universal_String ("presence-invisible") then
         Self.Add_Feature (XMPP.Presence_Invisible);

      elsif Val = To_Universal_String ("urn:xmpp:time") then
         Self.Add_Feature (XMPP.Urn_Xmpp_Time);

      elsif Val = To_Universal_String ("muc_public") then
         Self.Add_Feature (XMPP.Muc_Public);

      elsif Val = To_Universal_String ("muc_temporary") then
         Self.Add_Feature (XMPP.Muc_Temporary);

      elsif Val = To_Universal_String ("muc_open") then
         Self.Add_Feature (XMPP.Muc_Open);

      elsif Val = To_Universal_String ("muc_semianonymous") then
         Self.Add_Feature (XMPP.Muc_Semianonymous);

      elsif Val = To_Universal_String ("muc_moderated") then
         Self.Add_Feature (XMPP.Muc_Moderated);

      elsif Val = To_Universal_String ("muc_unsecured") then
         Self.Add_Feature (XMPP.Muc_Unsecured);
      else
         raise Program_Error
           with "XMPP.Services:Feature is not implemented : "
                 & Ada.Characters.Conversions.To_String
                    (Val.To_Wide_Wide_String);
      end if;
   end Add_Feature;

   --------------------
   --  Add_Identity  --
   --------------------
   procedure Add_Identity (Self : in out XMPP_Service;
                           Val  : XMPP.Services_Identities.Identity) is
   begin
      Self.Identities.Append (Val);
   end Add_Identity;

   ----------------
   --  Add_Item  --
   ----------------
   procedure Add_Item (Self : in out XMPP_Service; Item : Service_Item) is
   begin
      Self.Items.Append (Item);
   end Add_Item;

   --------------
   --  Create  --
   --------------
   function Create return not null XMPP_Service_Access is
   begin
      return new XMPP_Service;
   end Create;

   --------------------
   --  Get_Features  --
   --------------------
   function Get_Features (Self : XMPP_Service)
      return XMPP.Services_Features.Features_Vector is
   begin
      return Self.Features;
   end Get_Features;

   ----------------------
   --  Get_Identities  --
   ----------------------
   function Get_Identities (Self : XMPP_Service)
      return XMPP.Services_Identities.Identities_Vector is
   begin
      return Self.Identities;
   end Get_Identities;

   -----------------
   --  Get_Items  --
   -----------------
   function Get_Items (Self : XMPP_Service)
      return Service_Items_Package.Vector is
   begin
      return Self.Items;
   end Get_Items;

   ----------------
   --  Get_Kind  --
   ----------------
   overriding function Get_Kind (Self : XMPP_Service) return Object_Kind is
      pragma Unreferenced (Self);

   begin
      return XMPP.Disco;
   end Get_Kind;

   ----------------
   --  Get_Type  --
   ----------------
   function Get_Type (Self : XMPP_Service) return XMPP.Feature is
   begin
      return Self.Type_Of_Service;
   end Get_Type;

   -----------------
   --  Serialize  --
   -----------------
   overriding procedure Serialize
    (Self   : XMPP_Service;
     Writer : in out XML.SAX.Pretty_Writers.XML_Pretty_Writer'Class) is

      URI : constant League.Strings.Universal_String
        := XMPP.Services_Features.Image (Self.Type_Of_Service);

   begin
      Self.Start_IQ (Writer);
      Writer.Start_Prefix_Mapping (Namespace_URI => URI);

      Writer.Start_Element
        (Namespace_URI => URI,
         Local_Name    => Query_Element);

      Writer.End_Element (Namespace_URI => URI,
                          Local_Name    => Query_Element);

      Writer.End_Prefix_Mapping;
      Self.End_IQ (Writer);
   end Serialize;

   -------------------
   --  Set_Content  --
   -------------------
   overriding
   procedure Set_Content (Self      : in out XMPP_Service;
                          Parameter : League.Strings.Universal_String;
                          Value     : League.Strings.Universal_String) is
      pragma Unreferenced (Value);

   begin
      if Parameter = To_Universal_String ("disco#info") then
         Self.Type_Of_Service := XMPP.Protocol_Disco_Info;

      elsif Parameter = To_Universal_String ("disco#items") then
         Self.Type_Of_Service := XMPP.Protocol_Disco_Items;

      --  Added ignoring of tag itself
      elsif Parameter = To_Universal_String ("query") then
         null;

      else
         XMPP.Logger.Log ("XMPP_Service: Unknown parameter : " & Parameter);
      end if;
   end Set_Content;

   ----------------
   --  Set_Type  --
   ----------------
   procedure Set_Type (Self : in out XMPP_Service;
                       Val  : XMPP.Feature) is
   begin
      Self.Type_Of_Service := Val;
   end Set_Type;

end XMPP.Services;
