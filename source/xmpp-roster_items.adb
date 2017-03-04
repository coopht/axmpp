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
with XML.SAX.Attributes;

with XMPP.Logger;

package body XMPP.Roster_Items is

   use League.Strings;

   --------------------
   --  Append_Group  --
   --------------------
   procedure Append_Group (Self  : in out XMPP_Roster_Item;
                           Value : League.Strings.Universal_String) is
   begin
      --  Self.Groups.Append (Value);
      null;
   end Append_Group;

   --------------
   --  Create  --
   --------------
   function Create return not null XMPP_Roster_Item_Access is
   begin
      return new XMPP_Roster_Item;
   end Create;

   ------------------
   --  Get_Groups  --
   ------------------
   function Get_Groups (Self : XMPP_Roster_Item)
      return League.String_Vectors.Universal_String_Vector is
   begin
      return Self.Groups;
   end Get_Groups;

   ---------------
   --  Get_JID  --
   ---------------
   function Get_JID (Self : XMPP_Roster_Item)
      return League.Strings.Universal_String is
   begin
      return Self.JID;
   end Get_JID;

   ----------------
   --  Get_Kind  --
   ----------------
   overriding function Get_Kind (Self : XMPP_Roster_Item) return Object_Kind
   is
      pragma Unreferenced (Self);
   begin
      return XMPP.Roster_Item;
   end Get_Kind;

   ----------------
   --  Get_Name  --
   ----------------
   function Get_Name (Self : XMPP_Roster_Item)
      return League.Strings.Universal_String is
   begin
      return Self.Name;
   end Get_Name;

   ------------------------
   --  Get_Subscription  --
   ------------------------
   function Get_Subscription (Self : XMPP_Roster_Item)
      return Subscription_Type is
   begin
      return Self.Subscription;
   end Get_Subscription;

   -----------------
   --  Serialize  --
   -----------------
   overriding procedure Serialize
    (Self   : XMPP_Roster_Item;
     Writer : in out XML.SAX.Pretty_Writers.XML_Pretty_Writer'Class) is

      Attrs   : XML.SAX.Attributes.SAX_Attributes;

   begin
      if not Self.JID.Is_Empty then
         Attrs.Set_Value (Qualified_Name => JID_Attribute,
                          Value          => Self.JID);
      end if;

      if not Self.Name.Is_Empty then
         Attrs.Set_Value (Qualified_Name => Name_Attribute,
                          Value          => Self.Name);
      end if;

      Writer.Start_Element (Qualified_Name => Item_Element,
                            Attributes     => Attrs);

      for J in 0 .. Self.Groups.Length - 1 loop
         if not Self.Groups.Is_Empty then
            Writer.Start_Element (Qualified_Name => Group_Element);
            Writer.Characters (Self.Groups.Element (J));
            Writer.End_Element (Qualified_Name => Group_Element);
         end if;
      end loop;

      Writer.End_Element (Qualified_Name => Item_Element);
   end Serialize;

   -------------------
   --  Set_Content  --
   -------------------
   overriding
   procedure Set_Content (Self      : in out XMPP_Roster_Item;
                          Parameter : League.Strings.Universal_String;
                          Value     : League.Strings.Universal_String) is
   begin
      if Parameter = To_Universal_String ("subscription") then
         if Value = To_Universal_String ("both") then
            Self.Subscription := Both;
         else
            Self.Subscription := None;
         end if;

      elsif Parameter = To_Universal_String ("jid") then
         Self.JID := Value;

      elsif Parameter = To_Universal_String ("group") then
         Self.Append_Group (Value);

      else
         XMPP.Logger.Log ("Unknow parameter: " & Parameter);
      end if;
   end Set_Content;

   ---------------
   --  Set_JID  --
   ---------------
   procedure Set_JID (Self : in out XMPP_Roster_Item;
                      Value : League.Strings.Universal_String) is
   begin
      Self.JID := Value;
   end Set_JID;

   ----------------
   --  Set_Name  --
   ----------------
   procedure Set_Name (Self  : in out XMPP_Roster_Item;
                       Value : League.Strings.Universal_String) is
   begin
      Self.Name := Value;
   end Set_Name;

   ------------------------
   --  Set_Subscription  --
   ------------------------
   procedure Set_Subscription (Self  : in out XMPP_Roster_Item;
                               Value : Subscription_Type) is
   begin
      Self.Subscription := Value;
   end Set_Subscription;

end XMPP.Roster_Items;
