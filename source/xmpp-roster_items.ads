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
with League.String_Vectors;
with League.Strings;

with XML.SAX.Pretty_Writers;

with XMPP.Objects;

package XMPP.Roster_Items is

   Item_Element : constant League.Strings.Universal_String
     := League.Strings.To_Universal_String ("item");

   Group_Element : constant League.Strings.Universal_String
     := League.Strings.To_Universal_String ("group");

   JID_Attribute : constant League.Strings.Universal_String
     := League.Strings.To_Universal_String ("jid");

   Name_Attribute : constant League.Strings.Universal_String
     := League.Strings.To_Universal_String ("name");

   Subscription_Attribute : constant League.Strings.Universal_String
     := League.Strings.To_Universal_String ("subscription");

   type XMPP_Roster_Item is new XMPP.Objects.XMPP_Object with private;

   type XMPP_Roster_Item_Access is access all XMPP_Roster_Item'Class;

   --  Public API
   procedure Set_Subscription (Self  : in out XMPP_Roster_Item;
                               Value : Subscription_Type);
   --  Sets subscribtion type

   function Get_Subscription (Self : XMPP_Roster_Item)
      return Subscription_Type;
   --  Returns subscribtion type

   function Get_JID (Self : XMPP_Roster_Item)
      return League.Strings.Universal_String;
   --  Returns items identifier

   function Get_Name (Self : XMPP_Roster_Item)
      return League.Strings.Universal_String;
   --  Returns items name

   procedure Set_JID (Self : in out XMPP_Roster_Item;
                      Value : League.Strings.Universal_String);
   --  Sets items id

   procedure Set_Name (Self  : in out XMPP_Roster_Item;
                       Value : League.Strings.Universal_String);
   --  Sets items name

   function Create return not null XMPP_Roster_Item_Access;
   --  returns heap allocated object

   procedure Append_Group (Self  : in out XMPP_Roster_Item;
                           Value : League.Strings.Universal_String);

   function Get_Groups (Self : XMPP_Roster_Item)
     return League.String_Vectors.Universal_String_Vector;

   --  Private API
   --  Should not be used in application
   overriding function Get_Kind (Self : XMPP_Roster_Item) return Object_Kind;

   overriding procedure Serialize
    (Self : XMPP_Roster_Item;
     Writer : in out XML.SAX.Pretty_Writers.XML_Pretty_Writer'Class);

   overriding
   procedure Set_Content (Self      : in out XMPP_Roster_Item;
                          Parameter : League.Strings.Universal_String;
                          Value     : League.Strings.Universal_String);

private

   type XMPP_Roster_Item is new XMPP.Objects.XMPP_Object with
   record
      JID          : League.Strings.Universal_String;
      Name         : League.Strings.Universal_String;
      Subscription : Subscription_Type := None;
      Groups       : League.String_Vectors.Universal_String_Vector;
   end record;

end XMPP.Roster_Items;
