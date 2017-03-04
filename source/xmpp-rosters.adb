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
package body XMPP.Rosters is

   use XMPP.Objects;

   -------------------
   --  Append_Item  --
   -------------------
   procedure Append_Item
     (Self : in out XMPP_Roster;
      Item : not null XMPP.Roster_Items.XMPP_Roster_Item_Access) is
   begin
      Self.Items.Append (XMPP_Object_Access (Item));
   end Append_Item;

   --------------
   --  Create  --
   --------------
   function Create return not null XMPP_Roster_Access is
   begin
      return new XMPP_Roster;
   end Create;

   ----------------
   --  Get_Kind  --
   ----------------
   overriding function Get_Kind (Self : XMPP_Roster) return Object_Kind
   is
      pragma Unreferenced (Self);
   begin
      return XMPP.Roster;
   end Get_Kind;

   ---------------
   --  Item_At  --
   ---------------
   function Item_At (Self : XMPP_Roster; Pos : Natural)
      return not null XMPP.Roster_Items.XMPP_Roster_Item_Access is
   begin
      return XMPP.Roster_Items.XMPP_Roster_Item_Access
               (Self.Items.Element (Pos));
   end Item_At;

   -------------------
   --  Items_Count  --
   -------------------
   function Items_Count (Self : XMPP_Roster) return Natural is
   begin
      return Natural (Self.Items.Length);
   end Items_Count;

   -----------------
   --  Serialize  --
   -----------------
   overriding procedure Serialize
    (Self   : XMPP_Roster;
     Writer : in out XML.SAX.Pretty_Writers.XML_Pretty_Writer'Class) is

   begin
      Self.Start_IQ (Writer);

      Writer.Start_Prefix_Mapping (Namespace_URI => Roster_URI);

      Writer.Start_Element (Namespace_URI => Roster_URI,
                            Local_Name => Query_Element);

      Writer.End_Element (Namespace_URI => Roster_URI,
                          Local_Name => Query_Element);

      Writer.End_Prefix_Mapping;

      Self.End_IQ (Writer);
   end Serialize;

   -------------------
   --  Set_Content  --
   -------------------
   overriding
   procedure Set_Content (Self      : in out XMPP_Roster;
                          Parameter : League.Strings.Universal_String;
                          Value     : League.Strings.Universal_String) is
   begin
      raise Program_Error with "Not yet implemented";
   end Set_Content;

end XMPP.Rosters;
