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
--  <Unit> XMPP.IQS
--  <ImplementationNotes>
--
------------------------------------------------------------------------------
--  $Revision$ $Author$
--  $Date$
------------------------------------------------------------------------------
with League.Strings;

with XMPP.Objects;

package body XMPP.IQS is

   ----------------
   --  Get_Kind  --
   ----------------
   overriding function Get_Kind (Self : XMPP_IQ) return Objects.Object_Kind is
   begin
      return Objects.IQ;
   end Get_Kind;

   -------------------
   --  Get_IQ_Kind  --
   -------------------
   function Get_IQ_Kind (Self : XMPP_IQ) return IQ_Kind is
   begin
      return Self.Kind_Of_IQ;
   end Get_IQ_Kind;

   --------------
   --  Get_Id  --
   --------------
   function Get_Id (Self : XMPP_IQ) return League.Strings.Universal_String is
   begin
      return Self.Id;
   end Get_Id;

   --------------
   --  Set_Id  --
   --------------
   procedure Set_Id (Self : in out XMPP_IQ;
                     Val  : League.Strings.Universal_String) is
   begin
      Self.Id := Val;
   end Set_Id;

   -------------------
   --  Set_IQ_Kind  --
   -------------------
   procedure Set_IQ_Kind (Self : in out XMPP_IQ; Val : IQ_Kind) is
   begin
      Self.Kind_Of_IQ := Val;
   end Set_IQ_Kind;

   -----------------
   --  Serialize  --
   -----------------
   overriding function Serialize (Self : in XMPP_IQ)
      return League.Strings.Universal_String is
   begin
      return X : League.Strings.Universal_String;
   end Serialize;

   -------------------
   --  Set_Content  --
   -------------------
   overriding procedure Set_Content
     (Self      : in out XMPP_IQ;
      Parameter : League.Strings.Universal_String;
      Value     : League.Strings.Universal_String) is
   begin
      null;
   end Set_Content;

   ----------------
   --  Set_Body  --
   ----------------
   procedure Set_Body (Self : in out XMPP_IQ;
                       Val  : League.Strings.Universal_String) is
   begin
      Self.IQ_Body := Val;
   end Set_Body;

   ----------------
   --  Get_Body  --
   ----------------
   function Get_Body (Self : XMPP_IQ) return League.Strings.Universal_String is
   begin
      return Self.IQ_Body;
   end Get_Body;

   -------------------
   --  Append_Item  --
   -------------------
   procedure Append_Item
     (Self : in out XMPP_IQ;
      Item : not null access XMPP.Objects.XMPP_Object'Class) is
   begin
      Self.Item_List.Append (XMPP.Objects.XMPP_Object_Access (Item));
   end Append_Item;

   -------------------
   --  Items_Count  --
   -------------------
   function Items_Count (Self : XMPP_IQ) return Natural is
   begin
      return Natural (Self.Item_List.Length);
   end Items_Count;

   ---------------
   --  Item_At  --
   ---------------
   function Item_At (Self : XMPP_IQ; Pos : Natural)
     return not null access XMPP.Objects.XMPP_Object'Class is
   begin
      return Self.Item_List.Element (Pos);
   end Item_At;

end XMPP.IQS;

