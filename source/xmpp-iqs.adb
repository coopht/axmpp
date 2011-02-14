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
with XML.SAX.Attributes;

with XMPP.Logger;

package body XMPP.IQS is

   use League.Strings;

   -------------------
   --  Append_Item  --
   -------------------
   procedure Append_Item
     (Self : in out XMPP_IQ;
      Item : not null access XMPP.Objects.XMPP_Object'Class) is
   begin
      Self.Item_List.Append (XMPP.Objects.XMPP_Object_Access (Item));
   end Append_Item;

   --------------
   --  Create  --
   --------------
   function Create (X : IQ_Kind) return XMPP_IQ_Access is
   begin
      return new XMPP_IQ (X);
   end Create;

   ----------------
   --  Get_Body  --
   ----------------
   function Get_Body (Self : XMPP_IQ) return League.Strings.Universal_String is
   begin
      return Self.IQ_Body;
   end Get_Body;

   ----------------
   --  Get_From  --
   ----------------
   function Get_From (Self : XMPP_IQ) return League.Strings.Universal_String is
   begin
      return Self.From;
   end Get_From;

   --------------
   --  Get_Id  --
   --------------
   function Get_Id (Self : XMPP_IQ) return League.Strings.Universal_String is
   begin
      return Self.Id;
   end Get_Id;

   -------------------
   --  Get_IQ_Kind  --
   -------------------
   function Get_IQ_Kind (Self : XMPP_IQ) return IQ_Kind is
   begin
      return Self.Kind_Of_IQ;
   end Get_IQ_Kind;

   ----------------
   --  Get_Kind  --
   ----------------
   overriding function Get_Kind (Self : XMPP_IQ) return Objects.Object_Kind is
      pragma Unreferenced (Self);

   begin
      return Objects.IQ;
   end Get_Kind;

   --------------
   --  Get_To  --
   --------------
   function Get_To (Self : XMPP_IQ) return League.Strings.Universal_String is
   begin
      return Self.To;
   end Get_To;

   ---------------
   --  Item_At  --
   ---------------
   function Item_At (Self : XMPP_IQ; Pos : Natural)
     return not null access XMPP.Objects.XMPP_Object'Class is
   begin
      return Self.Item_List.Element (Pos);
   end Item_At;

   -------------------
   --  Items_Count  --
   -------------------
   function Items_Count (Self : XMPP_IQ) return Natural is
   begin
      return Natural (Self.Item_List.Length);
   end Items_Count;

   -----------------
   --  Serialize  --
   -----------------
   overriding procedure Serialize
    (Self   : XMPP_IQ;
     Writer : in out XML.SAX.Pretty_Writers.SAX_Pretty_Writer'Class) is

      Attrs   : XML.SAX.Attributes.SAX_Attributes;

   begin
      --  Generating IQ container xml
      case Self.Kind_Of_IQ is
         when Set =>
            Attrs.Set_Value
             (Qualified_Name => IQ_Type_Attribute,
              Value          => To_Universal_String ("set"));

         when Get =>
            Attrs.Set_Value
             (Qualified_Name => IQ_Type_Attribute,
              Value          => To_Universal_String ("get"));

         when Result =>
            Attrs.Set_Value
             (Qualified_Name => IQ_Type_Attribute,
              Value          => To_Universal_String ("result"));

         when Error =>
            Attrs.Set_Value
             (Qualified_Name => IQ_Type_Attribute,
              Value          => To_Universal_String ("error"));
      end case;

      Attrs.Set_Value
        (Qualified_Name => IQ_Id_Attribute,
         Value          => Self.Get_Id);

      if not Self.To.Is_Empty then
         Attrs.Set_Value
          (Qualified_Name => IQ_To_Attribute,
           Value          => Self.To);
      end if;

      Writer.Start_Element (Qualified_Name => IQ_Element,
                            Attributes     => Attrs);

      --  Generating IQ body
      if Self.Items_Count > 0 then
         for J in 0 .. Self.Items_Count - 1 loop
            Self.Item_At (J).Serialize (Writer);
         end loop;
      end if;

      Writer.End_Element (Qualified_Name => IQ_Element);
   end Serialize;

   ----------------
   --  Set_Body  --
   ----------------
   procedure Set_Body (Self : in out XMPP_IQ;
                       Val  : League.Strings.Universal_String) is
   begin
      Self.IQ_Body := Val;
   end Set_Body;

   -------------------
   --  Set_Content  --
   -------------------
   overriding procedure Set_Content
     (Self      : in out XMPP_IQ;
      Parameter : League.Strings.Universal_String;
      Value     : League.Strings.Universal_String) is
   begin
      if Parameter.To_Wide_Wide_String = "type" then
         if Value.To_Wide_Wide_String = "set" then
            Self.Kind_Of_IQ := Set;

         elsif Value.To_Wide_Wide_String = "get" then
            Self.Kind_Of_IQ := Get;

         elsif Value.To_Wide_Wide_String = "result" then
            Self.Kind_Of_IQ := Result;

         elsif Value.To_Wide_Wide_String = "error" then
            Self.Kind_Of_IQ := Error;
         end if;

      elsif Parameter.To_Wide_Wide_String = "id" then
         Self.Id := Value;

      elsif Parameter.To_Wide_Wide_String = "to" then
         Self.To := Value;

      elsif Parameter.To_Wide_Wide_String = "from" then
         Self.From := Value;

      elsif Parameter.To_Wide_Wide_String = "iq" then
         null;

      else
         XMPP.Logger.Log ("XMPP_IQ : Unknown parameter : " & Parameter);
      end if;
   end Set_Content;

   ----------------
   --  Set_From  --
   ----------------
   procedure Set_From (Self : in out XMPP_IQ;
                       Val  : League.Strings.Universal_String) is
   begin
      Self.From := Val;
   end Set_From;

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

   --------------
   --  Set_To  --
   --------------
   procedure Set_To (Self : in out XMPP_IQ;
                     Val  : League.Strings.Universal_String) is
   begin
      Self.To := Val;
   end Set_To;

end XMPP.IQS;
