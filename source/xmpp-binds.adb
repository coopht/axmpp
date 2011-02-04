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
--  <Unit> XMPP.Binds
--  <ImplementationNotes>
--
------------------------------------------------------------------------------
--  $Revision$ $Author$
--  $Date$
------------------------------------------------------------------------------
with Ada.Wide_Wide_Text_IO;

with XML.SAX.Pretty_Writers;

package body XMPP.Binds is

   use League.Strings;

   --------------
   --  Create  --
   --------------
   function Create return not null XMPP_Bind_Access is
   begin
      return new XMPP_Bind;
   end Create;

   ---------------
   --  Get_JID  --
   ---------------
   function Get_JID (Self : XMPP_Bind) return League.Strings.Universal_String
   is
   begin
      return Self.JID;
   end Get_JID;

   ----------------
   --  Get_Kind  --
   ----------------
   overriding function Get_Kind (Self : XMPP_Bind) return Objects.Object_Kind
   is
      pragma Unreferenced (Self);
   begin
      return XMPP.Objects.Bind;
   end Get_Kind;

   --------------------
   --  Get_Resource  --
   --------------------
   function Get_Resource (Self : XMPP_Bind)
      return League.Strings.Universal_String is
   begin
      return Self.Resource;
   end Get_Resource;

   -----------------
   --  Serialize  --
   -----------------
   overriding function Serialize (Self : XMPP_Bind)
      return League.Strings.Universal_String is
      Writer  : XML.SAX.Pretty_Writers.SAX_Pretty_Writer;
      Success : Boolean := False;

   begin
      Writer.Start_Prefix_Mapping
       (Namespace_URI => Bind_URI,
        Success       => Success);

      Writer.Start_Element
       (Namespace_URI => Bind_URI,
        Local_Name    => Bind_Element,
        Success       => Success);

      if not Self.Get_Resource.Is_Empty then
         Writer.Start_Element
          (Qualified_Name => Resource_Element,
           Success        => Success);

         Writer.Characters (Self.Get_Resource, Success);

         Writer.End_Element (Qualified_Name => Resource_Element,
                             Success        => Success);
      end if;

      Writer.End_Element (Namespace_URI => Bind_URI,
                          Local_Name    => Bind_Element,
                          Success       => Success);

      Writer.End_Prefix_Mapping (Success => Success);

      return Writer.Text;
   end Serialize;

   -------------------
   --  Set_Content  --
   -------------------
   overriding
   procedure Set_Content (Self      : in out XMPP_Bind;
                          Parameter : League.Strings.Universal_String;
                          Value     : League.Strings.Universal_String) is
   begin
      if Parameter = To_Universal_String ("jid") then
         Self.JID := Value;

      else
         Ada.Wide_Wide_Text_IO.Put_Line
           ("Unknown parameter : " & Parameter.To_Wide_Wide_String);
      end if;
   end Set_Content;

   ---------------
   --  Set_JID  --
   ---------------
   procedure Set_JID (Self : in out XMPP_Bind;
                      JID  : League.Strings.Universal_String) is
   begin
      Self.JID := JID;
   end Set_JID;

   --------------------
   --  Set_Resource  --
   --------------------
   procedure Set_Resource (Self : in out XMPP_Bind;
                           Res  : League.Strings.Universal_String) is
   begin
      Self.Resource := Res;
   end Set_Resource;

end XMPP.Binds;
