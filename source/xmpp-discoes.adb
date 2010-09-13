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
      return X : League.Strings.Universal_String;
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
         Self.Type_Of_Disco := Info;

      elsif Parameter = To_Universal_String ("disco#items") then
         Self.Type_Of_Disco := Items;

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
   function Get_Type (Self : XMPP_Disco) return Disco_Type is
   begin
      return Self.Type_Of_Disco;
   end Get_Type;

end XMPP.Discoes;
