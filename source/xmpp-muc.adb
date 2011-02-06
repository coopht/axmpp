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
--  <Unit> XMPP.MUC
--  <ImplementationNotes>
--
------------------------------------------------------------------------------
--  $Revision$ $Author$
--  $Date$
------------------------------------------------------------------------------

package body XMPP.MUC is

   --------------
   --  Create  --
   --------------
   function Create return XMPP_MUC_Access is
   begin
      return new XMPP_MUC;
   end Create;

   ----------------
   --  Get_Kind  --
   ----------------
   overriding function Get_Kind (Self : XMPP_MUC) return Objects.Object_Kind is
      pragma Unreferenced (Self);

   begin
      return XMPP.Objects.MUC;
   end Get_Kind;

   -----------------
   --  Serialize  --
   -----------------
   overriding procedure Serialize
    (Self   : XMPP_MUC;
     Writer : in out XML.SAX.Pretty_Writers.SAX_Pretty_Writer'Class) is
      pragma Unreferenced (Self);

   begin
      Writer.Start_Prefix_Mapping (Namespace_URI => MUC_URI);

      Writer.Start_Element (Namespace_URI => MUC_URI,
                            Local_Name    => MUC_Element);

      Writer.End_Element (Namespace_URI => MUC_URI,
                          Local_Name => MUC_Element);
   end Serialize;

   -------------------
   --  Set_Content  --
   -------------------
   overriding procedure Set_Content
     (Self      : in out XMPP_MUC;
      Parameter : League.Strings.Universal_String;
      Value     : League.Strings.Universal_String) is
   begin
      raise Program_Error with "Not yet implemented";
   end Set_Content;

   ----------------
   --  Set_Item  --
   ----------------
   procedure Set_Item (Self : in out XMPP_MUC; Item : MUC_Item) is
   begin
      Self.Item := Item;
   end Set_Item;

end XMPP.MUC;
