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
--  <Unit> XMPP.Presences
--  <ImplementationNotes>
--
------------------------------------------------------------------------------
--  $Revision$ $Author$
--  $Date$
------------------------------------------------------------------------------
with League.Strings;

with XMPP.Objects;

package body XMPP.Presences is

   ----------------
   --  Get_Kind  --
   ----------------
   overriding function Get_Kind (Self : XMPP_Presence)
      return Objects.Object_Kind is
   begin
      return XMPP.Objects.Presence;
   end Get_Kind;

   -----------------
   --  Serialize  --
   -----------------
   overriding function Serialize (Self : in XMPP_Presence)
      return League.Strings.Universal_String is
   begin
      return X : League.Strings.Universal_String;
   end Serialize;

   -------------------
   --  Set_Content  --
   -------------------
   overriding procedure Set_Content
     (Self      : in out XMPP_Presence;
      Parameter : League.Strings.Universal_String;
      Value     : League.Strings.Universal_String) is
   begin
      raise Program_Error with "Not yet implemented";
   end Set_Content;

   ----------------
   --  Set_Show  --
   ----------------
   procedure Set_Show (Self : in out XMPP_Presence; Show : Show_Kind) is
   begin
      Self.Show := Show;
   end Set_Show;

   ----------------
   --  Get_Show  --
   ----------------
   function Get_Show (Self : XMPP_Presence) return Show_Kind is
   begin
      return Self.Show;
   end Get_Show;

   ------------------
   --  Set_Status  --
   ------------------
   procedure Set_Status (Self   : in out XMPP_Presence;
                         Status : League.Strings.Universal_String) is
   begin
      Self.Status := Status;
   end Set_Status;

   ------------------
   --  Get_Status  --
   ------------------
   function Get_Status (Self : XMPP_Presence)
      return League.Strings.Universal_String is
   begin
      return Self.Status;
   end Get_Status;

   --------------------
   --  Set_Priority  --
   --------------------
   procedure Set_Priority (Self : in out XMPP_Presence; P : Priority_Type) is
   begin
      Self.Priority := P;
   end Set_Priority;

   --------------------
   --  Get_Priority  --
   --------------------
   function Get_Priority (Self : XMPP_Presence) return Priority_Type is
   begin
      return Self.Priority;
   end Get_Priority;

end XMPP.Presences;

