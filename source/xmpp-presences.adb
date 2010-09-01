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
with Ada.Wide_Wide_Text_IO;

with League.Strings;

with XMPP.Objects;

package body XMPP.Presences is

   use League.Strings;

   --------------
   --  Create  --
   --------------
   function Create return XMPP_Presence_Access is
   begin
      return new XMPP_Presence;
   end Create;

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

      Result : League.Strings.Universal_String
        := League.Strings.To_Universal_String ("<presence");

   begin
      if not Self.To.Is_Empty then
         Result.Append (" to='" & Self.To & "'");
      end if;

      if not Self.From.Is_Empty then
         Result.Append (" from='" & Self.From & "'");
      end if;

      Result := Result & ">";

      if Self.Priority /= -129 then
         Result := Result & "<priority>"
           & Priority_Type'Wide_Wide_Image (Self.Priority)
           & "</priority>";
      end if;

      if not Self.Status.Is_Empty then
         Result.Append ("<status>" & Self.Status & "</status>");
      end if;

      if Self.Show /= Online then
         Result.Append (To_Universal_String ("<show>"));

         case Self.Show is
            when Away =>
               Result.Append (To_Universal_String ("away"));

            when Chat =>
               Result.Append (To_Universal_String ("chat"));

            when DND =>
               Result.Append (To_Universal_String ("dnd"));

            when XA =>
               Result.Append (To_Universal_String ("xa"));

            when Online =>
               raise Program_Error;
         end case;

         Result.Append (To_Universal_String ("</show>"));
      end if;

      Result.Append (To_Universal_String ("</presence>"));

      return Result;
   end Serialize;

   -------------------
   --  Set_Content  --
   -------------------
   overriding procedure Set_Content
     (Self      : in out XMPP_Presence;
      Parameter : League.Strings.Universal_String;
      Value     : League.Strings.Universal_String) is
   begin
      if Parameter = To_Universal_String ("from") then
         Self.From := Value;

      elsif Parameter = To_Universal_String ("to") then
         Self.To := Value;

      --  elsif Parameter = To_Universal_String ("type") then
      --     Self.To := Value;

      else
         Ada.Wide_Wide_Text_IO.Put_Line
          ("Unknown Parameter : " & Parameter.To_Wide_Wide_String);
      end if;
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

   --------------
   --  Get_To  --
   --------------
   function Get_To (Self : XMPP_Presence)
      return League.Strings.Universal_String is
   begin
      return Self.To;
   end Get_To;

   ----------------
   --  Get_From  --
   ----------------
   function Get_From (Self : XMPP_Presence)
      return League.Strings.Universal_String is
   begin
      return Self.From;
   end Get_From;

   --------------
   --  Set_To  --
   --------------
   procedure Set_To (Self  : in out XMPP_Presence;
                     Value : League.Strings.Universal_String) is
   begin
      Self.To := Value;
   end Set_To;

   ----------------
   --  Set_From  --
   ----------------
   procedure Set_From (Self  : in out XMPP_Presence;
                       Value  : League.Strings.Universal_String) is
   begin
      Self.From := Value;
   end Set_From;

end XMPP.Presences;

