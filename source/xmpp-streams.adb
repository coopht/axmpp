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
--  <Unit> XMPP.Streams
--  <ImplementationNotes>
--
------------------------------------------------------------------------------
--  $Revision$ $Author$
--  $Date$
------------------------------------------------------------------------------
with Ada.Wide_Wide_Text_IO;

with League.Strings;

with XMPP.Objects;

package body XMPP.Streams is

   use type League.Strings.Universal_String;

   function Create return XMPP_Stream_Access is
   begin
      return new XMPP_Stream;
   end Create;

   ----------------
   --  Get_Kind  --
   ----------------
   overriding
   function Get_Kind (Self : XMPP_Stream) return XMPP.Objects.Object_Kind is
   begin
      return XMPP.Objects.Stream;
   end Get_Kind;

   -----------------
   --  Serialize  --
   -----------------
   function Serialize (Self : in XMPP_Stream)
      return League.Strings.Universal_String
   is
   begin
      return League.Strings.To_Universal_String ("");
   end Serialize;

   -------------------
   --  Set_Content  --
   -------------------
   overriding
   procedure Set_Content (Self      : in out XMPP_Stream;
                          Parameter : League.Strings.Universal_String;
                          Value     : League.Strings.Universal_String)
   is
   begin
      if Parameter.To_Wide_Wide_String = "id" then
         Self.Set_Id (Value);

      elsif Parameter.To_Wide_Wide_String = "from" then
         Self.Set_From (Value);

      elsif Parameter.To_Wide_Wide_String = "lang" then
         Self.Set_Lang (Value);

      elsif Parameter.To_Wide_Wide_String = "version" then
         Self.Set_Version (Value);

      else
         Ada.Wide_Wide_Text_IO.Put_Line
           ("Unknonw Parameter : " & Parameter.To_Wide_Wide_String);
      end if;
   end Set_Content;

   ----------------
   --  Set_From  --
   ----------------
   procedure Set_From (Self  : in out XMPP_Stream;
                       Value : League.Strings.Universal_String) is
   begin
      Self.From := Value;
   end Set_From;

   --------------
   --  Set_Id  --
   --------------
   procedure Set_Id (Self  : in out XMPP_Stream;
                     Value : League.Strings.Universal_String) is
   begin
      Self.Id := Value;
   end Set_Id;

   --------------
   --  Set_Id  --
   --------------
   procedure Set_Id (Self : in out XMPP_Stream; Value : Integer) is
   begin
      Self.Id := League.Strings.To_Universal_String
                  (Integer'Wide_Wide_Image (Value));
   end Set_Id;

   ----------------
   --  Set_Lang  --
   ----------------
   procedure Set_Lang (Self  : in out XMPP_Stream;
                       Value : League.Strings.Universal_String) is
   begin
      Self.Lang := Value;
   end Set_Lang;

   -------------------
   --  Set_Version  --
   -------------------
   procedure Set_Version (Self  : in out XMPP_Stream;
                          Value : League.Strings.Universal_String) is
   begin
      Self.Version := Value;
   end Set_Version;

end XMPP.Streams;
