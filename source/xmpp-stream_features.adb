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
--  <Unit> XMPP.Stream_Features
--  <ImplementationNotes>
--
------------------------------------------------------------------------------
--  $Revision$ $Author$
--  $Date$
------------------------------------------------------------------------------
with Ada.Wide_Wide_Text_IO;

package body XMPP.Stream_Features is

   use type League.Strings.Universal_String;

   ---------------------
   --  Add_Mechanism  --
   ---------------------
   procedure Add_Mechanism (Self  : in out XMPP_Stream_Feature;
                            Value : Wide_Wide_String)
   is
   begin
      if Value = "PLAIN" then
         Self.Mechanisms.Append (PLAIN);

      elsif Value = "DIGEST-MD5" then
         Self.Mechanisms.Append (DIGEST_MD5);
      else
         Ada.Wide_Wide_Text_IO.Put_Line
           ("Uknown mechanism detected : " & Value);
      end if;
   end Add_Mechanism;

   --------------
   --  Create  --
   --------------
   function Create return XMPP_Stream_Feature_Access is
   begin
      return new XMPP_Stream_Feature;
   end Create;

   ----------------
   --  Get_Kind  --
   ----------------
   overriding function Get_Kind (Self : XMPP_Stream_Feature)
      return XMPP.Objects.Object_Kind is
      pragma Unreferenced (Self);
   begin
      return XMPP.Objects.Stream_Features;
   end Get_Kind;

   -------------------------
   --  Is_Bind_Supported  --
   -------------------------
   function Is_Bind_Supported (Self : XMPP_Stream_Feature) return Boolean is
   begin
      return Self.Bind_Supported;
   end Is_Bind_Supported;

   ----------------------------
   --  Is_Session_Supported  --
   ----------------------------
   function Is_Session_Supported (Self : XMPP_Stream_Feature) return Boolean is
   begin
      return Self.Session_Supported;
   end Is_Session_Supported;

   -----------------
   --  Serialize  --
   -----------------
   overriding procedure Serialize
    (Self   : XMPP_Stream_Feature;
     Writer : in out XML.SAX.Pretty_Writers.SAX_Pretty_Writer'Class) is

      pragma Unreferenced (Self);
      pragma Unreferenced (Writer);
   begin
      raise Program_Error with "Not yet implemented";
   end Serialize;

   -------------------
   --  Set_Content  --
   -------------------
   overriding
   procedure Set_Content (Self      : in out XMPP_Stream_Feature;
                          Parameter : League.Strings.Universal_String;
                          Value     : League.Strings.Universal_String)
   is
   begin
      if Parameter.To_Wide_Wide_String = "starttls" then
         Self.Set_Has_TLS;

      elsif Parameter.To_Wide_Wide_String = "mechanism" then
         Self.Add_Mechanism (Value.To_Wide_Wide_String);

      elsif Parameter.To_Wide_Wide_String = "bind" then
         Self.Bind_Supported := Parameter = Value;

      elsif Parameter.To_Wide_Wide_String = "session" then
         Self.Bind_Supported := Parameter = Value;

      else
         Ada.Wide_Wide_Text_IO.Put_Line
           ("!!! Unknown Parameter : " & Parameter.To_Wide_Wide_String);
      end if;
   end Set_Content;

   -------------------
   --  Set_Has_TLS  --
   -------------------
   procedure Set_Has_TLS (Self  : in out XMPP_Stream_Feature;
                          Value : Boolean := True) is
   begin
      Self.Has_TLS := Value;
   end Set_Has_TLS;

end XMPP.Stream_Features;
