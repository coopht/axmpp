------------------------------------------------------------------------------
--                                                                          --
--                                 AXMPP                                    --
--                                                                          --
------------------------------------------------------------------------------
--                                                                          --
-- Copyright © 2011 Alexander Basov <coopht@gmail.com>                      --
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
--  <Unit> XMPP.Versions
--  <ImplementationNotes> Unit implements XEP-0092 extension:
--  XMPP protocol extension for retrieving information about the software
--  application associated with an XMPP entity. The protocol enables one
--  entity to explicitly query another entity, where the response can include
--  the name of the software application, the version of the software
--  application, and the operating system on which the application is running.
--
--  http://xmpp.org/extensions/xep-0092.html
--
------------------------------------------------------------------------------
--  $Revision$ $Author$
--  $Date$
------------------------------------------------------------------------------

package body XMPP.Versions is

   use type League.Strings.Universal_String;

   --------------
   --  Create  --
   --------------

   function Create return XMPP_Version_Access is
   begin
      return new XMPP_Version;
   end Create;

   ----------------
   --  Get_Kind  --
   ----------------

   overriding function Get_Kind (Self : XMPP_Version)
      return Objects.Object_Kind is
      pragma Unreferenced (Self);
   begin
      return XMPP.Objects.Version;
   end Get_Kind;

   ----------------
   --  Get_Name  --
   ----------------

   not overriding function Get_Name (Self : XMPP_Version)
      return League.Strings.Universal_String is
   begin
      return Self.Name;
   end Get_Name;

   --------------
   --  Get_OS  --
   --------------

   not overriding function Get_OS (Self : XMPP_Version)
      return League.Strings.Universal_String is
   begin
      return Self.OS;
   end Get_OS;

   -------------------
   --  Get_Version  --
   -------------------

   not overriding function Get_Version (Self : XMPP_Version)
      return League.Strings.Universal_String is
   begin
      return Self.Version;
   end Get_Version;

   -----------------
   --  Serialize  --
   -----------------

   overriding procedure Serialize
    (Self   : XMPP_Version;
     Writer : in out XML.SAX.Pretty_Writers.SAX_Pretty_Writer'Class) is

   begin
      Self.Start_IQ (Writer);

      Writer.Start_Prefix_Mapping (Namespace_URI => Version_URI);

      Writer.Start_Element (Namespace_URI => Version_URI,
                            Local_Name => Query_Element);

      --  Name

      Writer.Start_Element (Qualified_Name => Name_Element);
      Writer.Characters (Self.Name);
      Writer.End_Element (Qualified_Name => Name_Element);

      --  Version

      Writer.Start_Element (Qualified_Name => Version_Element);
      Writer.Characters (Self.Version);
      Writer.End_Element (Qualified_Name => Version_Element);

      --  OS

      if not Self.OS.Is_Empty then
         Writer.Start_Element (Qualified_Name => OS_Element);
         Writer.Characters (Self.OS);
         Writer.End_Element (Qualified_Name => OS_Element);
      end if;

      Writer.End_Element (Namespace_URI => Version_URI,
                          Local_Name => Query_Element);

      Writer.End_Prefix_Mapping;
      Self.End_IQ (Writer);
   end Serialize;

   -------------------
   --  Set_Content  --
   -------------------

   overriding
   procedure Set_Content (Self      : in out XMPP_Version;
                          Parameter : League.Strings.Universal_String;
                          Value     : League.Strings.Universal_String) is
   begin
      if Parameter = League.Strings.To_Universal_String ("name") then
         Self.Name := Value;

      elsif Parameter = League.Strings.To_Universal_String ("os") then
         Self.OS := Value;

      elsif Parameter = League.Strings.To_Universal_String ("version") then
         Self.Version := Value;
      end if;
   end Set_Content;

   ----------------
   --  Set_Name  --
   ----------------

   not overriding procedure Set_Name
     (Self : in out XMPP_Version;
      Name : League.Strings.Universal_String) is
   begin
      Self.Name := Name;
   end Set_Name;

   --------------
   --  Set_OS  --
   --------------

   not overriding procedure Set_OS
     (Self : in out XMPP_Version;
      OS   : League.Strings.Universal_String) is
   begin
      Self.OS := OS;
   end Set_OS;

   -------------------
   --  Set_Version  --
   -------------------

   not overriding procedure Set_Version
     (Self    : in out XMPP_Version;
      Version : League.Strings.Universal_String) is
   begin
      Self.Version := Version;
   end Set_Version;

end XMPP.Versions;
