------------------------------------------------------------------------------
--                                                                          --
--                              AXMPP Project                               --
--                                                                          --
--                           XMPP Library for Ada                           --
--                                                                          --
------------------------------------------------------------------------------
--                                                                          --
-- Copyright © 2011-2016, Alexander Basov <coopht@gmail.com>                --
-- All rights reserved.                                                     --
--                                                                          --
-- Redistribution and use in source and binary forms, with or without       --
-- modification, are permitted provided that the following conditions       --
-- are met:                                                                 --
--                                                                          --
--  * Redistributions of source code must retain the above copyright        --
--    notice, this list of conditions and the following disclaimer.         --
--                                                                          --
--  * Redistributions in binary form must reproduce the above copyright     --
--    notice, this list of conditions and the following disclaimer in the   --
--    documentation and/or other materials provided with the distribution.  --
--                                                                          --
--  * Neither the name of the Alexander Basov, IE nor the names of its      --
--    contributors may be used to endorse or promote products derived from  --
--    this software without specific prior written permission.              --
--                                                                          --
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS      --
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT        --
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR    --
-- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT     --
-- HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,   --
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED --
-- TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR   --
-- PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF   --
-- LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING     --
-- NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS       --
-- SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.             --
--                                                                          --
------------------------------------------------------------------------------
--  $Revision$ $Date$
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

   overriding function Get_Kind (Self : XMPP_Version) return Object_Kind is
      pragma Unreferenced (Self);
   begin
      return XMPP.Version;
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
     Writer : in out XML.SAX.Pretty_Writers.XML_Pretty_Writer'Class) is

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
