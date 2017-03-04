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
with XMPP.Logger;

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
   overriding function Get_Kind (Self : XMPP_Bind) return Object_Kind
   is
      pragma Unreferenced (Self);
   begin
      return XMPP.Bind;
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
   overriding procedure Serialize
    (Self   : XMPP_Bind;
     Writer : in out XML.SAX.Pretty_Writers.XML_Pretty_Writer'Class) is

   begin
      Self.Start_IQ (Writer);

      Writer.Start_Prefix_Mapping (Namespace_URI => Bind_URI);

      Writer.Start_Element
       (Namespace_URI => Bind_URI,
        Local_Name    => Bind_Element);

      if not Self.Get_Resource.Is_Empty then
         Writer.Start_Element (Qualified_Name => Resource_Element);

         Writer.Characters (Self.Get_Resource);

         Writer.End_Element (Qualified_Name => Resource_Element);
      end if;

      Writer.End_Element (Namespace_URI => Bind_URI,
                          Local_Name    => Bind_Element);

      Writer.End_Prefix_Mapping;
      Self.End_IQ (Writer);
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
         XMPP.Logger.Log
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
