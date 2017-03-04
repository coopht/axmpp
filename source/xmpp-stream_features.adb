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
         XMPP.Logger.Log  ("Uknown mechanism detected : " & Value);
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
      return XMPP.Object_Kind is
      pragma Unreferenced (Self);
   begin
      return Stream_Featuress;
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
     Writer : in out XML.SAX.Pretty_Writers.XML_Pretty_Writer'Class) is

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
         XMPP.Logger.Log ("!!! Unknown Parameter : " & Parameter);
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
