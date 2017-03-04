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

package body XMPP.Streams is

   use type League.Strings.Universal_String;

   function Create return XMPP_Stream_Access is
   begin
      return new XMPP_Stream;
   end Create;

   ----------------
   --  Get_Kind  --
   ----------------
   overriding function Get_Kind (Self : XMPP_Stream) return XMPP.Object_Kind is
      pragma Unreferenced (Self);

   begin
      return XMPP.Stream;
   end Get_Kind;

   -----------------
   --  Serialize  --
   -----------------
   overriding procedure Serialize
    (Self : XMPP_Stream;
     Writer : in out XML.SAX.Pretty_Writers.XML_Pretty_Writer'Class) is
      pragma Unreferenced (Self);
      pragma Unreferenced (Writer);

   begin
      raise Program_Error with "Not Yet Implemented";
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
         XMPP.Logger.Log ("Unknonw Parameter : " & Parameter);
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
