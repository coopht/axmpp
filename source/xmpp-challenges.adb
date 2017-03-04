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
with Ada.Characters.Conversions;
with Ada.Streams;

with GNAT.MD5;

with League.String_Vectors;

with XMPP.Base64;
with XMPP.Logger;
with XMPP.Utils;

package body XMPP.Challenges is

   use type Ada.Streams.Stream_Element_Offset;

   use League.Strings;

   package ACC renames Ada.Characters.Conversions;

   function Create return XMPP_Challenge_Access is
   begin
      return new XMPP_Challenge;
   end Create;

   -------------------------
   --  Generate_Response  --
   -------------------------
   function Generate_Response (Self : XMPP_Challenge)
      return League.Strings.Universal_String is

      function Hex_To_Oct (Sum : String) return String;

      ------------------
      --  Hex_To_Oct  --
      ------------------
      function Hex_To_Oct (Sum : String) return String is
         Result : String (Sum'First .. Sum'Last / 2);
         J      : Integer := Sum'First;

      begin
         for Idx in Result'Range loop
            Result (Idx)
              := Character'Val
                   (Integer'Value (("16#" & Sum (J .. J + 1) & "#")));

            J := J + 2;
         end loop;

         return Result;
      end Hex_To_Oct;

   begin

      if not Self.RSP_Auth.Is_Empty then
         return League.Strings.To_Universal_String
                  ("<response xmlns='urn:ietf:params:xml:ns:xmpp-sasl'/>");
      else
         declare

            --  TODO:
            --        CNonce should not be hardcoded.
            C_Nonce : constant Wide_Wide_String
              := "1a2f0ee81279451956625d2368";

            SY     : constant String
              := Hex_To_Oct (GNAT.MD5.Digest
                               (ACC.To_String
                                  (Self.JID.To_Wide_Wide_String & ":"
                                     & Self.Host.To_Wide_Wide_String & ":"
                                     & Self.Password.To_Wide_Wide_String)));

            HA1 : constant String
              := GNAT.MD5.Digest
                  (SY & ":"
                     & ACC.To_String (Self.Nonce.To_Wide_Wide_String)
                     & ":" & ACC.To_String (C_Nonce));

            HA2 : constant String
              := GNAT.MD5.Digest
                  (ACC.To_String ("AUTHENTICATE:xmpp/"
                                    & Self.Host.To_Wide_Wide_String));

            Z   : constant String
              := GNAT.MD5.Digest
                  (HA1 & ":"
                     & ACC.To_String (Self.Nonce.To_Wide_Wide_String)
                     & ":00000001:"
                     & ACC.To_String (C_Nonce) & ":auth:"
                     & HA2);

            Realm_Reply : constant Wide_Wide_String
              := "username=""" & Self.JID.To_Wide_Wide_String
              & """,realm=""" & Self.Host.To_Wide_Wide_String
              & """,nonce=""" & Self.Nonce.To_Wide_Wide_String
              & """,cnonce=""" & C_Nonce
              & """,nc=00000001"
              & ",qop=auth,digest-uri=""xmpp/" & Self.Host.To_Wide_Wide_String
              & """,response=" & ACC.To_Wide_Wide_String (Z)
              & ",charset="
              & Self.Charset.To_Wide_Wide_String;

            --  Calculating buffer size for base64 encoded string
            X : constant Integer := 4 * (Realm_Reply'Length + 2) / 3;
            Y : constant Integer := X + 2 * (X / 76);

            Realm_Reply_Base_64 : String (Realm_Reply'First .. Y);
            Len                 : Natural;

         begin
            --  XMPP.Logger.Log ("SY:" & SY);
            --  XMPP.Logger.Log ("HA1:" & HA1);
            --  XMPP.Logger.Log ("HA2:" & HA2);
            --  XMPP.Logger.Log ("Z:" & Z);
            --  XMPP.Logger.Log ("Realm_Reply:" & ACC.To_String (Realm_Reply));

            XMPP.Base64.Encode
              (XMPP.Utils.To_Stream_Element_Array
                 (ACC.To_String (Realm_Reply)),
               Realm_Reply_Base_64,
               Len);
            --  XMPP.Logger.Log ("Realm_Reply_Base_64:" & Realm_Reply_Base_64);

            return
              League.Strings.To_Universal_String
              ("<response xmlns='urn:ietf:params:xml:ns:xmpp-sasl'>"
                 & ACC.To_Wide_Wide_String
                 (Realm_Reply_Base_64 (Realm_Reply_Base_64'First .. Len))
                 & "</response>");
         end;
      end if;
   end Generate_Response;

   ----------------
   --  Get_Kind  --
   ----------------
   overriding
   function Get_Kind (Self : XMPP_Challenge) return XMPP.Object_Kind is
      pragma Unreferenced (Self);

   begin
      return XMPP.Challenge;
   end Get_Kind;

   -----------------------
   --  Parse_Challenge  --
   -----------------------
   procedure Parse_Challenge (Self      : in out XMPP_Challenge;
                              Challenge : String) is

      function Drop_Quotes (Str : String) return String;

      -------------------
      --  Drop_Quotes  --
      -------------------
      function Drop_Quotes (Str : String) return String is
      begin
         if Str (Str'First) = '"' and Str (Str'Last) = '"' then
            return Str (Str'First + 1 .. Str'Last - 1);
         else
            return Str;
         end if;
      end Drop_Quotes;

      Pos : Integer := Challenge'First;

   begin
      if Challenge (Challenge'First .. Challenge'First + 6) = "rspauth" then
         Self.Set_RSP_Auth
           (League.Strings.To_Universal_String
              (ACC.To_Wide_Wide_String
                 (Challenge (Challenge'First + 8 .. Challenge'Last))));
         return;
      end if;

      for J in Challenge'Range loop
         if Challenge (J) = ',' then
            declare
               T : constant String := Challenge (Pos .. J - 1);

            begin
               for X in T'Range loop
                  if T (X) = '=' then
                     declare
                        Param : constant String := T (T'First .. X - 1);
                        Val   : constant String := T (X + 1 .. T'Last);

                     begin
                        if Param = "nonce" then
                           Self.Set_Nonce
                             (League.Strings.To_Universal_String
                                (ACC.To_Wide_Wide_String (Drop_Quotes (Val))));

                        elsif Param = "qop" then
                           Self.Set_Qop
                             (League.Strings.To_Universal_String
                                (ACC.To_Wide_Wide_String (Drop_Quotes (Val))));

                        elsif Param = "charset" then
                           Self.Set_Charset
                             (League.Strings.To_Universal_String
                                (ACC.To_Wide_Wide_String (Drop_Quotes (Val))));

                        elsif Param = "algorithm" then
                           Self.Set_Algorithm
                             (League.Strings.To_Universal_String
                                (ACC.To_Wide_Wide_String (Drop_Quotes (Val))));

                        else
                           XMPP.Logger.Log
                             ("unknown parameter : "
                                & ACC.To_Wide_Wide_String (Param));
                        end if;
                     end;
                  end if;
               end loop;
            end;
            Pos := J + 1;
         end if;
      end loop;
   end Parse_Challenge;

   -----------------
   --  Serialize  --
   -----------------
   overriding procedure Serialize
    (Self   : XMPP_Challenge;
     Writer : in out XML.SAX.Pretty_Writers.XML_Pretty_Writer'Class) is

      pragma Unreferenced (Self);
      pragma Unreferenced (Writer);

   begin
      raise Program_Error with "Not yet implemented";
   end Serialize;

   ---------------------
   --  Set_Algorithm  --
   ---------------------
   procedure Set_Algorithm (Self      : in out XMPP_Challenge;
                            Algorithm : League.Strings.Universal_String) is
   begin
      Self.Algorithm := Algorithm;
   end Set_Algorithm;

   -------------------
   --  Set_Charset  --
   -------------------
   procedure Set_Charset (Self    : in out XMPP_Challenge;
                          Charset : League.Strings.Universal_String) is
   begin
      Self.Charset := Charset;
   end Set_Charset;

   -------------------
   --  Set_Content  --
   -------------------
   overriding
   procedure Set_Content (Self      : in out XMPP_Challenge;
                          Parameter : League.Strings.Universal_String;
                          Value     : League.Strings.Universal_String) is
   begin
      if Parameter.To_Wide_Wide_String = "challenge" then
         declare
            Buffer : Ada.Streams.Stream_Element_Array (0 .. 4096);
            Length : Ada.Streams.Stream_Element_Offset;

         begin
            XMPP.Base64.Decode
              (Ada.Characters.Conversions.To_String
                 (Value.To_Wide_Wide_String),
               Buffer,
               Length);

            declare
               Result : String (1 .. Integer (Length));

            begin
               for J in 1 .. Length loop
                  Result (Integer (J)) := (Character'Val (Buffer (J - 1)));
               end loop;

               --  XMPP.Logger.Log ("Decoded challenge: " & Result);

               Self.Parse_Challenge (Result);
            end;
         end;
      else
         XMPP.Logger.Log ("Unknown parameter : " & Parameter);
      end if;
   end Set_Content;

   ---------------
   --  Set_JID  --
   ---------------
   procedure Set_JID (Self : in out XMPP_Challenge;
                      JID  : League.Strings.Universal_String) is
      Vec : constant League.String_Vectors.Universal_String_Vector
        := JID.Split ('@');

   begin
      if Vec.Length /= 2 then
         raise Program_Error with "Wrong jid specfified";

      else
         Self.JID := Vec.Element (1);
         Self.Host := Vec.Element (2);
      end if;
   end Set_JID;

   -----------------
   --  Set_Nonce  --
   -----------------
   procedure Set_Nonce (Self  : in out XMPP_Challenge;
                        Nonce : League.Strings.Universal_String) is
   begin
      Self.Nonce := Nonce;
   end Set_Nonce;

   --------------------
   --  Set_Password  --
   --------------------
   procedure Set_Password (Self     : in out XMPP_Challenge;
                           Password : League.Strings.Universal_String) is
   begin
      Self.Password := Password;
   end Set_Password;

   ---------------
   --  Set_Qop  --
   ---------------
   procedure Set_Qop (Self : in out XMPP_Challenge;
                      Qop  : League.Strings.Universal_String) is
   begin
      Self.Qop := Qop;
   end Set_Qop;

   -----------------
   --  Set_Realm  --
   -----------------
   procedure Set_Realm (Self : in out XMPP_Challenge;
                        Realm : League.Strings.Universal_String) is
   begin
      Self.Realm := Realm;
   end Set_Realm;

   --------------------
   --  Set_RSP_Auth  --
   --------------------
   procedure Set_RSP_Auth (Self     : in out XMPP_Challenge;
                           RSP_Auth : League.Strings.Universal_String) is
   begin
      Self.RSP_Auth := RSP_Auth;
   end Set_RSP_Auth;

end XMPP.Challenges;
