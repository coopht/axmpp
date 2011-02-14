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
--  <Unit> XMPP.Challenges
--  <ImplementationNotes>
--
------------------------------------------------------------------------------
--  $Revision$ $Author$
--  $Date$
------------------------------------------------------------------------------
with Ada.Characters.Conversions;
with Ada.Streams;

with GNAT.MD5;

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
   function Get_Kind (Self : XMPP_Challenge) return XMPP.Objects.Object_Kind is
      pragma Unreferenced (Self);

   begin
      return XMPP.Objects.Challenge;
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
     Writer : in out XML.SAX.Pretty_Writers.SAX_Pretty_Writer'Class) is

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

   ----------------
   --  Set_Host  --
   ----------------
   procedure Set_Host (Self : in out XMPP_Challenge;
                       Host : League.Strings.Universal_String) is
   begin
      Self.Host := Host;
   end Set_Host;

   ---------------
   --  Set_JID  --
   ---------------
   procedure Set_JID (Self : in out XMPP_Challenge;
                      JID  : League.Strings.Universal_String) is
   begin
      Self.JID := JID;
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
