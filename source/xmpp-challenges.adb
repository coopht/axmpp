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
with Ada.Text_IO;
with Ada.Wide_Wide_Text_IO;

with XMPP.Base64;
with XMPP.Objects;

with League.Strings;

package body XMPP.Challenges is

   use type Ada.Streams.Stream_Element_Offset;

   -------------------
   --  Drop_Quotes  --
   -------------------
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

   ----------------
   --  Get_Kind  --
   ----------------
   overriding
   function Get_Kind (Self : XMPP_Challenge) return XMPP.Objects.Object_Kind is
   begin
      return XMPP.Objects.Challenge;
   end Get_Kind;

   -----------------
   --  Serialize  --
   -----------------
   overriding
   function Serialize (Self : in XMPP_Challenge)
      return League.Strings.Universal_String is
   begin
      raise Program_Error with "not yet implemented";
      return X : League.Strings.Universal_String;
   end Serialize;

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
                 (Parameter.To_Wide_Wide_String),
               Buffer,
               Length);

            declare
               Result : String (1 .. Integer (Length));

            begin
               for J in 1 .. Length loop
                  Result (Integer (J)) := Character'Val (Buffer (J - 1));
               end loop;

               Ada.Text_IO.Put_Line ("challenge = " & Result);
            end;
         end;
      else
         Ada.Wide_Wide_Text_IO.Put_Line
           ("Unknown parameter : " & Parameter.To_Wide_Wide_String);
      end if;
   end Set_Content;

   -----------------
   --  Set_Realm  --
   -----------------
   procedure Set_Realm (Self : in out XMPP_Challenge;
                        Realm : League.Strings.Universal_String) is
   begin
      Self.Realm := Realm;
   end Set_Realm;

   -----------------
   --  Set_Nonce  --
   -----------------
   procedure Set_Nonce (Self  : in out XMPP_Challenge;
                        Nonce : League.Strings.Universal_String) is
   begin
      Self.Nonce := Nonce;
   end Set_Nonce;

   ---------------
   --  Set_Qop  --
   ---------------
   procedure Set_Qop (Self : in out XMPP_Challenge;
                      Qop  : League.Strings.Universal_String) is
   begin
      Self.Qop := Qop;
   end Set_Qop;

   -------------------
   --  Set_Charset  --
   -------------------
   procedure Set_Charset (Self    : in out XMPP_Challenge;
                          Charset : League.Strings.Universal_String) is
   begin
      Self.Charset := Charset;
   end Set_Charset;

   ---------------------
   --  Set_Algorithm  --
   ---------------------
   procedure Set_Algorithm (Self      : in out XMPP_Challenge;
                            Algorithm : League.Strings.Universal_String) is
   begin
      Self.Algorithm := Algorithm;
   end Set_Algorithm;

end XMPP.Challenges;
