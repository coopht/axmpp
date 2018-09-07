------------------------------------------------------------------------------
--                                                                          --
--                              AXMPP Project                               --
--                                                                          --
--                           XMPP Library for Ada                           --
--                                                                          --
------------------------------------------------------------------------------
--                                                                          --
-- Copyright © 2011, Alexander Basov <coopht@gmail.com>                     --
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
package body XMPP.Base64 is

   subtype Six_Bits is Ada.Streams.Stream_Element range 0 .. 63;

   From_String : constant array (Character) of Six_Bits
     := ('A' =>  0, 'B' =>  1, 'C' =>  2, 'D' =>  3, 'E' =>  4, 'F' =>  5,
         'G' =>  6, 'H' =>  7, 'I' =>  8, 'J' =>  9, 'K' => 10, 'L' => 11,
         'M' => 12, 'N' => 13, 'O' => 14, 'P' => 15, 'Q' => 16, 'R' => 17,
         'S' => 18, 'T' => 19, 'U' => 20, 'V' => 21, 'W' => 22, 'X' => 23,
         'Y' => 24, 'Z' => 25, 'a' => 26, 'b' => 27, 'c' => 28, 'd' => 29,
         'e' => 30, 'f' => 31, 'g' => 32, 'h' => 33, 'i' => 34, 'j' => 35,
         'k' => 36, 'l' => 37, 'm' => 38, 'n' => 39, 'o' => 40, 'p' => 41,
         'q' => 42, 'r' => 43, 's' => 44, 't' => 45, 'u' => 46, 'v' => 47,
         'w' => 48, 'x' => 49, 'y' => 50, 'z' => 51, '0' => 52, '1' => 53,
         '2' => 54, '3' => 55, '4' => 56, '5' => 57, '6' => 58, '7' => 59,
         '8' => 60, '9' => 61, '+' => 62, '/' => 63, others => 0);

   procedure Decode (Source  :     String;
                     Target  : out Ada.Streams.Stream_Element_Array;
                     Last    : out Ada.Streams.Stream_Element_Offset) is
      --  decode Source into Target(Target'first .. Last)
      --  Note: it may be appropriate to prescan Source for '=',
      --  indicating termination, or for illegitimate characters,
      --  indicating corruption, before calling Decode.
      use type Ada.Streams.Stream_Element;
      use type Ada.Streams.Stream_Element_Offset;

      D       : Six_Bits;

      type Slots is mod 4;

      Slot    : Slots := 0;

   begin
      Last := Target'First - 1;

      for SI in Source'Range loop
         D := From_String (Source (SI));
         if D /= 0 or else Source (SI) = 'A' then
            --  OK source
            case Slot is
               when 0 =>
                  Last := Last + 1;
                  Target (Last) := 4 * D;
                  --  dddddd00 ........ ........

               when 1 =>
                  Target (Last) := Target (Last) + D / 16;
                  exit when Last = Target'Last
                    and then (SI = Source'Last or else Source (SI + 1) = '=')
                    and then (D mod 16) = 0;
                  Last := Last + 1;
                  Target (Last) := (D mod 16) * 16;
                  --  dddddddd dddd0000 ........
               when 2 =>
                  Target (Last) := Target (Last) + D / 4;
                  exit when Last = Target'Last
                    and then (SI = Source'Last or else Source (SI + 1) = '=')
                    and then (D mod 4) = 0;
                  Last := Last + 1;
                  Target (Last) := (D mod 4) * 64;
                  --  dddddddd dddddddd dd000000

               when 3 =>
                  Target (Last) := Target (Last) + D;
                  --  dddddddd dddddddd dddddddd

            end case;
            Slot := Slot + 1;
         elsif Source (SI) = '=' then
            exit; --  terminator encountered
         end if; --  silently ignore whitespace, lf, garbage, ...
      end loop;
   end Decode;

   To_String : constant array (Six_Bits) of Character
     := "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

   procedure Encode (Source  :     Ada.Streams.Stream_Element_Array;
                     Target  : out String;
                     Last    : out Natural) is
      --  Target is filled in four character increments, except that
      --  a CR-LF pair is inserted after every 76 characters.
      --  Target'length must be at least:
      --  Output_Quad_Count: constant := (Source'length + 2) / 3;
      --  Output_Byte_Count: constant := 4 * Output_Quad_Count;
      --  Target'length = Output_Byte_Count + 2 * (Output_Byte_Count / 76)
      --  Constraint_Error will be raised if Target isn't long enough.
      use type Ada.Streams.Stream_Element;
      D       : Six_Bits;
      type Slots is mod 3;
      Slot    : Slots := 0;
      Output_Line_Length : Natural := 0;

   begin
      Last := Target'First - 1;

      for SI in Source'Range loop
         case Slot is
            when 0 =>
               --  if Output_Line_Length = 76 then
               --     Last := Last + 2;
               --     Target (Last - 1) := ASCII.CR;
               --     Target (Last) := ASCII.LF;
               --     Output_Line_Length := 0;
               --  end if;
               Output_Line_Length := Output_Line_Length + 4;
               Last := Last + 4;
               Target (Last - 3) := To_String (Source (SI) / 4);
               D := (Source (SI) mod 4) * 16;
               Target (Last - 2) := To_String (D);
               Target (Last - 1) := '=';
               Target (Last) := '=';
               --  dddddd dd0000  = =
            when 1 =>
               D := D + Source (SI) / 16;
               Target (Last - 2) := To_String (D);
               D := (Source (SI) mod 16) * 4;
               Target (Last - 1) := To_String (D);
               --  dddddd dddddd dddd00 =

            when 2 =>
               D := D + Source (SI) / 64;
               Target (Last - 1) := To_String (D);
               Target (Last) := To_String (Source (SI) mod 64);
               --  dddddd dddddd dddddd dddddd
         end case;
         Slot := Slot + 1;
      end loop;
   end Encode;

end XMPP.Base64;
