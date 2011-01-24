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
--  Base64 encode/decode & test driver.
--  Copyright 2001 Tom Moran (tmoran@acm.org, PGP signed tmoran@bix.com),
--  anyone may use for any purpose.
--
--  XXX : This module should be rewritten
------------------------------------------------------------------------------
--  $Revision$ $Author$
--  $Date$
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
      use type Ada.Streams.Stream_Element_Offset;
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
