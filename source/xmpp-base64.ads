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
with Ada.Streams;

package XMPP.Base64 is

   --  RFC 1521, MIME Base64 encode/decode
   --  Assumes Ada.Streams.Stream_Element is a byte.

   procedure Decode (Source  :     String;
                     Target  : out Ada.Streams.Stream_Element_Array;
                     Last    : out Ada.Streams.Stream_Element_Offset);
   --  decode Source into Target(Target'first .. Last)
   --  Note: it may be appropriate to prescan Source for '=',
   --  indicating termination, or for illegitimate characters,
   --  indicating corruption, before calling Decode.

   procedure Encode (Source  :     Ada.Streams.Stream_Element_Array;
                     Target  : out String;
                     Last    : out Natural);
   --  Target is filled in four character increments, except that
   --  a CR-LF pair is inserted after every 76 characters.
   --  Target'length must be at least:
   --  Output_Quad_Count: constant := (Source'length + 2) / 3;
   --  Output_Byte_Count: constant := 4 * Output_Quad_Count;
   --  Target'length = Output_Byte_Count + 2 * (Output_Byte_Count / 76)
   --  Constraint_Error will be raised if Target isn't long enough.

end XMPP.Base64;
