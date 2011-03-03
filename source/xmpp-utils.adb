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
--  <Unit> XMPP.Utils
--  <ImplementationNotes>
--
------------------------------------------------------------------------------
--  $Revision$ $Author$
--  $Date$
------------------------------------------------------------------------------
with Ada.Unchecked_Conversion;

package body XMPP.Utils is

   use League.Strings;

   Int_Id : Integer := 0;

   --------------
   --  Gen_Id  --
   --------------
   function Gen_Id (Prefix : League.Strings.Universal_String
                      := League.Strings.Empty_Universal_String)
      return League.Strings.Universal_String is

      function Image (J : Integer) return Wide_Wide_String;

      -------------
      --  Image  --
      -------------
      function Image (J : Integer) return Wide_Wide_String is
         S : constant Wide_Wide_String
           := Integer'Wide_Wide_Image (J);

      begin
         return S (S'First + 1 .. S'Last);
      end Image;

      Tmp : League.Strings.Universal_String := Prefix;

   begin
      Int_Id := Int_Id + 1;

      if Int_Id > 1000 then
         Int_Id := 0;
      end if;

      if Tmp.Is_Empty then
         Tmp := Tmp & "aa";
      end if;

      return Tmp & Image (Int_Id);
   end Gen_Id;

   -------------------------------
   --  To_Stream_Element_Array  --
   -------------------------------
   function To_Stream_Element_Array (Value : String)
      return Ada.Streams.Stream_Element_Array
   is
      subtype Source is String (Value'Range);
      subtype Result is Ada.Streams.Stream_Element_Array
        (Ada.Streams.Stream_Element_Offset (Value'First)
           .. Ada.Streams.Stream_Element_Offset (Value'Last));

      function To_Array is
         new Ada.Unchecked_Conversion (Source, Result);
   begin
      return To_Array (Value);
   end To_Stream_Element_Array;

end XMPP.Utils;
