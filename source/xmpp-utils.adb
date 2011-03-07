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
