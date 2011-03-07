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
with Ada.Characters.Conversions;

package body XMPP.Services_Identities is

   use League.Strings;

   function Create (Category : League.Strings.Universal_String;
                    I_Type   : League.Strings.Universal_String) return Identity
   is
   begin
      if Category = To_Universal_String ("account") then
         if I_Type = To_Universal_String ("admin") then
            return (Account, Admin);

         elsif I_Type = To_Universal_String ("anonymous") then
            return (Account, Anonymous);

         elsif I_Type = To_Universal_String ("registered") then
            return (Account, Registered);

         else
            raise Program_Error with "Unknown type of the Category"
              & Ada.Characters.Conversions.To_String
                 (I_Type.To_Wide_Wide_String);
         end if;

      elsif Category = To_Universal_String ("conference") then
         if I_Type = To_Universal_String ("text") then
            return (Conference, Text);

         elsif I_Type = To_Universal_String ("IRC") then
            return (Conference, IRC);

         else
            raise Program_Error with "Unknown type of the Category : "
              & Ada.Characters.Conversions.To_String
                 (I_Type.To_Wide_Wide_String);
         end if;
      --  elsif Category = To_Universal_String ("auth") then
      --  elsif Category = To_Universal_String ("automation") then
      --  elsif Category = To_Universal_String ("client") then
      --  elsif Category = To_Universal_String ("collaboration") then
      --  elsif Category = To_Universal_String ("component") then
      --  elsif Category = To_Universal_String ("directory") then
      --  elsif Category = To_Universal_String ("gateway") then
      --  elsif Category = To_Universal_String ("headline") then
      --  elsif Category = To_Universal_String ("hierarchy") then
      --  elsif Category = To_Universal_String ("proxy") then
      elsif Category = To_Universal_String ("pubsub") then
         if I_Type = To_Universal_String ("collection") then
            return (Pubsub, Collection);

         elsif I_Type = To_Universal_String ("leaf") then
            return (Pubsub, Leaf);

         elsif I_Type = To_Universal_String ("pep") then
            return (Pubsub, Pep);

         elsif I_Type = To_Universal_String ("service") then
            return (Pubsub, Service);

         else
            raise Program_Error with "Unknown type of the Category"
              & Ada.Characters.Conversions.To_String
                 (I_Type.To_Wide_Wide_String);
         end if;

      elsif Category = To_Universal_String ("server") then
         if I_Type = To_Universal_String ("im") then
            return (Server, IM);
         else
            raise Program_Error
              with "Unknown type of the Category : "
                     & Ada.Characters.Conversions.To_String
                        (I_Type.To_Wide_Wide_String);
         end if;

      --  elsif Category = To_Universal_String ("store") then

      else
         raise Program_Error with "Identity of Category is not implemented : "
           & Ada.Characters.Conversions.To_String
              (Category.To_Wide_Wide_String);
      end if;
   end Create;

end XMPP.Services_Identities;
