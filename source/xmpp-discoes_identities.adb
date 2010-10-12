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
--  <Unit> XMPP.Discoes_Identities
--  <ImplementationNotes>
--
------------------------------------------------------------------------------
--  $Revision$ $Author$
--  $Date$
------------------------------------------------------------------------------
with Ada.Characters.Conversions;

package body XMPP.Discoes_Identities is

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

end XMPP.Discoes_Identities;

