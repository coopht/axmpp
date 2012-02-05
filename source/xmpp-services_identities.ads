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
with Ada.Containers.Indefinite_Vectors;

with League.Strings;

package XMPP.Services_Identities is

   --  XXX: May be this type is too complex;
   type Identity (Category : Category_Type) is record
      case Category is
         when Account =>
            Account : Account_Type;

         when Auth =>
            Auth : Auth_Type;

         when Automation =>
            Automation : Automation_Type;

         when Client =>
            Client : Client_Type;

         when Collaboration =>
            Collaboration : Collaboration_Type;

         when Component =>
            Component : Component_Type;

         when Conference =>
            Conference : Conference_Type;

         when Directory =>
            Directory : Directory_Type;

         when Gateway =>
            Gateway : Gateway_Type;

         when Headline =>
            Headline : Headline_Type;

         when Hierarchy =>
            Hierarchy : Hierarchy_Type;

         when Proxy =>
            Proxy : Proxy_Type;

         when Pubsub =>
            Pubsub : Pubsub_Type;

         when Server =>
            Server : Server_Type;

         when Store =>
            Store : Store_Type;

      end case;

   end record;

   package Identities_Vectors is
      new Ada.Containers.Indefinite_Vectors (Natural, Identity);

   subtype Identities_Vector is Identities_Vectors.Vector;

   function Create (Category : League.Strings.Universal_String;
                    I_Type   : League.Strings.Universal_String)
     return Identity;

end XMPP.Services_Identities;
