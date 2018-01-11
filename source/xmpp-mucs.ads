------------------------------------------------------------------------------
--                                                                          --
--                              AXMPP Project                               --
--                                                                          --
--                           XMPP Library for Ada                           --
--                                                                          --
------------------------------------------------------------------------------
--                                                                          --
-- Copyright © 2011-2018, Alexander Basov <coopht@gmail.com>                --
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
with League.Strings;

with XML.SAX.Pretty_Writers;

with XMPP.Objects;

package XMPP.MUCS is

   MUC_Element : constant League.Strings.Universal_String
     := League.Strings.To_Universal_String ("x");

   MUC_URI : constant League.Strings.Universal_String
     := League.Strings.To_Universal_String
         ("http://jabber.org/protocol/muc");

   History_Element : constant League.Strings.Universal_String
     := League.Strings.To_Universal_String ("history");

   type MUC_Item is record
      Affilation : MUC_Affilation := None;
      Role       : MUC_Role := None;
   end record;

   type Optional_Integer (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : Integer;
         when False =>
            null;
      end case;
   end record;

   type MUC_History is record
      Max_Chars : Optional_Integer;
   end record;

   type XMPP_MUC is new XMPP.Objects.XMPP_Object with private;

   type XMPP_MUC_Access is access all XMPP_MUC'Class;

   overriding function Get_Kind (Self : XMPP_MUC) return Object_Kind;

   overriding procedure Serialize
    (Self   : XMPP_MUC;
     Writer : in out XML.SAX.Pretty_Writers.XML_Pretty_Writer'Class);

   overriding procedure Set_Content
     (Self      : in out XMPP_MUC;
      Parameter : League.Strings.Universal_String;
      Value     : League.Strings.Universal_String);

   function Create return XMPP_MUC_Access;

   procedure Set_Item (Self : in out XMPP_MUC; Item : MUC_Item);
   procedure Set_History (Self : in out XMPP_MUC; Value : MUC_History);

private

   type XMPP_MUC is new XMPP.Objects.XMPP_Object with
   record
      Item    : MUC_Item;
      History : MUC_History;
   end record;
end XMPP.MUCS;
