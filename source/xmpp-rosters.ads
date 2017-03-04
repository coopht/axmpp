------------------------------------------------------------------------------
--                                                                          --
--                              AXMPP Project                               --
--                                                                          --
--                           XMPP Library for Ada                           --
--                                                                          --
------------------------------------------------------------------------------
--                                                                          --
-- Copyright © 2011-2016, Alexander Basov <coopht@gmail.com>                --
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

with XMPP.IQS;
with XMPP.Objects;
with XMPP.Roster_Items;

package XMPP.Rosters is

   Query_Element    : constant League.Strings.Universal_String
     := League.Strings.To_Universal_String ("query");

   Roster_Element : constant League.Strings.Universal_String
     := League.Strings.To_Universal_String ("roster");

   Roster_URI : constant League.Strings.Universal_String
     := League.Strings.To_Universal_String ("jabber:iq:roster");

   type XMPP_Roster is new XMPP.IQS.XMPP_IQ with private;

   type XMPP_Roster_Access is access all XMPP_Roster'Class;

   --  Public API  --

   function Items_Count (Self : XMPP_Roster) return Natural;
   --  Returns number of roster_Items in roster.

   function Item_At (Self : XMPP_Roster; Pos : Natural)
      return not null XMPP.Roster_Items.XMPP_Roster_Item_Access;
   --  Returns roster item at given position.

   procedure Append_Item
     (Self : in out XMPP_Roster;
      Item : not null XMPP.Roster_Items.XMPP_Roster_Item_Access);
   --  Appends item to roster.

   function Create return not null XMPP_Roster_Access;
   --  Returns heap-allocated roster object.

   --  End of public API  --

   --  Private API, should not be used by application
   overriding function Get_Kind (Self : XMPP_Roster) return Object_Kind;

   overriding procedure Serialize
    (Self   : XMPP_Roster;
     Writer : in out XML.SAX.Pretty_Writers.XML_Pretty_Writer'Class);

   overriding
   procedure Set_Content (Self      : in out XMPP_Roster;
                          Parameter : League.Strings.Universal_String;
                          Value     : League.Strings.Universal_String);
private

   type XMPP_Roster is new XMPP.IQS.XMPP_IQ with
   record
      Items : XMPP.Objects.Object_Vectors.Vector;
   end record;

end XMPP.Rosters;
