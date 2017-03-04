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

with XMPP.MUCS;
with XMPP.Objects;

package XMPP.Presences is

   Presence_Element        : constant League.Strings.Universal_String
     := League.Strings.To_Universal_String ("presence");

   Presence_To_Attribute   : constant League.Strings.Universal_String
     := League.Strings.To_Universal_String ("to");

   Presence_From_Attribute : constant League.Strings.Universal_String
     := League.Strings.To_Universal_String ("from");

   Priority_Element        : constant League.Strings.Universal_String
     := League.Strings.To_Universal_String ("priority");

   Status_Element        : constant League.Strings.Universal_String
     := League.Strings.To_Universal_String ("status");

   Show_Element        : constant League.Strings.Universal_String
     := League.Strings.To_Universal_String ("show");

   type XMPP_Presence is new XMPP.Objects.XMPP_Object with private;

   type XMPP_Presence_Access is access all XMPP_Presence'Class;

   --  Public API  --

   procedure Set_Show (Self : in out XMPP_Presence; Show : Show_Kind);
   --  Sets presence

   function Get_Show (Self : XMPP_Presence) return Show_Kind;
   --  Returns presence

   procedure Set_Status (Self   : in out XMPP_Presence;
                         Status : League.Strings.Universal_String);
   --  Sets extended text status for presence

   function Get_Status (Self : XMPP_Presence)
      return League.Strings.Universal_String;
   --  returns extended text status for presence

   procedure Set_Priority (Self : in out XMPP_Presence; P : Priority_Type);
   --  Sets presence priority

   function Get_Priority (Self : XMPP_Presence) return Priority_Type;
   --  Reuturns presence priority

   function Get_To (Self : XMPP_Presence)
      return League.Strings.Universal_String;
   --  Returns presence repcipient

   function Get_From (Self : XMPP_Presence)
      return League.Strings.Universal_String;
   --  Returns presence sender

   procedure Set_To (Self  : in out XMPP_Presence;
                     Value : League.Strings.Universal_String);
   --  Sets presence repcipient

   procedure Set_From (Self  : in out XMPP_Presence;
                       Value : League.Strings.Universal_String);
   --  Sets presence sender

   procedure Set_Type (Self : in out XMPP_Presence; Value : Presence_Type);
   --  Sets presence type

   function Get_Type (Self : XMPP_Presence) return Presence_Type;
   --  Returns presence type

   function Create return XMPP_Presence_Access;
   --  Returns heap allocated presence object

   function Is_Multi_Chat (Self : XMPP_Presence) return Boolean;
   --  Returns if it is a multichat presence

   procedure Set_Multi_Chat (Self : in out XMPP_Presence;
                             MUC  : XMPP.MUCS.XMPP_MUC);
   --  Sets multichat presence

   function Get_MUC (Self : XMPP_Presence) return XMPP.MUCS.XMPP_MUC;
   --  Returns multichat object, associated with presence

   --  private API, should not be used by application
   overriding function Get_Kind (Self : XMPP_Presence) return Object_Kind;

   overriding procedure Serialize
    (Self   : XMPP_Presence;
     Writer : in out XML.SAX.Pretty_Writers.XML_Pretty_Writer'Class);

   overriding
   procedure Set_Content (Self      : in out XMPP_Presence;
                          Parameter : League.Strings.Universal_String;
                          Value     : League.Strings.Universal_String);

private

   type XMPP_Presence is new XMPP.Objects.XMPP_Object with
   record
      To               : League.Strings.Universal_String;
      From             : League.Strings.Universal_String;
      Show             : Show_Kind := Online;
      Status           : League.Strings.Universal_String;
      Priority         : Priority_Type := -129;
      Type_Of_Presence : Presence_Type := Unavailable;
      MUC              : XMPP.MUCS.XMPP_MUC;
      Multi_Chat       : Boolean := False;
   end record;
end XMPP.Presences;
