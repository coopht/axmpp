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

with XMPP.Objects;
with XMPP.Utils;

package XMPP.IQS is

   IQ_Element : constant League.Strings.Universal_String
     := League.Strings.To_Universal_String ("iq");

   IQ_Type_Attribute : constant League.Strings.Universal_String
     := League.Strings.To_Universal_String ("type");

   IQ_Id_Attribute : constant League.Strings.Universal_String
     := League.Strings.To_Universal_String ("id");

   IQ_To_Attribute : constant League.Strings.Universal_String
     := League.Strings.To_Universal_String ("to");

   IQ_From_Attribute : constant League.Strings.Universal_String
     := League.Strings.To_Universal_String ("from");

   type XMPP_IQ is new XMPP.Objects.XMPP_Object with private;

   type XMPP_IQ_Access is access all XMPP_IQ'Class;

   overriding function Get_Kind (Self : XMPP_IQ) return Object_Kind;

   overriding procedure Serialize
    (Self   : XMPP_IQ;
     Writer : in out XML.SAX.Pretty_Writers.XML_Pretty_Writer'Class) is null;

   not overriding procedure Start_IQ
    (Self   : XMPP_IQ;
     Writer : in out XML.SAX.Pretty_Writers.XML_Pretty_Writer'Class);

   not overriding procedure End_IQ
    (Self   : XMPP_IQ;
     Writer : in out XML.SAX.Pretty_Writers.XML_Pretty_Writer'Class);

   overriding
   procedure Set_Content (Self      : in out XMPP_IQ;
                          Parameter : League.Strings.Universal_String;
                          Value     : League.Strings.Universal_String);

   function Get_IQ_Kind (Self : XMPP_IQ) return IQ_Kind;

   procedure Set_IQ_Kind (Self : in out XMPP_IQ; Val : IQ_Kind);

   function Get_Id (Self : XMPP_IQ) return League.Strings.Universal_String;

   procedure Set_Id (Self : in out XMPP_IQ;
                     Val  : League.Strings.Universal_String);

   procedure Set_From (Self : in out XMPP_IQ;
                       Val  : League.Strings.Universal_String);

   function Get_From (Self : XMPP_IQ) return League.Strings.Universal_String;

   procedure Set_To (Self : in out XMPP_IQ;
                     Val  : League.Strings.Universal_String);

   function Get_To (Self : XMPP_IQ) return League.Strings.Universal_String;

private

   type XMPP_IQ is new XMPP.Objects.XMPP_Object with
   record
      Id         : League.Strings.Universal_String := XMPP.Utils.Gen_Id;
      Kind_Of_IQ : IQ_Kind := Result;
      To         : League.Strings.Universal_String;
      From       : League.Strings.Universal_String;
   end record;

end XMPP.IQS;
