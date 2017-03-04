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
with Ada.Containers.Vectors;

with League.Strings;

with XML.SAX.Pretty_Writers;

with XMPP.Objects;

package XMPP.Stream_Features is

   type XMPP_Stream_Feature is new XMPP.Objects.XMPP_Object with private;

   type XMPP_Stream_Feature_Access is access all XMPP_Stream_Feature'Class;

   package Mechanisms_Vectors is
      new Ada.Containers.Vectors (Natural, Mechanism);

   procedure Add_Mechanism (Self  : in out XMPP_Stream_Feature;
                            Value : Wide_Wide_String);

   function Create return XMPP_Stream_Feature_Access;

   overriding function Get_Kind (Self : XMPP_Stream_Feature)
      return XMPP.Object_Kind;

   overriding procedure Serialize
    (Self   : XMPP_Stream_Feature;
     Writer : in out XML.SAX.Pretty_Writers.XML_Pretty_Writer'Class);

   procedure Set_Has_TLS (Self  : in out XMPP_Stream_Feature;
                          Value : Boolean := True);

   overriding procedure Set_Content
     (Self      : in out XMPP_Stream_Feature;
      Parameter : League.Strings.Universal_String;
      Value     : League.Strings.Universal_String);

   function Is_Bind_Supported (Self : XMPP_Stream_Feature) return Boolean;

   function Is_Session_Supported (Self : XMPP_Stream_Feature) return Boolean;

private

   type XMPP_Stream_Feature is new XMPP.Objects.XMPP_Object with
   record
      Has_TLS           : Boolean := False;
      Mechanisms        : Mechanisms_Vectors.Vector;
      Bind_Supported    : Boolean := False;
      Session_Supported : Boolean := False;
   end record;
end XMPP.Stream_Features;
