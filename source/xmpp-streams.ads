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

package XMPP.Streams is

   type XMPP_Stream is new XMPP.Objects.XMPP_Object with private;

   type XMPP_Stream_Access is access all XMPP_Stream'Class;

   function Create return XMPP_Stream_Access;

   overriding function Get_Kind (Self : XMPP_Stream) return XMPP.Object_Kind;

   overriding procedure Serialize
    (Self   : XMPP_Stream;
     Writer : in out XML.SAX.Pretty_Writers.XML_Pretty_Writer'Class);

   overriding
   procedure Set_Content (Self      : in out XMPP_Stream;
                          Parameter : League.Strings.Universal_String;
                          Value     : League.Strings.Universal_String);

   procedure Set_From (Self  : in out XMPP_Stream;
                       Value : League.Strings.Universal_String);

   procedure Set_Id (Self  : in out XMPP_Stream;
                     Value : League.Strings.Universal_String);

   procedure Set_Id (Self : in out XMPP_Stream; Value : Integer);

   procedure Set_Lang (Self  : in out XMPP_Stream;
                       Value : League.Strings.Universal_String);

   procedure Set_Version (Self  : in out XMPP_Stream;
                          Value : League.Strings.Universal_String);

private

   type XMPP_Stream is new XMPP.Objects.XMPP_Object with
   record
      Id      : League.Strings.Universal_String;
      From    : League.Strings.Universal_String;
      Lang    : League.Strings.Universal_String;
      Version : League.Strings.Universal_String;
   end record;
end XMPP.Streams;
