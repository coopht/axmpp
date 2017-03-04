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
with XMPP.IQS;

with League.Strings;
with XML.SAX.Pretty_Writers;

package XMPP.Binds is

   Bind_Element : constant League.Strings.Universal_String
     := League.Strings.To_Universal_String ("bind");

   Bind_URI : constant League.Strings.Universal_String
     := League.Strings.To_Universal_String
         ("urn:ietf:params:xml:ns:xmpp-bind");

   Resource_Element : constant League.Strings.Universal_String
     := League.Strings.To_Universal_String ("resource");

   type XMPP_Bind is new XMPP.IQS.XMPP_IQ with private;

   type XMPP_Bind_Access is access all XMPP_Bind'Class;

   function Create return not null XMPP_Bind_Access;
   --  returns heap allocated object

   procedure Set_Resource (Self : in out XMPP_Bind;
                           Res  : League.Strings.Universal_String);
   --  Sets resource

   procedure Set_JID (Self : in out XMPP_Bind;
                      JID  : League.Strings.Universal_String);
   --  Sets JID

   function Get_Resource (Self : XMPP_Bind)
      return League.Strings.Universal_String;
   --  Returns resource

   function Get_JID (Self : XMPP_Bind) return League.Strings.Universal_String;
   --  Returns JID

   --  Private API, should not be used by application
   overriding function Get_Kind (Self : XMPP_Bind) return Object_Kind;

   overriding procedure Serialize
    (Self   : XMPP_Bind;
     Writer : in out XML.SAX.Pretty_Writers.XML_Pretty_Writer'Class);

   overriding
   procedure Set_Content (Self      : in out XMPP_Bind;
                          Parameter : League.Strings.Universal_String;
                          Value     : League.Strings.Universal_String);

private

   type XMPP_Bind is new XMPP.IQS.XMPP_IQ with
   record
      JID      : League.Strings.Universal_String;
      Resource : League.Strings.Universal_String;
   end record;

end XMPP.Binds;
