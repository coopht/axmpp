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

package XMPP.Versions is

   Name_Element    : constant League.Strings.Universal_String
     := League.Strings.To_Universal_String ("name");

   OS_Element      : constant League.Strings.Universal_String
     := League.Strings.To_Universal_String ("os");

   Query_Element    : constant League.Strings.Universal_String
     := League.Strings.To_Universal_String ("query");

   Version_Element : constant League.Strings.Universal_String
     := League.Strings.To_Universal_String ("version");

   Version_URI     : constant League.Strings.Universal_String
     := League.Strings.To_Universal_String ("jabber:iq:version");

   type XMPP_Version is new XMPP.IQS.XMPP_IQ with private;

   type XMPP_Version_Access is access all XMPP_Version'Class;

   --  Public API  --

   not overriding function Get_Name (Self : XMPP_Version)
      return League.Strings.Universal_String;

   --  Returns client's name

   not overriding function Get_OS (Self : XMPP_Version)
      return League.Strings.Universal_String;
   --  Returns os information

   not overriding function Get_Version (Self : XMPP_Version)
      return League.Strings.Universal_String;
   --  Returns version information

   not overriding procedure Set_Name
     (Self : in out XMPP_Version;
      Name : League.Strings.Universal_String);
   --  Sets name information

   not overriding procedure Set_OS
     (Self : in out XMPP_Version;
      OS   : League.Strings.Universal_String);
   --  Sets os information

   not overriding procedure Set_Version
     (Self    : in out XMPP_Version;
      Version : League.Strings.Universal_String);
   --  Sets version information

   function Create return XMPP_Version_Access;
   --  Returns heap allocated object.

   --  Private API
   --  Should not be used in application

   overriding
   procedure Set_Content (Self      : in out XMPP_Version;
                          Parameter : League.Strings.Universal_String;
                          Value     : League.Strings.Universal_String);

   overriding function Get_Kind (Self : XMPP_Version) return Object_Kind;

   overriding procedure Serialize
    (Self   : XMPP_Version;
     Writer : in out XML.SAX.Pretty_Writers.XML_Pretty_Writer'Class);

private

   type XMPP_Version is new XMPP.IQS.XMPP_IQ with
   record
      Name    : League.Strings.Universal_String
        := League.Strings.To_Universal_String ("ada xmpp library");
      OS      : League.Strings.Universal_String;
      Version : League.Strings.Universal_String
        := League.Strings.To_Universal_String ("0.0.1");
   end record;

end XMPP.Versions;
