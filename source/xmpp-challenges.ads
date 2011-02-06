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
--  <Unit> XMPP.Challenges
--  <ImplementationNotes>
--
------------------------------------------------------------------------------
--  $Revision$ $Author$
--  $Date$
------------------------------------------------------------------------------
with League.Strings;

with XML.SAX.Pretty_Writers;

with XMPP.Objects;

package XMPP.Challenges is

   type XMPP_Challenge is new XMPP.Objects.XMPP_Object with private;

   type XMPP_Challenge_Access is access all XMPP_Challenge'Class;

   function Create return XMPP_Challenge_Access;

   overriding
   function Get_Kind (Self : XMPP_Challenge) return XMPP.Objects.Object_Kind;

   overriding procedure Serialize
    (Self   : XMPP_Challenge;
     Writer : in out XML.SAX.Pretty_Writers.SAX_Pretty_Writer'Class);

   overriding
   procedure Set_Content (Self      : in out XMPP_Challenge;
                          Parameter : League.Strings.Universal_String;
                          Value     : League.Strings.Universal_String);

   procedure Set_Realm (Self : in out XMPP_Challenge;
                        Realm : League.Strings.Universal_String);

   procedure Set_Nonce (Self  : in out XMPP_Challenge;
                        Nonce : League.Strings.Universal_String);

   procedure Set_Qop (Self : in out XMPP_Challenge;
                      Qop  : League.Strings.Universal_String);

   procedure Set_Charset (Self    : in out XMPP_Challenge;
                          Charset : League.Strings.Universal_String);

   procedure Set_Algorithm (Self      : in out XMPP_Challenge;
                            Algorithm : League.Strings.Universal_String);

   --  data for response
   procedure Set_JID (Self : in out XMPP_Challenge;
                      JID  : League.Strings.Universal_String);

   procedure Set_Host (Self : in out XMPP_Challenge;
                       Host : League.Strings.Universal_String);

   procedure Set_Password (Self     : in out XMPP_Challenge;
                           Password : League.Strings.Universal_String);

   procedure Set_RSP_Auth (Self     : in out XMPP_Challenge;
                           RSP_Auth : League.Strings.Universal_String);

   function Generate_Response (Self : XMPP_Challenge)
      return League.Strings.Universal_String;

private

   type XMPP_Challenge is new XMPP.Objects.XMPP_Object with
   record
      Realm     : League.Strings.Universal_String;
      Nonce     : League.Strings.Universal_String;
      Qop       : League.Strings.Universal_String;
      Charset   : League.Strings.Universal_String;
      Algorithm : League.Strings.Universal_String;
      RSP_Auth  : League.Strings.Universal_String;

      --  data for response
      JID       : League.Strings.Universal_String;
      Host      : League.Strings.Universal_String;
      Password  : League.Strings.Universal_String;
   end record;

   procedure Parse_Challenge (Self      : in out XMPP_Challenge;
                              Challenge : String);

end XMPP.Challenges;
