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
--  <Unit> XMPP.IQS
--  <ImplementationNotes>
--
------------------------------------------------------------------------------
--  $Revision$ $Author$
--  $Date$
------------------------------------------------------------------------------
with League.Strings;

with XML.SAX.Pretty_Writers;

with XMPP.Objects;

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

   type IQ_Kind is (Error, Get, Result, Set);

   type XMPP_IQ is new XMPP.Objects.XMPP_Object with private;

   type XMPP_IQ_Access is access all XMPP_IQ'Class;

   overriding function Get_Kind (Self : XMPP_IQ) return Objects.Object_Kind;

   overriding procedure Serialize
    (Self   : XMPP_IQ;
     Writer : in out XML.SAX.Pretty_Writers.SAX_Pretty_Writer'Class) is null;

   not overriding procedure Start_IQ
    (Self   : XMPP_IQ;
     Writer : in out XML.SAX.Pretty_Writers.SAX_Pretty_Writer'Class);

   not overriding procedure End_IQ
    (Self   : XMPP_IQ;
     Writer : in out XML.SAX.Pretty_Writers.SAX_Pretty_Writer'Class);

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
      Id         : League.Strings.Universal_String;
      Kind_Of_IQ : IQ_Kind := Result;
      To         : League.Strings.Universal_String;
      From       : League.Strings.Universal_String;
   end record;

end XMPP.IQS;
