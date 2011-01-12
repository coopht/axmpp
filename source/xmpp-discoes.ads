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
--  <Unit> XMPP.Discoes
--  <ImplementationNotes>
--
------------------------------------------------------------------------------
--  $Revision$ $Author$
--  $Date$
------------------------------------------------------------------------------
with League.Strings;

with XMPP.Discoes_Features;
with XMPP.Discoes_Identities;
with XMPP.Objects;

package XMPP.Discoes is

   type XMPP_Disco is new XMPP.Objects.XMPP_Object with private;

   type XMPP_Disco_Access is access all XMPP_Disco'Class;

   overriding function Get_Kind (Self : XMPP_Disco)
      return Objects.Object_Kind;

   overriding function Serialize (Self : in XMPP_Disco)
      return League.Strings.Universal_String;

   overriding
   procedure Set_Content (Self      : in out XMPP_Disco;
                          Parameter : League.Strings.Universal_String;
                          Value     : League.Strings.Universal_String);

   function Create return not null XMPP_Disco_Access;

   function Get_Type (Self : XMPP_Disco) return XMPP.Discoes_Features.Feature;

   procedure Set_Type (Self : in out XMPP_Disco;
                       Val  : XMPP.Discoes_Features.Feature);

   function Get_Identities (Self : XMPP_Disco)
      return XMPP.Discoes_Identities.Identities_Vector;

   function Get_Features (Self : XMPP_Disco)
      return XMPP.Discoes_Features.Features_Vector;

   procedure Add_Identity (Self : in out XMPP_Disco;
                           Val  : XMPP.Discoes_Identities.Identity);

   procedure Add_Feature (Self : in out XMPP_Disco;
                          Val  : XMPP.Discoes_Features.Feature);

   procedure Add_Feature (Self : in out XMPP_Disco;
                          Val  : League.Strings.Universal_String);

private

   type XMPP_Disco is new XMPP.Objects.XMPP_Object with
   record
      Type_Of_Disco : XMPP.Discoes_Features.Feature;
      Identities    : XMPP.Discoes_Identities.Identities_Vector;
      Features      : XMPP.Discoes_Features.Features_Vector;
   end record;

end XMPP.Discoes;
