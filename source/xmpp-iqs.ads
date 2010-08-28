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

with XMPP.Objects;

package XMPP.IQS is

   type IQ_Kind is (Error, Get, Result, Set);

   type XMPP_IQ (Kind : IQ_Kind) is new XMPP.Objects.XMPP_Object with private;

   type XMPP_IQ_Access is access all XMPP_IQ'Class;

   overriding function Get_Kind (Self : XMPP_IQ) return Objects.Object_Kind;

   overriding function Serialize (Self : in XMPP_IQ)
      return League.Strings.Universal_String;

   overriding
   procedure Set_Content (Self      : in out XMPP_IQ;
                          Parameter : League.Strings.Universal_String;
                          Value     : League.Strings.Universal_String);

   function Get_IQ_Kind (Self : XMPP_IQ) return IQ_Kind;

   procedure Set_IQ_Kind (Self : in out XMPP_IQ; Val : IQ_Kind);

   function Get_Id (Self : XMPP_IQ) return League.Strings.Universal_String;

   procedure Set_Id (Self : in out XMPP_IQ;
                     Val  : League.Strings.Universal_String);

   procedure Set_Body (Self : in out XMPP_IQ;
                       Val  : League.Strings.Universal_String);

   function Get_Body (Self : XMPP_IQ) return League.Strings.Universal_String;

   procedure Append_Item
     (Self : in out XMPP_IQ;
      Item : not null access XMPP.Objects.XMPP_Object'Class);

   function Items_Count (Self : XMPP_IQ) return Natural;

   function Item_At (Self : XMPP_IQ; Pos : Natural)
     return not null access XMPP.Objects.XMPP_Object'Class;

private

   type XMPP_IQ (Kind : IQ_Kind) is new XMPP.Objects.XMPP_Object with
   record
      Id         : League.Strings.Universal_String;
      Kind_Of_IQ : IQ_Kind := Kind;
      IQ_Body    : League.Strings.Universal_String;
      Item_List  : XMPP.Objects.Object_Vectors.Vector;
   end record;

end XMPP.IQS;
