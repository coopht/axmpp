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
--  <Unit> XMPP.Stream_Features
--  <ImplementationNotes>
--
------------------------------------------------------------------------------
--  $Revision$ $Author$
--  $Date$
------------------------------------------------------------------------------
with Ada.Containers.Vectors;

with League.Strings;

with XMPP.Objects;

package XMPP.Stream_Features is

   type XMPP_Stream_Feature is new XMPP.Objects.XMPP_Object with private;

   type XMPP_Stream_Feature_Access is access all XMPP_Stream_Feature'Class;

   type Mechanism is (PLAIN, DIGEST_MD5);

   package Mechanisms_Vectors is
      new Ada.Containers.Vectors (Natural, Mechanism);

   ---------------------
   --  Add_Mechanism  --
   ---------------------
   procedure Add_Mechanism (Self  : in out XMPP_Stream_Feature;
                            Value : Wide_Wide_String);

   ----------------
   --  Get_Kind  --
   ----------------
   overriding
   function Get_Kind (Self : XMPP_Stream_Feature)
      return XMPP.Objects.Object_Kind;

   -----------------
   --  Serialize  --
   -----------------
   overriding
   function Serialize (Self : in XMPP_Stream_Feature)
      return League.Strings.Universal_String;

   -------------------
   --  Set_Has_TLS  --
   -------------------
   procedure Set_Has_TLS (Self  : in out XMPP_Stream_Feature;
                          Value : Boolean := True);

   -------------------
   --  Set_Content  --
   -------------------
   overriding
   procedure Set_Content (Self      : in out XMPP_Stream_Feature;
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
