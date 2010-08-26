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
--  <Unit> XMPP.Presences
--  <ImplementationNotes>
--
------------------------------------------------------------------------------
--  $Revision$ $Author$
--  $Date$
------------------------------------------------------------------------------
with League.Strings;

with XMPP.Objects;

package body XMPP.Messages is

   ----------------
   --  Get_Kind  --
   ----------------
   overriding function Get_Kind (Self : XMPP_Message)
     return Objects.Object_Kind is
   begin
      return XMPP.Objects.Message;
   end Get_Kind;

   -----------------
   --  Serialize  --
   -----------------
   overriding function Serialize (Self : in XMPP_Message)
      return League.Strings.Universal_String is
   begin
      return X : League.Strings.Universal_String;
   end Serialize;

   -------------------
   --  Set_Content  --
   -------------------
   overriding
   procedure Set_Content (Self      : in out XMPP_Message;
                          Parameter : League.Strings.Universal_String;
                          Value     : League.Strings.Universal_String) is
   begin
      raise Program_Error with "Not yet implemented";
   end Set_Content;

   ----------------
   --  Set_Type  --
   ----------------
   procedure Set_Type (Self : in out XMPP_Message; T : Message_Type) is
   begin
      Self.Type_Of_Message := T;
   end Set_Type;

   ----------------
   --  Get_Type  --
   ----------------
   function Get_Type (Self : XMPP_Message) return Message_Type is
   begin
      return Self.Type_Of_Message;
   end Get_Type;

   -------------------
   --  Set_Subject  --
   -------------------
   procedure Set_Subject (Self : in out XMPP_Message;
                          Subj : League.Strings.Universal_String) is
   begin
      Self.Subject := Subj;
   end Set_Subject;
   -------------------
   --  Get_Subject  --
   -------------------
   function Get_Subject (Self : XMPP_Message)
      return League.Strings.Universal_String is
   begin
      return Self.Subject;
   end Get_Subject;

   ----------------
   --  Set_Body  --
   ----------------
   procedure Set_Body (Self : in out XMPP_Message;
                       Val  : League.Strings.Universal_String) is
   begin
      Self.Message_Body := Val;
   end Set_Body;

   ----------------
   --  Get_Body  --
   ----------------
   function Get_Body (Self : XMPP_Message)
      return League.Strings.Universal_String is
   begin
      return Self.Message_Body;
   end Get_Body;

   ------------------
   --  Set_Thread  --
   ------------------
   procedure Set_Thread (Self : in out XMPP_Message;
                         Val : League.Strings.Universal_String) is
   begin
      Self.Thread := Val;
   end Set_Thread;

   ------------------
   --  Get_Thread  --
   ------------------
   function Get_Thread (Self : XMPP_Message)
      return League.Strings.Universal_String is
   begin
      return Self.Thread;
   end Get_Thread;

end XMPP.Messages;

