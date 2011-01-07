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
--  <Unit> XMPP.MUC
--  <ImplementationNotes>
--
------------------------------------------------------------------------------
--  $Revision$ $Author$
--  $Date$
------------------------------------------------------------------------------
with XMPP.Discoes_Features;

package body XMPP.MUC is

   ----------------
   --  Get_Kind  --
   ----------------
   overriding function Get_Kind (Self : XMPP_MUC) return Objects.Object_Kind is
   begin
      return XMPP.Objects.MUC;
   end Get_Kind;

   -----------------
   --  Serialize  --
   -----------------
   overriding function Serialize (Self : XMPP_MUC)
     return League.Strings.Universal_String is
      X : League.Strings.Universal_String;

   begin

      return X;
   end Serialize;

   -------------------
   --  Set_Content  --
   -------------------
   overriding procedure Set_Content
     (Self      : in out XMPP_MUC;
      Parameter : League.Strings.Universal_String;
      Value     : League.Strings.Universal_String) is
   begin
      raise Program_Error with "Not yet implemented";
   end Set_Content;

   ---------------------------
   --  Set_Conf_Server_JID  --
   ---------------------------
   procedure Set_Conf_Server_JID (Self : in out XMPP_MUC;
                                  Srv  : League.Strings.Universal_String) is
   begin
      Self.Conf_Server_JID := Srv;
   end Set_Conf_Server_JID;

   ---------------------------
   --  Get_Conf_Server_JID  --
   ---------------------------
   function Get_Conf_Server_JID (Self : XMPP_MUC)
      return League.Strings.Universal_String is
   begin
      return Self.Conf_Server_JID;
   end Get_Conf_Server_JID;

   --------------------------
   --  MUC_Support_Query  --
   --------------------------
   procedure MUC_Support_Query (Self : in out XMPP_MUC) is
   begin
      Self.Disco.Add_Feature (XMPP.Discoes_Features.Protocol_Disco_Info);
   end MUC_Support_Query;

end XMPP.MUC;
