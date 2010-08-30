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
--  <Unit> XMPP.Con_Cli_Handlers
--  <ImplementationNotes>
--
------------------------------------------------------------------------------
--  $Revision$ $Author$
--  $Date$
------------------------------------------------------------------------------
with Ada.Wide_Wide_Text_IO;

with League.Strings;

with Con_Cli;

with XMPP.Binds;
with XMPP.IQS;
with XMPP.Objects;
with XMPP.Stream_Handlers;

package body Con_Cli_Handlers is

   use XMPP.IQS;
   use XMPP.Objects;

   -----------------
   --  Connected  --
   -----------------
   overriding procedure Connected
     (Self    : in out Con_Cli_Handler;
      Object  : XMPP.Stream_Features.XMPP_Stream_Feature_Access) is

      Bind_IQ     : XMPP.IQS.XMPP_IQ (XMPP.IQS.Set);
      Bind_Object : XMPP.Binds.XMPP_Bind_Access := new XMPP.Binds.XMPP_Bind;

   begin
      Ada.Wide_Wide_Text_IO.Put_Line ("Yeah, we are connected");

      Bind_Object.Set_Resource
        (League.Strings.To_Universal_String ("con_cli_resource"));

      --  Binding resource
      Bind_IQ.Set_Id (League.Strings.To_Universal_String ("bind_1"));
      Bind_IQ.Append_Item (Bind_Object);

      Self.Object.Send_Object (Bind_IQ);
   end Connected;

   ----------
   --  IQ  --
   ----------
   overriding procedure IQ (Self : in out Con_Cli_Handler;
                            IQ   : not null XMPP.IQS.XMPP_IQ_Access) is
   begin
      if IQ.Get_IQ_Kind = XMPP.IQS.Result then
         for J in 0 .. IQ.Items_Count - 1 loop
            if IQ.Item_At (J).Get_Kind = XMPP.Objects.Bind then
               declare
                  Bind_Object : XMPP.Binds.XMPP_Bind_Access
                    := XMPP.Binds.XMPP_Bind_Access (IQ.Item_At (J));

               begin
                  Ada.Wide_Wide_Text_IO.Put_Line
                    ("Resource Binded Success: "
                       & Bind_Object.Get_JID.To_Wide_Wide_String);
               end;
            end if;
         end loop;
      end if;
   end IQ;

   --------------------------
   --  Set_Session_Object  --
   --------------------------
   procedure Set_Session_Object
     (Self   : in out Con_Cli_Handler;
      Object : not null access Con_Cli.Session'Class) is
   begin
      Self.Object := Object;
   end Set_Session_Object;

   --------------------
   --  Start_Stream  --
   --------------------
   overriding procedure Start_Stream
     (Self   : in out Con_Cli_Handler;
      Object : not null XMPP.Streams.XMPP_Stream_Access)
   is
   begin
      Ada.Wide_Wide_Text_IO.Put_Line ("Start_Stream called");
   end Start_Stream;

   -----------------------
   --  Stream_Features  --
   -----------------------
   overriding procedure Stream_Features
     (Self   : in out Con_Cli_Handler;
      Object : not null XMPP.Stream_Features.XMPP_Stream_Feature_Access)
   is
   begin
      Ada.Wide_Wide_Text_IO.Put_Line ("Stream_Features called");
   end Stream_Features;

end Con_Cli_Handlers;
