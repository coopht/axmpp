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

with Con_Cli;

with XMPP.IQS;
pragma Warnings (Off, XMPP.IQS);
--  XXX : Gnat gpl 2010 bug

with XMPP.Objects;
pragma Warnings (Off, XMPP.Objects);
--  XXX : Gnat gpl 2010 bug

package body Con_Cli_Handlers is

   use XMPP.IQS;
   use XMPP.Objects;

   use type XMPP.Binds.Bind_State;
   use type XMPP.IQ_Sessions.Session_State;

   ---------------------------
   --  Bind_Resource_State  --
   ---------------------------
   overriding procedure Bind_Resource_State
     (Self   : in out Con_Cli_Handler;
      JID    : League.Strings.Universal_String;
      Status : XMPP.Binds.Bind_State) is
   begin
      if Status = XMPP.Binds.Success then
         Ada.Wide_Wide_Text_IO.Put_Line
           ("Resource Binded Success: " & JID.To_Wide_Wide_String);

         --  After resource binded successfull establishing session
         Self.Object.Establish_IQ_Session;
      end if;
   end Bind_Resource_State;

   -----------------
   --  Connected  --
   -----------------
   overriding procedure Connected
     (Self    : in out Con_Cli_Handler;
      Object  : XMPP.Stream_Features.XMPP_Stream_Feature'Class) is
      pragma Unreferenced (Object);
   begin
      Ada.Wide_Wide_Text_IO.Put_Line ("Yeah, we are connected");
      Self.Object.Bind_Resource
        (League.Strings.To_Universal_String ("con_cli_resource"));
   end Connected;

   ----------------
   --  Presence  --
   ----------------
   overriding procedure Presence
     (Self : in out Con_Cli_Handler;
      Data : XMPP.Presences.XMPP_Presence'Class) is
      pragma Unreferenced (Self);

   begin
      Ada.Wide_Wide_Text_IO.Put_Line ("Presence Arrived: ");
      Ada.Wide_Wide_Text_IO.Put_Line
        ("User "
           & Data.Get_From.To_Wide_Wide_String
           & " is "
           & XMPP.Presences.Show_Kind'Wide_Wide_Image (Data.Get_Show)
           & "(" & Data.Get_Status.To_Wide_Wide_String & ")");
   end Presence;

   ---------------------
   --  Session_State  --
   ---------------------
   overriding procedure Session_State
     (Self   : in out Con_Cli_Handler;
      Status : XMPP.IQ_Sessions.Session_State) is
   begin
      if Status = XMPP.IQ_Sessions.Established then
         Ada.Wide_Wide_Text_IO.Put_Line ("Session established !!!");

         Self.Object.Request_Roster;

         --  After session successfully established,
         --  sending presence
         Self.Set_Presence;
      end if;
   end Session_State;

   --------------------
   --  Set_Presence  --
   --------------------
   procedure Set_Presence (Self : in out Con_Cli_Handler) is
      P : XMPP.Presences.XMPP_Presence;

   begin
      Self.Object.Send_Object (P);
   end Set_Presence;

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
      Object : XMPP.Streams.XMPP_Stream'Class) is
      pragma Unreferenced (Self);
      pragma Unreferenced (Object);
   begin
      Ada.Wide_Wide_Text_IO.Put_Line ("Start_Stream called");
   end Start_Stream;

   -----------------------
   --  Stream_Features  --
   -----------------------
   overriding procedure Stream_Features
     (Self   : in out Con_Cli_Handler;
      Object : XMPP.Stream_Features.XMPP_Stream_Feature'Class) is
      pragma Unreferenced (Self);
      pragma Unreferenced (Object);
   begin
      Ada.Wide_Wide_Text_IO.Put_Line ("Stream_Features called");
   end Stream_Features;

end Con_Cli_Handlers;
