------------------------------------------------------------------------------
--                                                                          --
--                              AXMPP Project                               --
--                                                                          --
--                           XMPP Library for Ada                           --
--                                                                          --
------------------------------------------------------------------------------
--                                                                          --
-- Copyright © 2011, Alexander Basov <coopht@gmail.com>                     --
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

   use type XMPP.Bind_State;
   use type XMPP.Session_State;

   ---------------------------
   --  Bind_Resource_State  --
   ---------------------------
   overriding procedure Bind_Resource_State
     (Self   : in out Con_Cli_Handler;
      JID    : League.Strings.Universal_String;
      Status : XMPP.Bind_State) is
   begin
      if Status = XMPP.Success then
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
      pragma Unreferenced (Self);
   begin
      Ada.Wide_Wide_Text_IO.Put_Line ("Yeah, we are connected");
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
           & XMPP.Show_Kind'Wide_Wide_Image (Data.Get_Show)
           & "(" & Data.Get_Status.To_Wide_Wide_String & ")");
   end Presence;

   ---------------------
   --  Session_State  --
   ---------------------
   overriding procedure Session_State
     (Self   : in out Con_Cli_Handler;
      Status : XMPP.Session_State) is
   begin
      if Status = XMPP.Established then
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
