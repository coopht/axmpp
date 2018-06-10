------------------------------------------------------------------------------
--                                                                          --
--                              AXMPP Project                               --
--                                                                          --
--                           XMPP Library for Ada                           --
--                                                                          --
------------------------------------------------------------------------------
--                                                                          --
-- Copyright © 2011-2018, Alexander Basov <coopht@gmail.com>                --
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
with Ada.Characters.Conversions;
with Ada.Exceptions;

with League.Text_Codecs;

with XML.SAX.Readers;

with XMPP.Binds;
with XMPP.Services;
with XMPP.Services_Identities;
with XMPP.IQ_Sessions;
with XMPP.Logger;
with XMPP.Messages;
with XMPP.MUCS;
with XMPP.Presences;
with XMPP.Roster_Items;
with XMPP.Rosters;
with XMPP.Streams;
with XMPP.Stream_Features;
with XMPP.Utils;
with XMPP.Versions;

package body XMPP.Sessions is

   use League.Strings;

   use XMPP.Logger;
   use XMPP.Objects;
   use XMPP.Objects.Object_Vectors;

   function "+" (Item : Wide_Wide_String) return Universal_String
     renames League.Strings.To_Universal_String;

   ---------------------
   --  Bind_Resource  --
   ---------------------
   procedure Bind_Resource (Self        : not null access XMPP_Session;
                            Resource_Id : League.Strings.Universal_String
                              := League.Strings.Empty_Universal_String) is

      Bind_Object : XMPP.Binds.XMPP_Bind;

   begin
      Bind_Object.Set_To (Self.Host);
      Bind_Object.Set_From (Self.JID);
      Bind_Object.Set_IQ_Kind (XMPP.Set);

      --  overwriting default resource name here.
      if not Resource_Id.Is_Empty then
         Self.Resource_Id := Resource_Id;
      end if;

      Bind_Object.Set_Resource (Self.Resource_Id);

      Self.Send_Object (Bind_Object);
   end Bind_Resource;

   ------------------
   --  Characters  --
   ------------------
   overriding procedure Characters
     (Self    : in out XMPP_Session;
      Text    : League.Strings.Universal_String;
      Success : in out Boolean)
   is
      pragma Unreferenced (Success);
   begin
      --  Log ("*** Text = [" & Text & "]");
      --  Log (Self.Tag);
      Self.Text.Append (Text);
   end Characters;

   -------------
   --  Close  --
   -------------
   procedure Close (Self : in out XMPP_Session) is
      Close_Stream : constant Universal_String
        := +"</stream:stream>";

   begin
      --  Log (" !!! Closing Stream !!!");
      --  Sending close stream stanza
      Self.Network.Send
        (XMPP.Utils.To_Stream_Element_Array
           (Ada.Characters.Conversions.To_String
              (Close_Stream.To_Wide_Wide_String)));
   end Close;

   ------------------
   --  Disconnect  --
   ------------------
   procedure Disconnect (Self : in out XMPP_Session) is
      function "+" (Item : String) return Wide_Wide_String
        renames Ada.Characters.Conversions.To_Wide_Wide_String;
   begin
      Self.Idle_Task.Stop;
   exception
      when E : others =>
         Log  (+Ada.Exceptions.Exception_Information (E));
   end Disconnect;

   ----------------------------
   --  Discover_Information  --
   ----------------------------
   procedure Discover_Information (Self : in out XMPP_Session;
                                   JID  : League.Strings.Universal_String) is
      D : XMPP.Services.XMPP_Service;

   begin
      D.Set_IQ_Kind (XMPP.Get);
      D.Set_Type (XMPP.Protocol_Disco_Info);
      D.Set_From (Self.JID);
      D.Set_To (JID);

      Self.Send_Object (D);
   end Discover_Information;

   ----------------------
   --  Discover_Items  --
   ----------------------
   procedure Discover_Items (Self : in out XMPP_Session;
                             JID  : League.Strings.Universal_String) is
      D : XMPP.Services.XMPP_Service;

   begin
      D.Set_IQ_Kind (XMPP.Get);
      D.Set_Type (XMPP.Protocol_Disco_Items);
      D.Set_From (Self.JID);
      D.Set_To (JID);

      --  XXX: removed hardcoded ID;
      --  IQ.Append_Item (D);
      Self.Send_Object (D);
   end Discover_Items;

   -------------------
   --  End_Element  --
   -------------------
   overriding procedure End_Element
     (Self           : in out XMPP_Session;
      Namespace_URI  : League.Strings.Universal_String;
      Local_Name     : League.Strings.Universal_String;
      Qualified_Name : League.Strings.Universal_String;
      Success        : in out Boolean)
   is
      Current : constant XMPP.Objects.Object_Vectors.Cursor
        := Self.Stack.Last;

      pragma Unreferenced (Qualified_Name);

   begin
      if not Self.Tag.Is_Empty and not Self.Text.Is_Empty then
         if not Self.Stack.Is_Empty then
            Self.Stack.Last_Element.Set_Content (Self.Tag, Self.Text);
         end if;
         Self.Tag.Clear;
         Self.Text.Clear;
      end if;

      --  Log ("<<< End_Element_QN = " & Qualified_Name);
      --  Log ("<<< End_Element_URI = " & Namespace_URI);

      --  Log ("Stack size at begin : "
      --        & Integer'Wide_Wide_Image (Integer (Self.Stack.Length)));

      if Namespace_URI = +"http://etherx.jabber.org/streams" then
         if Local_Name = +"stream" then

            --  TODO:
            --  Free (Self.Current);
            Self.Stack.Delete_Last;

            delay 1.0;
            Self.Disconnect;

         elsif Local_Name = +"features" then
            if not Self.Authenticated then
               Self.Stream_Handler.Stream_Features
                (XMPP.Stream_Features.XMPP_Stream_Feature_Access
                  (Self.Stack.Last_Element).all);

            else
               Self.Bind_Resource;
               Self.Stream_Handler.Connected
                 (XMPP.Stream_Features.XMPP_Stream_Feature_Access
                    (Self.Stack.Last_Element).all);
            end if;

            --  if tls session was not established, than send command to server
            --  to start tls negotiation
            --  XXX: may be XMPP object for xmpp-tls name
            --  space should be created
            if not Self.Network.Is_TLS_Established then
               Log ("Sending starttls");
               Self.Send_Wide_Wide_String
                 ("<starttls xmlns='urn:ietf:params:xml:ns:xmpp-tls'/>");

            elsif not Self.Authenticated then
               Log ("Starting sasl auth");
               Self.Send_Wide_Wide_String
                 ("<auth xmlns='urn:ietf:params:xml:ns:xmpp-sasl' "
                    & "mechanism='DIGEST-MD5'/>");
            end if;

            Self.Stack.Delete_Last;
            --  TODO:
            --  Free (Self.Current);
         end if;

      elsif Namespace_URI = +"urn:ietf:params:xml:ns:xmpp-tls"
        and Local_Name = +"proceed"
      then
         if not Self.Network.Is_TLS_Established then
            Self.Proceed_TLS_Auth;
            --  Stop XML parsing immediately to avoid XML reading from TLS
            --  handshake dialog
            Success := False;
         end if;

      --  Proceed with SASL Authentication
      elsif Namespace_URI = +"urn:ietf:params:xml:ns:xmpp-sasl" then
         if Local_Name = +"challenge" then
            --  Proceeding with SASL auth
            Self.Proceed_SASL_Auth
              (XMPP.Challenges.XMPP_Challenge_Access
                 (Self.Stack.Last_Element));

            Self.Stack.Delete_Last;
            --  TODO:
            --  Free (Self.Current);
            return;

            --  For successfull authentication

         elsif Local_Name = +"success" then

            --  TODO:
            --  Free (Self.Current);

            --  We do not create any object here, just notifies user about
            --  successful authentification, and opening another one stream
            --  to server

            Self.Stack.Delete_Last;
            Log ("Authentification successfull !!");
            Self.Authenticated := True;
            Self.Open_Stream;
         end if;

      elsif Namespace_URI = +"jabber:iq:roster" then

         --  Adding item to the roster
         if Local_Name = +"item" then
            if Self.Stack.Last_Element.Get_Kind = Roster_Item then
               if Previous (Current) /= No_Element then
                  if Self.Stack.Element
                    (To_Index (Previous (Current))).Get_Kind = Roster
                  then
                     XMPP.Rosters.XMPP_Roster_Access
                       (Self.Stack.Element (To_Index (Previous (Current))))
                       .Append_Item (Roster_Items.XMPP_Roster_Item_Access
                                      (Self.Stack.Last_Element));

                     Self.Stack.Delete_Last;
                  end if;
               end if;
            end if;
         end if;

      --  Service discovery
      elsif Namespace_URI = +"http://jabber.org/protocol/disco#info" or
        Namespace_URI = +"http://jabber.org/protocol/disco#items"
      then

         --  Nothing to process here

         null;

      elsif Namespace_URI = +"http://jabber.org/protocol/muc#user" then
         if Local_Name = +"x" then
            if Self.Stack.Last_Element.Get_Kind = MUC then
               if Previous (Current) /= No_Element then
                  if Self.Stack.Element
                    (To_Index (Previous (Current))).Get_Kind = Presence
                  then
                     declare
                        M : constant access XMPP.MUCS.XMPP_MUC
                          := XMPP.MUCS.XMPP_MUC_Access
                          (Self.Stack.Last_Element);

                        P : constant XMPP.Presences.XMPP_Presence_Access
                          := XMPP.Presences.XMPP_Presence_Access
                              (Self.Stack.Element
                                (To_Index (Previous (Current))));
                     begin
                        P.Set_Multi_Chat (M.all);
                     end;

                     Self.Stack.Delete_Last;
                  end if;
               end if;
            end if;
         end if;

      elsif Namespace_URI = +"jabber:client" then
         --  Calling IQ Handler

         if Local_Name = +"iq" then

            --  Log ("Self.Stack.Last_Element.Get_Kind "
            --        & XMPP.Objects.Object_Kind'Wide_Wide_Image
            --           (Self.Stack.Last_Element.Get_Kind));

            if Self.Stack.Is_Empty then
               Self.Process_IQ (null);
            else
               Self.Process_IQ (Self.Stack.Last_Element);
               Self.Stack.Delete_Last;
            end if;

         --  Calling Presence Handler
         elsif Local_Name = +"presence" then
            Self.Stream_Handler.Presence
              (XMPP.Presences.XMPP_Presence_Access
                (Self.Stack.Last_Element).all);
            Self.Stack.Delete_Last;

         --  Calling Message Handler
         elsif Local_Name = +"message" then
            Self.Stream_Handler.Message
              (XMPP.Messages.XMPP_Message_Access
                (Self.Stack.Last_Element).all);
            Self.Stack.Delete_Last;
         end if;

      --  All objects of chatsets namespaces processed in start_element
      elsif Namespace_URI = +"http://jabber.org/protocol/chatstates" then
         null;
      end if;

      --  Log ("Stack size at end : "
      --        & Integer'Wide_Wide_Image (Integer (Self.Stack.Length)));
      --  Log ("Stack size : "
      --        & Integer'Wide_Wide_Image (Integer (Self.Stack.Length)));
   end End_Element;

   --------------------
   --  Error_String  --
   --------------------
   overriding function Error_String (Self : XMPP_Session)
                 return League.Strings.Universal_String
   is
      pragma Unreferenced (Self);

   begin
      return X : League.Strings.Universal_String;
   end Error_String;

   ----------------------------
   --  Establish_IQ_Session  --
   ----------------------------
   procedure Establish_IQ_Session (Self : not null access XMPP_Session) is
      S  : XMPP.IQ_Sessions.XMPP_IQ_Session;

   begin
      S.Set_To (Self.Host);
      S.Set_From (Self.JID);
      S.Set_IQ_Kind (XMPP.Set);
      Self.Send_Object (S);
   end Establish_IQ_Session;

   -------------------
   --  Fatal_Error  --
   -------------------
   overriding procedure Fatal_Error
     (Self       : in out XMPP_Session;
      Occurrence : XML.SAX.Parse_Exceptions.SAX_Parse_Exception) is

   begin
      Log ("Fatal_Error: " & Occurrence.Message);
      Log ("         Line   : "
             & Natural'Wide_Wide_Image (Occurrence.Line));

      Log ("         Column : "
             & Natural'Wide_Wide_Image (Occurrence.Column));

      Log ("Locator ("
             & Natural'Wide_Wide_Image (Self.Locator.Line)
             & ":"
             & Natural'Wide_Wide_Image (Self.Locator.Column)
             & ")");
   end Fatal_Error;

   -----------------
   --  Is_Opened  --
   -----------------
   function Is_Opened (Self : XMPP_Session) return Boolean is
   begin
      return Self.Session_Opened;
   end Is_Opened;

   ----------------------------
   --  Join_Multi_User_Chat  --
   ----------------------------

   procedure Join_Multi_User_Chat
    (Self      : in out XMPP_Session;
     Room      : League.Strings.Universal_String;
     Server    : League.Strings.Universal_String;
     Nick_Name : League.Strings.Universal_String) is
      P : XMPP.Presences.XMPP_Presence;
      M : XMPP.MUCS.XMPP_MUC;

   begin
      M.Set_History ((Max_Chars => (Is_Set => True, Value => 0)));
      P.Set_From (Self.JID);
      P.Set_To (Room & "@" & Server & "/" & Nick_Name);
      P.Set_Multi_Chat (M);
      Self.Send_Object (P);
   end Join_Multi_User_Chat;

   ------------------
   --  On_Connect  --
   ------------------
   overriding procedure On_Connect (Self : not null access XMPP_Session) is
   begin
      --  After we connected, initialize parser.

      Log ("On_Connect!");
      Self.Open_Stream;
   end On_Connect;

   ---------------------
   --  On_Disconnect  --
   ---------------------
   overriding procedure On_Disconnect (Self : not null access XMPP_Session) is
   begin
      Log ("Disconnected");
      Self.Stream_Handler.Disconnected;
   end On_Disconnect;

   ------------
   --  Open  --
   ------------
   procedure Open (Self : not null access XMPP_Session) is
   begin
      if not Self.Is_Opened then
         Log ("Connecting");
         Self.Network.Connect
           (Self.Host.To_UTF_8_String, Self.Port);
         Log ("Starting idle");
         Self.Idle_Task.Start (Self.Network'Unchecked_Access);
      end if;
   end Open;

   -------------------
   --  Open_Stream  --
   -------------------
   procedure Open_Stream (Self : not null access XMPP_Session) is
      --  TODO:
      --       Use appropriate object, instead of raw xml
      Open_Stream : constant Universal_String
        := "<stream:stream "
             & "xmlns:stream='http://etherx.jabber.org/streams' "
             & "version='1.0' "
             & "xmlns='jabber:client' "
             & "to='"
             & Self.Host
             & "' >";

   begin
      --  Log (" !!! Opening_Stream !!!");
      --  We need to reset parser each time we start new xml stream
      Self.Reader.Set_Content_Handler
        (XML.SAX.Readers.SAX_Content_Handler_Access (Self));
      Self.Reader.Set_Input_Source (Self.Network.Get_Source);

      --  Sending open stream stanza
      Self.Network.Send
        (XMPP.Utils.To_Stream_Element_Array
           (Ada.Characters.Conversions.To_String
              (Open_Stream.To_Wide_Wide_String)));
   end Open_Stream;

   -------------------------
   --  Proceed_SASL_Auth  --
   -------------------------
   procedure Proceed_SASL_Auth
     (Self   : not null access XMPP_Session;
      Object : not null XMPP.Challenges.XMPP_Challenge_Access) is
   begin
      Object.Set_JID (Self.JID);
      Object.Set_Password (Self.Password);
      Self.Send_Wide_Wide_String
        (Object.Generate_Response.To_Wide_Wide_String);
   end Proceed_SASL_Auth;

   ------------------------
   --  Proceed_TLS_Auth  --
   ------------------------
   procedure Proceed_TLS_Auth (Self : not null access XMPP_Session) is
   begin
      Self.Network.Start_Handshake;

      --  Now reset XML parset to begin read TLS stream
      Self.Reader.Set_Content_Handler
        (XML.SAX.Readers.SAX_Content_Handler_Access (Self));
      Self.Reader.Set_Input_Source (Self.Network.Get_Source);

      --  On Success handshake,
      --  we should reopen stream via TLS session.

      --  Log ("TLS Session established. Sending Stream");
      --  Self.On_Connect;
   exception
      when E : others =>
         Log (Ada.Characters.Conversions.To_Wide_Wide_String
                (Ada.Exceptions.Exception_Information (E)));

         Self.Disconnect;
   end Proceed_TLS_Auth;

   ------------------
   --  Process_IQ  --
   ------------------
   procedure Process_IQ (Self : in out XMPP_Session;
                         IQ   : XMPP.Objects.XMPP_Object_Access) is
   begin
      --  Log ("XMPP.Session.Process_IQ : IQ arrived : "
      --         & XMPP.IQS.IQ_Kind'Wide_Wide_Image (IQ.Get_IQ_Kind));

      --  Setting_IQ_Header (to, from, type, id attrs)

      Log ("Process_IQ:");
      Log (Self.IQ_Header.Get_To);
      Log (Self.IQ_Header.Get_From);
      Log (Self.IQ_Header.Get_Id);
      Log (XMPP.IQ_Kind'Wide_Wide_Image (Self.IQ_Header.Get_IQ_Kind));

      if IQ = null then
         --  Response to session iq has no nested element
         Self.Stream_Handler.Session_State (Established);

      else
         XMPP.IQS.XMPP_IQ_Access (IQ).Set_To (Self.IQ_Header.Get_To);
         XMPP.IQS.XMPP_IQ_Access (IQ).Set_From (Self.IQ_Header.Get_From);
         XMPP.IQS.XMPP_IQ_Access (IQ).Set_Id (Self.IQ_Header.Get_Id);
         XMPP.IQS.XMPP_IQ_Access (IQ).Set_IQ_Kind (Self.IQ_Header.Get_IQ_Kind);

         case IQ.Get_Kind is

            --  Resource Binded
            when Bind =>
               Self.Stream_Handler.Bind_Resource_State
                 (XMPP.Binds.XMPP_Bind_Access (IQ).Get_JID, Success);

               --  Session established
            when IQ_Session =>
               Self.Stream_Handler.Session_State (Established);

               --  Roster arrived
            when Roster =>
               Self.Stream_Handler.Roster
                 (XMPP.Rosters.XMPP_Roster_Access (IQ).all);

               --  Discover information arrived
            when Disco =>
               Self.Stream_Handler.Service_Information
                 (XMPP.Services.XMPP_Service_Access (IQ).all);

               --  Software version information arrived
            when Version =>

               case XMPP.IQS.XMPP_IQ_Access (IQ).Get_IQ_Kind is
                  when XMPP.Result =>
                     Self.Stream_Handler.Version
                       (XMPP.Versions.XMPP_Version_Access (IQ).all);

                  when XMPP.Get =>
                     Self.Stream_Handler.Version_Request
                       (XMPP.Versions.XMPP_Version_Access (IQ).all);

                  when others =>
                     null;
               end case;

            when others =>
               null;
         end case;
      end if;

      Self.IQ_Header.Set_To (+"");
      Self.IQ_Header.Set_From (+"");
      Self.IQ_Header.Set_Id (+"");
      Self.IQ_Header.Set_IQ_Kind (XMPP.Get);
   end Process_IQ;

   -----------------
   --  Read_Data  --
   -----------------
   overriding function Read_Data (Self   : not null access XMPP_Session)
     return Boolean is
   begin
      Self.Reader.Parse;
      return True;
   end Read_Data;

   ----------------------
   --  Request_Roster  --
   ----------------------
   procedure Request_Roster (Self : not null access XMPP_Session) is
      R : XMPP.Rosters.XMPP_Roster;

   begin
      R.Set_From (Self.JID);
      R.Set_IQ_Kind (XMPP.Get);
      Self.Send_Object (R);
   end Request_Roster;

   -----------------------
   --  Request_Version  --
   -----------------------
   procedure Request_Version
    (Self        : not null access XMPP_Session;
     XMPP_Entity : League.Strings.Universal_String) is

      Ver : XMPP.Versions.XMPP_Version;

   begin
      Ver.Set_To (XMPP_Entity);
      Ver.Set_From (Self.JID);
      Ver.Set_IQ_Kind (XMPP.Get);

      Self.Send_Object (Ver);
   end Request_Version;

   -------------------
   --  Send_Object  --
   -------------------
   procedure Send_Object (Self   : not null access XMPP_Session;
                          Object : XMPP.Objects.XMPP_Object'Class) is

      UTF_8_Codec : constant League.Text_Codecs.Text_Codec
        := League.Text_Codecs.Codec (+"UTF-8");

   begin
      Self.Writer.Set_Output_Destination (Self.Output'Unchecked_Access);
      Object.Serialize (Self.Writer);

      Log ("Sending Data : " & Self.Output.Get_Text);

      Self.Network.Send
        (UTF_8_Codec.Encode (Self.Output.Get_Text).To_Stream_Element_Array);

       --  Self.Send_Wide_Wide_String (Self.Writer.Text.To_Wide_Wide_String);

      Self.Output.Clear;

      Log ("Data Sent");
   end Send_Object;

   -----------------------------
   --  Send_Wide_Wide_String  --
   -----------------------------
   procedure Send_Wide_Wide_String (Self : in out XMPP_Session;
                                    Str  : Wide_Wide_String) is
   begin
      --  DEBUG
      Log ("Sending XML : " & Str);

      Self.Network.Send
        (XMPP.Utils.To_Stream_Element_Array
           (Ada.Characters.Conversions.To_String (Str)));
   end Send_Wide_Wide_String;

   ----------------
   --  Set_Host  --
   ----------------
   procedure Set_Host (Self : not null access XMPP_Session;
                       Host : League.Strings.Universal_String) is
   begin
      Self.Host := Host;
   end Set_Host;

   ---------------
   --  Set_JID  --
   ---------------
   procedure Set_JID (Self : in out XMPP_Session;
                      JID  : League.Strings.Universal_String) is
   begin
      Self.JID := JID;
   end Set_JID;

   --------------------
   --  Set_Password  --
   --------------------
   procedure Set_Password (Self     : in out XMPP_Session;
                           Password : League.Strings.Universal_String) is
   begin
      Self.Password := Password;
   end Set_Password;

   ----------------
   --  Set_Port  --
   ----------------
   procedure Set_Port (Self : not null access XMPP_Session;
                       Port : Natural) is
   begin
      Self.Port := Port;
   end Set_Port;

   ------------------
   -- Set_Resource --
   ------------------
   procedure Set_Resource (Self        : not null access XMPP_Session;
                           Resource_Id : League.Strings.Universal_String)
   is
   begin
      Self.Resource_Id := Resource_Id;
   end Set_Resource;

   --------------------------
   --  Set_Stream_Handler  --
   --------------------------
   procedure Set_Stream_Handler
    (Self    : not null access XMPP_Session;
     Handler : not null access XMPP.Stream_Handlers.XMPP_Stream_Handler'Class)
   is
   begin
      Self.Stream_Handler
        := XMPP.Stream_Handlers.XMPP_Stream_Handler_Access (Handler);
   end Set_Stream_Handler;

   --  XML SAX Parser implementation  --

   ---------------------
   --  Start_Element  --
   ---------------------
   overriding procedure Start_Element
     (Self           : in out XMPP_Session;
      Namespace_URI  : League.Strings.Universal_String;
      Local_Name     : League.Strings.Universal_String;
      Qualified_Name : League.Strings.Universal_String;
      Attributes     : XML.SAX.Attributes.SAX_Attributes;
      Success        : in out Boolean)
   is
      pragma Unreferenced (Qualified_Name);
      pragma Unreferenced (Success);
   begin
      --  DEBUG  --
      --  Log (">>> Start_Element_QN = "
      --        & Qualified_Name.To_Wide_Wide_String & " (");

      --  Log ("Namespace_URI = " & Namespace_URI.To_Wide_Wide_String & " ");

      --  for J in 1 .. Attributes.Length loop
      --     Log (Attributes.Local_Name (J).To_Wide_Wide_String
      --           & "="
      --           & Attributes.Value (J).To_Wide_Wide_String
      --           & " ");
      --  end loop;

      --  Log (")");

      --  DEBUG  --

      Self.Tag := Local_Name;

      --  For XMPP_Stream_Feature
      if Namespace_URI = +"urn:ietf:params:xml:ns:xmpp-tls" then
         if Local_Name = +"starttls" then
            Self.Stack.Last_Element.Set_Content (Local_Name, Local_Name);

            --  proceed tls connection establishment
            --  nothing todo here, just send required data to server in
            --  End_Element callback
         elsif Namespace_URI = +"urn:ietf:params:xml:ns:xmpp-tls"
           and Local_Name = +"proceed"
         then
            null;
         end if;

         --  For XMPP_Stream_Feature
      elsif Namespace_URI = +"urn:ietf:params:xml:ns:xmpp-sasl" then
         if Local_Name = +"mechanisms" then
            --  we can safety skip mechanisms tag here.
            null;

            --  For XMPP_Stream_Feature
         elsif Local_Name = +"mechanism" then
               --  We add mechanism parameter in Characters procedure
            Self.Tag := Local_Name;

         --  Creating Challenge object for sasl authentication
         elsif Local_Name = +"challenge" then
            Self.Tag := Local_Name;

            Self.Stack.Append
              (XMPP.Objects.XMPP_Object_Access (XMPP.Challenges.Create));

         --  For successfull authentication
         elsif Local_Name = +"success" then
         --  all work is don in delete_object
            null;
         end if;

      --  For XMPP_Stream_Feature
      elsif Namespace_URI = XMPP.Binds.Bind_URI then
         if Local_Name = XMPP.Binds.Bind_Element then

            --  If bind for iq object, than create new bind object
            --  and push it into stack

            if Self.Stack.Is_Empty then
               Self.Stack.Append
                 (XMPP.Objects.XMPP_Object_Access (XMPP.Binds.Create));

            else
               if Self.Stack.Last_Element.Get_Kind = Stream_Featuress then
                  --  Setting bind feature to stream feature object
                  Self.Stack.Last_Element.Set_Content (Local_Name, Local_Name);

               else
                  Self.Stack.Append
                    (XMPP.Objects.XMPP_Object_Access (XMPP.Binds.Create));
               end if;
            end if;

         --  setting jid value for bind object
         --  the data actual set in character procedure
         elsif Local_Name = +"jid" then
            Self.Tag := Local_Name;
         end if;

      --  For XMPP_Stream_Feature
      elsif Namespace_URI = +"urn:ietf:params:xml:ns:xmpp-session"
        and Local_Name = +"session"
      then
         --  Setting session feature to stream feature object
         if Self.Stack.Is_Empty then
            Self.Stack.Append
              (XMPP.Objects.XMPP_Object_Access (XMPP.IQ_Sessions.Create));

         elsif Self.Stack.Last_Element.Get_Kind = Stream_Featuress then

           Self.Stack.Last_Element.Set_Content (Local_Name, Local_Name);

         else
            Self.Stack.Append
              (XMPP.Objects.XMPP_Object_Access (XMPP.IQ_Sessions.Create));
         end if;

      elsif Namespace_URI = +"http://etherx.jabber.org/streams" then
         if Local_Name = +"stream" then
            Self.Stack.Append
              (XMPP.Objects.XMPP_Object_Access (XMPP.Streams.Create));

         elsif Local_Name = +"features" then
            Self.Stack.Append
              (XMPP.Objects.XMPP_Object_Access
                 (XMPP.Stream_Features.Create));
         end if;

      elsif Namespace_URI = +"jabber:client" then

         --  Creating IQ object
         if Local_Name = +"iq" then
         --  setting up object's attributes
            for J in 1 .. Attributes.Length loop
               Self.IQ_Header.Set_Content
                (Attributes.Local_Name (J), Attributes.Value (J));
            end loop;

         --  Creating Presence object
         elsif Local_Name = +"presence" then
            Self.Stack.Append
             (XMPP.Objects.XMPP_Object_Access (XMPP.Presences.Create));

         --  Creating message
         elsif Local_Name = +"message" then
            Self.Stack.Append
             (XMPP.Objects.XMPP_Object_Access (XMPP.Messages.Create));
         end if;

      elsif Namespace_URI
        = +"http://jabber.org/protocol/chatstates"
      then
         if Local_Name = +"active"
           or Local_Name = +"inactive"
           or Local_Name = +"gone"
           or Local_Name = +"composing"
           or Local_Name = +"paused"
         then
            if Self.Stack.Last_Element.Get_Kind = Message then
               Self.Stack.Last_Element.Set_Content (Local_Name, Local_Name);
            end if;
         end if;

      --  working with roster
      elsif Namespace_URI = XMPP.Rosters.Roster_URI then
         --  if query found within jabber:iq:roster namespace, then
         --  creating a roster list.
         if Local_Name = XMPP.Rosters.Query_Element then

            Self.Stack.Append
              (XMPP.Objects.XMPP_Object_Access (XMPP.Rosters.Create));

         --  if item found within jabber:iq:roster namespace, then
         --  creating a roster item.
         elsif Local_Name = XMPP.Roster_Items.Item_Element then
            if Self.Stack.Last_Element.Get_Kind = Roster then
               Self.Stack.Append
                 (XMPP.Objects.XMPP_Object_Access (XMPP.Roster_Items.Create));
            end if;

         --  nothing to do here for group tag
         elsif Local_Name = XMPP.Roster_Items.Group_Element then
            null;
         end if;

      elsif Namespace_URI = +"http://jabber.org/protocol/disco#info" then
         if Local_Name = +"query" then
            Self.Stack.Append
              (XMPP.Objects.XMPP_Object_Access (XMPP.Services.Create));

         elsif Local_Name = +"identity" then
            if Self.Stack.Last_Element.Get_Kind = Disco then
               XMPP.Services.XMPP_Service_Access
                 (Self.Stack.Last_Element).Add_Identity
                  (XMPP.Services_Identities.Create
                     (Attributes.Value (1), Attributes.Value (2)));
               return;
            end if;

         elsif Local_Name = +"feature" then
            if Self.Stack.Last_Element.Get_Kind = Disco then
               XMPP.Services.XMPP_Service_Access
                 (Self.Stack.Last_Element).Add_Feature (Attributes.Value (1));
               return;
            end if;
         end if;

      elsif Namespace_URI = +"http://jabber.org/protocol/disco#items" then
         if Local_Name = +"query" then
            Self.Stack.Append
              (XMPP.Objects.XMPP_Object_Access (XMPP.Services.Create));

         elsif Local_Name = +"item" then
            if Self.Stack.Last_Element.Get_Kind = Disco then
               XMPP.Services.XMPP_Service_Access
                 (Self.Stack.Last_Element).Add_Item
                 ((JID => Attributes.Value (+"jid"),
                   Name => Attributes.Value (+"name"),
                   Node => Attributes.Value (+"node")));
               return;
            end if;
         end if;

      elsif Namespace_URI = +"http://jabber.org/protocol/muc#user" then
         if Local_Name = +"x" then
            if Self.Stack.Last_Element.Get_Kind = Presence then
               Self.Stack.Append
                (XMPP.Objects.XMPP_Object_Access (XMPP.MUCS.Create));
            end if;
         elsif Local_Name = +"item" then
            if Self.Stack.Last_Element.Get_Kind = MUC then
               declare
                  Item       : XMPP.MUCS.MUC_Item;
                  Affilation : constant Universal_String
                    := Attributes.Value (+"affilation");

                  Role       : constant Universal_String
                    := Attributes.Value (+"role");

               begin
                  --  Setting affilation
                  if Affilation = +"admin" then
                     Item.Affilation := Admin;
                  elsif Affilation = +"member" then
                     Item.Affilation := Member;
                  elsif Affilation = +"none" then
                     Item.Affilation := None;
                  elsif Affilation = +"outcast" then
                     Item.Affilation := Outcast;
                  elsif Affilation = +"owner" then
                     Item.Affilation := Owner;
                  end if;

                  --  Setting role
                  if Role = +"moderator" then
                     Item.Role := Moderator;
                  elsif Role = +"none" then
                     Item.Role := None;
                  elsif Role = +"participant" then
                     Item.Role := Participant;
                  elsif Role = +"visitor" then
                     Item.Role := Visitor;
                  end if;

                  XMPP.MUCS.XMPP_MUC_Access
                   (Self.Stack.Last_Element).Set_Item (Item);
                  return;
               end;
            end if;
         end if;

      --  working with version
      elsif Namespace_URI = XMPP.Versions.Version_URI then
         --  if query found within jabber:iq:version namespace, then
         --  creating a version object.
         if Local_Name = XMPP.Versions.Query_Element then
            Self.Stack.Append
              (XMPP.Objects.XMPP_Object_Access (XMPP.Versions.Create));
         end if;

      --  Here is the end of actual object parsing.
      else
         Log ("WARNING skipped unknown data : ");
         Log ("Namespace_URI : " & Namespace_URI);
         Log ("Local_Name : " & Local_Name);
         return;
      end if;

      if not Self.Stack.Is_Empty then

         --  setting up object's attributes
         for J in 1 .. Attributes.Length loop
            Self.Stack.Last_Element.Set_Content
              (Attributes.Local_Name (J), Attributes.Value (J));
         end loop;

         --  Hack for stream:stream stanza, which does not have close tag
         if Self.Stack.Last_Element.Get_Kind = Stream then
            Self.Stream_Handler.Start_Stream
              (XMPP.Streams.XMPP_Stream_Access (Self.Stack.Last_Element).all);
            Self.Stack.Delete_Last;
         end if;
      end if;
   end Start_Element;

end XMPP.Sessions;
