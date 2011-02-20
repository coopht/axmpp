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
--  <Unit> XMPP.Sessions
--  <ImplementationNotes>
--
------------------------------------------------------------------------------
--  $Revision$ $Author$
--  $Date$
------------------------------------------------------------------------------
with Ada.Characters.Conversions;
with Ada.Exceptions;
with Ada.Streams;

with XML.SAX.Readers;

with XMPP.Binds;
with XMPP.Services;
with XMPP.Services_Features;
with XMPP.Services_Identities;
with XMPP.IQ_Sessions;
with XMPP.Logger;
with XMPP.Messages;
with XMPP.MUC;
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

   use type Ada.Streams.Stream_Element_Offset;
   use type XMPP.IQS.IQ_Kind;

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
      Bind_Object.Set_From (Self.JID & "@" & Self.Host);
      Bind_Object.Set_IQ_Kind (XMPP.IQS.Set);
      Bind_Object.Set_Resource (Resource_Id);

      Bind_Object.Set_Id (+"bind_1");
      --  Bind_IQ.Append_Item (Bind_Object);

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
   begin
      --  Log ("*** Text = [" & Text & "]");
      if not Self.Tag.Is_Empty and not Text.Is_Empty then
         Self.Stack.Last_Element.Set_Content (Self.Tag, Text);
         Self.Tag.Clear;
         Success := True;
      end if;
   end Characters;

   -------------
   --  Close  --
   -------------
   procedure Close (Self : in out XMPP_Session) is
   begin
      null;
   end Close;

   ----------------------------
   --  Discover_Information  --
   ----------------------------
   procedure Discover_Information (Self : in out XMPP_Session;
                                   JID  : League.Strings.Universal_String) is
      D : XMPP.Services.XMPP_Service;

   begin
      D.Set_IQ_Kind (XMPP.IQS.Get);
      D.Set_Type (XMPP.Services_Features.Protocol_Disco_Info);
      D.Set_From (Self.JID & "@" & Self.Host);
      D.Set_To (JID);
      D.Set_Id (+"info1");

      --  XXX: removed hardcoded ID;
      --  D.Append_Item (D);
      Self.Send_Object (D);
   end Discover_Information;

   ----------------------
   --  Discover_Items  --
   ----------------------
   procedure Discover_Items (Self : in out XMPP_Session;
                             JID  : League.Strings.Universal_String) is
      D : XMPP.Services.XMPP_Service;

   begin
      D.Set_IQ_Kind (XMPP.IQS.Get);
      D.Set_Type (XMPP.Services_Features.Protocol_Disco_Items);
      D.Set_From (Self.JID & "@" & Self.Host);
      D.Set_To (JID);
      D.Set_Id (+"info1");

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
      --  Log ("<<< End_Element_QN = " & Qualified_Name);
      --  Log ("<<< End_Element_URI = " & Namespace_URI);

      --  Log ("Stack size at begin : "
      --        & Integer'Wide_Wide_Image (Integer (Self.Stack.Length)));

      if Namespace_URI = +"http://etherx.jabber.org/streams" then
         if Local_Name = +"stream" then

            --  TODO:
            --  Free (Self.Current);
            Self.Stack.Delete_Last;

         elsif Local_Name = +"features" then
            if not Self.Authenticated then
               Self.Stream_Handler.Stream_Features
                (XMPP.Stream_Features.XMPP_Stream_Feature_Access
                  (Self.Stack.Last_Element).all);

            else
               Self.Stream_Handler.Connected
                 (XMPP.Stream_Features.XMPP_Stream_Feature_Access
                    (Self.Stack.Last_Element).all);
            end if;

            --  if tls session was not established, than send command to server
            --  to start tls negotiation
            --  XXX: may be XMPP object for xmpp-tls name
            --  space should be created
            if not Self.Source.Is_TLS_Established then
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
        and Local_Name = +"proceed" then
         if not Self.Source.Is_TLS_Established then
            Self.Proceed_TLS_Auth;
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
            Self.On_Connect;
         end if;

      --  For IQ
      elsif Namespace_URI = +"urn:ietf:params:xml:ns:xmpp-bind" then

         --  XXX: ugly code
         if Local_Name = +"bind" then
            null;
            --  Adding bind body for IQ object
            --  if Previous (Current) /= No_Element then
            --   if Self.Stack.Element (To_Index (Previous (Current))).Get_Kind
            --       = XMPP.Objects.Bind then
            --        Self.Stream_Handler.Bind_Resource_State
            --          (XMPP.Binds.XMPP_Bind_Access
            --            (Self.Stack.Last_Element).Get_JID,
            --           XMPP.Binds.Success);
                  --  XMPP.IQS.XMPP_IQ_Access
                  --    (Self.Stack.Element (To_Index (Previous (Current))))
                  --     .Append_Item (Self.Stack.Last_Element);

            --  Self.Stack.Delete_Last;
            --     end if;
            --  end if;
         end if;

      --  For IQ
      elsif Namespace_URI = +"urn:ietf:params:xml:ns:xmpp-session" then

         --  Nothing to process here

         null;

      elsif Namespace_URI = +"jabber:iq:version" then

         --  Nothing to process here

         null;

      elsif Namespace_URI = +"jabber:iq:roster" then

         --  Adding item to the roster
         if Local_Name = +"item" then
            if Self.Stack.Last_Element.Get_Kind = Objects.Roster_Item then
               if Previous (Current) /= No_Element then
                  if Self.Stack.Element
                    (To_Index
                      (Previous (Current))).Get_Kind = XMPP.Objects.Roster
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
        Namespace_URI = +"http://jabber.org/protocol/disco#items" then

         --  Nothing to process here

         null;

      elsif Namespace_URI = +"http://jabber.org/protocol/muc#user" then
         if Local_Name = +"x" then
            if Self.Stack.Last_Element.Get_Kind = XMPP.Objects.MUC then
               if Previous (Current) /= No_Element then
                  if Self.Stack.Element
                    (To_Index (Previous (Current))).Get_Kind
                                                        = XMPP.Objects.Presence
                  then
                     declare
                        M : constant access XMPP.MUC.XMPP_MUC
                          := XMPP.MUC.XMPP_MUC_Access
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

            Self.Process_IQ (Self.Stack.Last_Element);
            Self.Stack.Delete_Last;

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

      Success := True;

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
      S.Set_From (Self.JID & "@" & Self.Host);
      S.Set_IQ_Kind (XMPP.IQS.Set);
      S.Set_Id (+"sess_1");
      Self.Send_Object (S);
   end Establish_IQ_Session;

   -------------------
   --  Fatal_Error  --
   -------------------
   overriding procedure Fatal_Error
    (Self       : in out XMPP_Session;
     Occurrence : XML.SAX.Parse_Exceptions.SAX_Parse_Exception;
     Success    : in out Boolean) is
      pragma Unreferenced (Success);

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

   ------------------
   --  On_Connect  --
   ------------------
   overriding procedure On_Connect (Self : not null access XMPP_Session) is
   begin
      --  After we connected, initialize parser.

      Log ("On_Connect!");
      if not Self.Source.Is_TLS_Established then

         Log ("Reset Parser");

         Self.Source.Set_Socket (Self.Get_Socket);
      end if;

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
         Self.Connect (Self.Addr.To_Wide_Wide_String, 5222);
         Log ("Starting idle");
         Self.Idle;
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
      Self.Reader.Set_Input_Source (Self.Source'Access);

      --  Sending open stream stanza
      Self.Send
        (XMPP.Utils.To_Stream_Element_Array
           (Ada.Characters.Conversions.To_String
              (Open_Stream.To_Wide_Wide_String)),
         Self.Source.Is_TLS_Established);
   end Open_Stream;

   -------------------------
   --  Proceed_SASL_Auth  --
   -------------------------
   procedure Proceed_SASL_Auth
     (Self   : not null access XMPP_Session;
      Object : not null XMPP.Challenges.XMPP_Challenge_Access) is
   begin
      Object.Set_JID (Self.JID);
      Object.Set_Host (Self.Host);
      Object.Set_Password (Self.Password);
      Self.Send_Wide_Wide_String
        (Object.Generate_Response.To_Wide_Wide_String);
   end Proceed_SASL_Auth;

   ------------------------
   --  Proceed_TLS_Auth  --
   ------------------------
   procedure Proceed_TLS_Auth (Self : not null access XMPP_Session) is
      KX_Priority     : constant GNUTLS.KX_Algorithm_Array
        := (GNUTLS.GNUTLS_KX_RSA, 0);

      Proto_Priority  : constant GNUTLS.Protocol_Array
        := (GNUTLS.GNUTLS_TLS1, GNUTLS.GNUTLS_SSL3, 0);

      Cipher_Priority : constant GNUTLS.Cipher_Algorithm_Array
        := (GNUTLS.GNUTLS_CIPHER_3DES_CBC, GNUTLS.GNUTLS_CIPHER_ARCFOUR, 0);

      --  Comp_Priority   : GNUTLS.Compression_Method_Array
      --    := (GNUTLS.GNUTLS_COMP_ZLIB, GNUTLS.GNUTLS_COMP_NULL, 0);

      --  Mac_Priority    : GNUTLS.Mac_Algorithm_Array
      --    := (GNUTLS.GNUTLS_MAC_SHA, GNUTLS.GNUTLS_MAC_MD5, 0);

   begin
      --  Initializing gnutls
      GNUTLS.Global_Set_Log_Level (65537);

      GNUTLS.Global_Init;

      Log ("GNUTLS.Anon_Allocate_Client_Credentials");

      GNUTLS.Certificate_Allocate_Credentials (Self.Credential);
      --  GNUTLS.Anon_Allocate_Client_Credentials (Self.Cred);

      --  Initialize TLS session
      Log ("Init");
      GNUTLS.Init (Self.TLS_Session, GNUTLS.GNUTLS_CLIENT);

      GNUTLS.Protocol_Set_Priority (Self.TLS_Session, Proto_Priority);
      GNUTLS.Cipher_Set_Priority (Self.TLS_Session, Cipher_Priority);

      --  GNUTLS.Compression_Set_Priority (Self.TLS_Session, Comp_Priority);

      --  GNUTLS.Mac_Set_Priority (Self.TLS_Session, Mac_Priority);

      GNUTLS.Credentials_Set (Self.TLS_Session,
                              GNUTLS.GNUTLS_CRD_CERTIFICATE,
                              Self.Credential);

      GNUTLS.Set_Default_Priority (Self.TLS_Session);
      GNUTLS.KX_Set_Priority (Self.TLS_Session, KX_Priority);

      --  GNUTLS.Credentials_Set (Self.TLS_Session,
      --                          GNUTLS.GNUTLS_CRD_ANON,
      --                          Self.Cred);

      Log ("GNUTLS.Transport_Set_Ptr");
      GNUTLS.Transport_Set_Ptr (Self.TLS_Session, Self.Get_Socket);

      Log ("GNUTLS.Handshake");
--        begin
--           GNUTLS.Handshake (Self.TLS_Session);
--
--        exception
--           when others =>
--              Log (GNUTLS.IO_Direction'Image
--                    (GNUTLS.Get_Direction (Self.TLS_Session)));
--        end;
--        Log ("End of GNUTLS.Handshake");
--
      Self.Set_TLS_Session (Self.TLS_Session);
      Self.Source.Set_TLS_Session (Self.TLS_Session);
      Self.Source.Object := Self.all'Unchecked_Access;

      Self.Source.Start_Handshake;

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
                         IQ   : not null XMPP.Objects.XMPP_Object_Access) is
   begin
      --  Log ("XMPP.Session.Process_IQ : IQ arrived : "
      --         & XMPP.IQS.IQ_Kind'Wide_Wide_Image (IQ.Get_IQ_Kind));

      --  Setting_IQ_Header (to, from, type, id attrs)

      Log ("Process_IQ:");
      Log (Self.IQ_Header.Get_To);
      Log (Self.IQ_Header.Get_From);
      Log (Self.IQ_Header.Get_Id);
      Log (XMPP.IQS.IQ_Kind'Wide_Wide_Image (Self.IQ_Header.Get_IQ_Kind));

      XMPP.IQS.XMPP_IQ_Access (IQ).Set_To (Self.IQ_Header.Get_To);
      XMPP.IQS.XMPP_IQ_Access (IQ).Set_From (Self.IQ_Header.Get_From);
      XMPP.IQS.XMPP_IQ_Access (IQ).Set_Id (Self.IQ_Header.Get_Id);
      XMPP.IQS.XMPP_IQ_Access (IQ).Set_IQ_Kind (Self.IQ_Header.Get_IQ_Kind);

      case IQ.Get_Kind is

         --  Resource Binded
         when XMPP.Objects.Bind =>
            Self.Stream_Handler.Bind_Resource_State
              (XMPP.Binds.XMPP_Bind_Access (IQ).Get_JID,
               XMPP.Binds.Success);

         --  Session established
         when XMPP.Objects.IQ_Session =>
               Self.Stream_Handler.Session_State
                 (XMPP.IQ_Sessions.Established);

         --  Roster arrived
         when XMPP.Objects.Roster =>
            Self.Stream_Handler.Roster
              (XMPP.Rosters.XMPP_Roster_Access (IQ).all);

         --  Discover information arrived
         when XMPP.Objects.Disco =>
            Self.Stream_Handler.Service_Information
              (XMPP.Services.XMPP_Service_Access (IQ).all);

         --  Software version information arrived
         when XMPP.Objects.Version =>
            Self.Stream_Handler.Version
              (XMPP.Versions.XMPP_Version_Access (IQ).all);

         when others =>
            null;
      end case;

      Self.IQ_Header.Set_To (+"");
      Self.IQ_Header.Set_From (+"");
      Self.IQ_Header.Set_Id (+"");
      Self.IQ_Header.Set_IQ_Kind (XMPP.IQS.Get);
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
   begin
      Self.Send_Wide_Wide_String
        ("<iq from='"
           & Self.JID.To_Wide_Wide_String
           & "@"
           & Self.Host.To_Wide_Wide_String
           & "' type='get' id='roster_1'>"
           & " <query xmlns='jabber:iq:roster'/>"
           & "</iq>");
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
      Ver.Set_From (Self.JID & "@" & Self.Host);
      Ver.Set_IQ_Kind (XMPP.IQS.Get);
      Ver.Set_Id (+"ver_1");

      Self.Send_Object (Ver);
   end Request_Version;

   -------------------
   --  Send_Object  --
   -------------------
   procedure Send_Object (Self   : not null access XMPP_Session;
                          Object : XMPP.Objects.XMPP_Object'Class) is
   begin
      Object.Serialize (Self.Writer);

      Log ("Sending Data : " & Self.Writer.Text);
      Self.Send_Wide_Wide_String (Self.Writer.Text.To_Wide_Wide_String);

      Self.Writer.Reset;
   end Send_Object;

   -----------------------------
   --  Send_Wide_Wide_String  --
   -----------------------------
   procedure Send_Wide_Wide_String (Self : in out XMPP_Session;
                                    Str  : Wide_Wide_String) is
   begin
      --  DEBUG
      Log ("Sending XML : " & Str);

      Self.Send
        (XMPP.Utils.To_Stream_Element_Array
           (Ada.Characters.Conversions.To_String (Str)),
         Self.Source.Is_TLS_Established);
   end Send_Wide_Wide_String;

   ----------------
   --  Set_Host  --
   ----------------
   procedure Set_Host (Self : in out XMPP_Session;
                       Host : League.Strings.Universal_String) is
   begin
      Self.Host := Host;
   end Set_Host;

   ---------------------
   --  Set_Host_Addr  --
   ---------------------
   procedure Set_Host_Addr (Self : in out XMPP_Session;
                            Addr : League.Strings.Universal_String) is
   begin
      Self.Addr := Addr;
   end Set_Host_Addr;

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
           and Local_Name = +"proceed" then
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
      elsif Namespace_URI = +"urn:ietf:params:xml:ns:xmpp-bind" then
         if Local_Name = +"bind" then

            --  If bind for iq object, than create new bind object
            --  and push it into stack

            if Self.Stack.Is_Empty then
               Self.Stack.Append
                 (XMPP.Objects.XMPP_Object_Access (XMPP.Binds.Create));

            else
               if Self.Stack.Last_Element.Get_Kind
                 = Objects.Stream_Features then
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
        and Local_Name = +"session" then
         --  Setting session feature to stream feature object
         if Self.Stack.Is_Empty then
            Self.Stack.Append
              (XMPP.Objects.XMPP_Object_Access (XMPP.IQ_Sessions.Create));

         elsif Self.Stack.Last_Element.Get_Kind
           = XMPP.Objects.Stream_Features then

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
        = +"http://jabber.org/protocol/chatstates" then
         if Local_Name = +"active"
           or Local_Name = +"inactive"
           or Local_Name = +"gone"
           or Local_Name = +"composing"
           or Local_Name = +"paused"
         then
            if Self.Stack.Last_Element.Get_Kind = Objects.Message then
               Self.Stack.Last_Element.Set_Content (Local_Name, Local_Name);
            end if;
         end if;

      --  working with roster
      elsif Namespace_URI = +"jabber:iq:roster" then
         --  if query found within jabber:iq:roster namespace, then
         --  creating a roster list.
         if Local_Name = +"query" then

            Self.Stack.Append
              (XMPP.Objects.XMPP_Object_Access (XMPP.Rosters.Create));

         --  if item found within jabber:iq:roster namespace, then
         --  creating a roster item.
         elsif Local_Name = +"item" then
            if Self.Stack.Last_Element.Get_Kind = Objects.Roster then
               Self.Stack.Append
                 (XMPP.Objects.XMPP_Object_Access (XMPP.Roster_Items.Create));
            end if;

         --  nothing to do here for group tag
         elsif Local_Name = +"group" then
            null;
         end if;

      elsif Namespace_URI = +"http://jabber.org/protocol/disco#info" then
         if Local_Name = +"query" then
            if Self.Stack.Last_Element.Get_Kind = Objects.IQ then
               Self.Stack.Append
                 (XMPP.Objects.XMPP_Object_Access (XMPP.Services.Create));
            end if;

         elsif Local_Name = +"identity" then
            if Self.Stack.Last_Element.Get_Kind = Objects.Disco then
               XMPP.Services.XMPP_Service_Access
                 (Self.Stack.Last_Element).Add_Identity
                  (XMPP.Services_Identities.Create
                     (Attributes.Value (1), Attributes.Value (2)));
               return;
            end if;

         elsif Local_Name = +"feature" then
            if Self.Stack.Last_Element.Get_Kind = Objects.Disco then
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
            if Self.Stack.Last_Element.Get_Kind = Objects.Disco then
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
            if Self.Stack.Last_Element.Get_Kind = Objects.Presence then
               Self.Stack.Append
                (XMPP.Objects.XMPP_Object_Access (XMPP.MUC.Create));
            end if;
         elsif Local_Name = +"item" then
            if Self.Stack.Last_Element.Get_Kind = Objects.MUC then
               declare
                  Item       : XMPP.MUC.MUC_Item;
                  Affilation : constant Universal_String
                    := Attributes.Value (+"affilation");

                  Role       : constant Universal_String
                    := Attributes.Value (+"role");

               begin
                  --  Setting affilation
                  if Affilation = +"admin" then
                     Item.Affilation := XMPP.MUC.Admin;
                  elsif Affilation = +"member" then
                     Item.Affilation := XMPP.MUC.Member;
                  elsif Affilation = +"none" then
                     Item.Affilation := XMPP.MUC.None;
                  elsif Affilation = +"outcast" then
                     Item.Affilation := XMPP.MUC.Outcast;
                  elsif Affilation = +"owner" then
                     Item.Affilation := XMPP.MUC.Owner;
                  end if;

                  --  Setting role
                  if Role = +"moderator" then
                     Item.Role := XMPP.MUC.Moderator;
                  elsif Role = +"none" then
                     Item.Role := XMPP.MUC.None;
                  elsif Role = +"participant" then
                     Item.Role := XMPP.MUC.Participant;
                  elsif Role = +"visitor" then
                     Item.Role := XMPP.MUC.Visitor;
                  end if;

                  XMPP.MUC.XMPP_MUC_Access
                   (Self.Stack.Last_Element).Set_Item (Item);
                  return;
               end;
            end if;
         end if;

      --  working with version
      elsif Namespace_URI = +"jabber:iq:version" then
         --  if query found within jabber:iq:version namespace, then
         --  creating a version object.
         if Local_Name = +"query" then
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
         if Self.Stack.Last_Element.Get_Kind = XMPP.Objects.Stream then
            Self.Stream_Handler.Start_Stream
              (XMPP.Streams.XMPP_Stream_Access (Self.Stack.Last_Element).all);
            Self.Stack.Delete_Last;
         end if;
      end if;
   end Start_Element;

end XMPP.Sessions;
