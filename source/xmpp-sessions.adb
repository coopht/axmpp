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
with Ada.Text_IO;
with Ada.Wide_Wide_Text_IO;

with League.Strings;
with XML.SAX.Readers;

with XMPP.Binds;
with XMPP.Challenges;
with XMPP.IQS;
with XMPP.Networks;
with XMPP.Null_Objects;
with XMPP.Objects;
with XMPP.Streams;
with XMPP.Stream_Features;
with XMPP.Utils;

package body XMPP.Sessions is

   use League.Strings;

   use type Ada.Streams.Stream_Element_Offset;
   use type XMPP.Objects.Object_Kind;

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

   -------------
   --  Close  --
   -------------
   procedure Close (Self : in out XMPP_Session) is
   begin
      null;
   end Close;

   -----------------
   --  Is_Opened  --
   -----------------
   function Is_Opened (Self : XMPP_Session) return Boolean is
   begin
      return Self.Session_Opened;
   end Is_Opened;

   ------------
   --  Open  --
   ------------
   procedure Open (Self : not null access XMPP_Session) is
   begin
      if not Self.Is_Opened then
         Ada.Wide_Wide_Text_IO.Put_Line ("Connecting");
         Self.Connect (Self.Addr.To_Wide_Wide_String, 5222);
         Ada.Wide_Wide_Text_IO.Put_Line ("Starting idle");
         Self.Idle;
      end if;
   end Open;

   ------------------
   --  On_Connect  --
   ------------------
   overriding procedure On_Connect (Self : not null access XMPP_Session) is
   begin
      --  After we connected, initialize parser.

      Ada.Text_IO.Put_Line ("On_Connect");

      if not Self.Source.Is_TLS_Established then
         Self.Reader.Set_Content_Handler
           (XML.SAX.Readers.SAX_Content_Handler_Access (Self));
         XML.SAX.Simple_Readers.Put_Line := Put_Line'Access;

         Self.Source.Set_Socket (Self.Get_Socket);
         Self.Reader.Set_Input_Source (Self.Source'Access);
      end if;

      Self.Open_Stream;
   end On_Connect;

   ---------------------
   --  On_Disconnect  --
   ---------------------
   overriding procedure On_Disconnect (Self : not null access XMPP_Session) is
   begin
      null;
   end On_Disconnect;

   -------------------
   --  Open_Stream  --
   -------------------
   procedure Open_Stream (Self : not null access XMPP_Session) is
      --  TODO:
      --       Use appropriate object, instead of raw xml
      Open_Stream : Universal_String
        := "<stream:stream "
             & "xmlns:stream='http://etherx.jabber.org/streams' "
             & "version='1.0' "
             & "xmlns='jabber:client' "
             & "to='"
             & Self.Host
             & "' >";

   begin
      --  Sending open stream stanza
      Self.Send
        (XMPP.Utils.To_Stream_Element_Array
           (Ada.Characters.Conversions.To_String
              (Open_Stream.To_Wide_Wide_String)),
         Self.Source.Is_TLS_Established);
   end Open_Stream;

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

   ------------------
   --  Characters  --
   ------------------
   overriding procedure Characters
     (Self    : in out XMPP_Session;
      Text    : League.Strings.Universal_String;
      Success : in out Boolean)
   is
   begin
      --  Put_Line ("*** Text = " & Text);
      if not Self.Stack.Is_Empty then
         Self.Stack.Last_Element.Set_Content (Self.Tag, Text);
      end if;
   end Characters;

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
   begin
      --  Put_Line ("<<< End_Element_QN = " & Qualified_Name);
      --  Put_Line ("<<< End_Element_URI = " & Namespace_URI);
      Self.Delete_Object (Namespace_URI.To_Wide_Wide_String,
                          Local_Name.To_Wide_Wide_String);
   end End_Element;

   --------------------
   --  Error_String  --
   --------------------
   overriding function Error_String (Self : XMPP_Session)
                 return League.Strings.Universal_String
   is
   begin
      return X : League.Strings.Universal_String;
   end Error_String;

   ---------------------
   --  Create_Object  --
   ---------------------
   procedure Create_Object (Self          : in out XMPP_Session;
                            Namespace_URI : Wide_Wide_String;
                            Local_Name    : Wide_Wide_String)
   is
   begin
      if Namespace_URI = "http://etherx.jabber.org/streams"
        and Local_Name = "stream" then
         Self.Stack.Append
           (XMPP.Objects.XMPP_Object_Access (XMPP.Streams.Create));
         return;

      elsif Namespace_URI = "http://etherx.jabber.org/streams"
        and Local_Name = "features" then
         Self.Stack.Append
           (XMPP.Objects.XMPP_Object_Access (XMPP.Stream_Features.Create));
         return;

      --  proceed tls connection establishment
      --  nothing todo here, just send required data to server in
      --  End_Element callback
      elsif Namespace_URI = "urn:ietf:params:xml:ns:xmpp-tls"
        and Local_Name = "proceed" then
         return;

      --  Creating Challenge object for sasl authentication
      elsif Namespace_URI = "urn:ietf:params:xml:ns:xmpp-sasl"
        and Local_Name = "challenge" then
         Self.Tag := League.Strings.To_Universal_String (Local_Name);

         Self.Stack.Append
           (XMPP.Objects.XMPP_Object_Access (XMPP.Challenges.Create));
         return;

      --  For successfull authentication
      elsif Namespace_URI = "urn:ietf:params:xml:ns:xmpp-sasl"
        and Local_Name = "success" then
         --  all work is don in delete_object
         return;

      --  Creating IQ object
      elsif Namespace_URI = "jabber:client" and Local_Name = "iq" then
         Self.Stack.Append
           (XMPP.Objects.XMPP_Object_Access
              (XMPP.IQS.Create (XMPP.IQS.Result)));
         return;
      end if;

      --  Creating Null_Object, if actual object cannot be created.

      Ada.Wide_Wide_Text_IO.Put_Line
        ("Don't know what kind of element should be created : ");
      Ada.Wide_Wide_Text_IO.Put_Line ("Namespace_URI : " & Namespace_URI);
      Ada.Wide_Wide_Text_IO.Put_Line ("Local_Name : " & Local_Name);
   end Create_Object;

   ---------------------
   --  Delete_Object  --
   ---------------------
   procedure Delete_Object (Self          : in out XMPP_Session;
                            Namespace_URI : Wide_Wide_String;
                            Local_Name    : Wide_Wide_String)
   is
   begin
      --  Ada.Wide_Wide_Text_IO.Put_Line
      --    ("Stack size at begin : "
      --       & Integer'Wide_Wide_Image (Integer (Self.Stack.Length)));

      if Namespace_URI = "http://etherx.jabber.org/streams"
        and Local_Name = "stream" then

         --  TODO:
         --  Free (Self.Current);
         Self.Stack.Delete_Last;

      elsif Namespace_URI = "http://etherx.jabber.org/streams"
        and Local_Name = "features" then

         if not Self.Authenticated then
            Self.Stream_Handler.Stream_Features
              (XMPP.Stream_Features.XMPP_Stream_Feature_Access
                 (Self.Stack.Last_Element));

         else
            Self.Stream_Handler.Connected
              (XMPP.Stream_Features.XMPP_Stream_Feature_Access
                 (Self.Stack.Last_Element));
         end if;

         --  if tls session was not established, than send command to server
         --  to start tls negotiation
         --  XXX: may be XMPP object for xmpp-tls name space should be created
         if not Self.Source.Is_TLS_Established then
            Ada.Wide_Wide_Text_IO.Put_Line ("Sending starttls");
            Self.Send_Wide_Wide_String
              ("<starttls xmlns='urn:ietf:params:xml:ns:xmpp-tls'/>");

         elsif not Self.Authenticated then
            Ada.Wide_Wide_Text_IO.Put_Line ("Starting sasl auth");
            Self.Send_Wide_Wide_String
              ("<auth xmlns='urn:ietf:params:xml:ns:xmpp-sasl' "
                 & "mechanism='DIGEST-MD5'/>");
         end if;

         Self.Stack.Delete_Last;
         --  TODO:
         --  Free (Self.Current);

      elsif Namespace_URI = "urn:ietf:params:xml:ns:xmpp-tls"
        and Local_Name = "proceed" then
         if not Self.Source.Is_TLS_Established then
            Self.Proceed_TLS_Auth;
         end if;

      --  Proceed with SASL Authentication
      elsif Namespace_URI = "urn:ietf:params:xml:ns:xmpp-sasl"
        and Local_Name = "challenge" then
         --  Proceeding with SASL auth
         Self.Proceed_SASL_Auth
           (XMPP.Challenges.XMPP_Challenge_Access (Self.Stack.Last_Element));

         Self.Stack.Delete_Last;
         --  TODO:
         --  Free (Self.Current);
         return;

      --  For successfull authentication
      elsif Namespace_URI = "urn:ietf:params:xml:ns:xmpp-sasl"
        and Local_Name = "success" then
         --  TODO:
         --  Free (Self.Current);

         --  We do not create any object here, just notifies user about
         --  successful authentification, and opening another one stream
         --  to server
         Self.Stack.Delete_Last;
         Ada.Wide_Wide_Text_IO.Put_Line ("Authentification successfull !!");
         Self.Authenticated := True;
         Self.On_Connect;

         --  For XMPP_Stream_Feature
      elsif Namespace_URI = "urn:ietf:params:xml:ns:xmpp-bind"
        and Local_Name = "bind" then

         --  Adding bind body for IQ object
         if Self.Stack.Element (Integer (Self.Stack.Length) - 1).Get_Kind
           = XMPP.Objects.IQ then
            XMPP.IQS.XMPP_IQ_Access
              (Self.Stack.Element
                 (Integer (Self.Stack.Length) - 1))
              .Append_Item (Self.Stack.Last_Element);
            Self.Stack.Delete_Last;
         end if;
      end if;

      --  Ada.Wide_Wide_Text_IO.Put_Line
      --    ("Stack size at end : "
      --       & Integer'Wide_Wide_Image (Integer (Self.Stack.Length)));
   end Delete_Object;

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
   begin
      --  DEBUG  --
      --  Ada.Wide_Wide_Text_IO.Put
      --    (">>> Start_Element_QN = "
      --       & Qualified_Name.To_Wide_Wide_String & " (");

      --  Ada.Wide_Wide_Text_IO.Put
      --    ("Namespace_URI = " & Namespace_URI.To_Wide_Wide_String & " ");

      --  for J in 1 .. Attributes.Length loop
      --     Ada.Wide_Wide_Text_IO.Put
      --       (Attributes.Local_Name (J).To_Wide_Wide_String
      --          & "="
      --          & Attributes.Value (J).To_Wide_Wide_String
      --          & " ");
      --  end loop;

      --  Ada.Wide_Wide_Text_IO.Put_Line (")");

      --  DEBUG  --

      Self.Tag := Local_Name;

      --  If object was created, then fill it
      if not Self.Stack.Is_Empty then

         --  For XMPP_Stream_Feature
         if Namespace_URI.To_Wide_Wide_String
           = "urn:ietf:params:xml:ns:xmpp-tls"
           and Local_Name.To_Wide_Wide_String = "starttls"
         then
            Self.Stack.Last_Element.Set_Content (Local_Name, Local_Name);

         --  For XMPP_Stream_Feature
         elsif Namespace_URI.To_Wide_Wide_String
           = "urn:ietf:params:xml:ns:xmpp-sasl"
           and Local_Name.To_Wide_Wide_String = "mechanisms" then
            null; --  we can safety skip mechanisms tag here.

         --  For XMPP_Stream_Feature
         elsif Namespace_URI.To_Wide_Wide_String
           = "urn:ietf:params:xml:ns:xmpp-sasl"
           and Local_Name.To_Wide_Wide_String = "mechanism" then
            --  We add mechanism parameter in Characters procedure
            Self.Tag := Local_Name;

         --  For XMPP_Stream_Feature
         elsif Namespace_URI.To_Wide_Wide_String
           = "urn:ietf:params:xml:ns:xmpp-bind" then
            if Local_Name.To_Wide_Wide_String = "bind" then

               --  If bind for iq object, than create new bind object
               --  and push it into stack
               if Self.Stack.Last_Element.Get_Kind = XMPP.Objects.IQ then
                  Self.Stack.Append
                    (XMPP.Objects.XMPP_Object_Access (XMPP.Binds.Create));
               else
                  --  Setting bind feature to stream feature object
                  Self.Stack.Last_Element.Set_Content (Local_Name, Local_Name);
               end if;

               --  For XMPP_Stream_Feature
            elsif Namespace_URI.To_Wide_Wide_String
              = "urn:ietf:params:xml:ns:xmpp-session"
              and Local_Name.To_Wide_Wide_String = "session" then
               --  Setting session feature to stream feature object
               Self.Stack.Last_Element.Set_Content (Local_Name, Local_Name);

            end if;

         --  setting jid value for bind object
         --  the data actual set in character procedure
         elsif Local_Name.To_Wide_Wide_String = "jid" then
            Self.Tag := Local_Name;
         end if;

      --  If Object not yet created, then create it
      else
         Self.Create_Object (Namespace_URI.To_Wide_Wide_String,
                             Local_Name.To_Wide_Wide_String);

         if not Self.Stack.Is_Empty then
            --  Hack for stream:stream stanza, which does not have close tag
            if Self.Stack.Last_Element.Get_Kind = XMPP.Objects.Stream then
               for J in 1 .. Attributes.Length loop
                  Self.Stack.Last_Element.Set_Content
                    (Attributes.Local_Name (J), Attributes.Value (J));
               end loop;

               Self.Stream_Handler.Start_Stream
                 (XMPP.Streams.XMPP_Stream_Access (Self.Stack.Last_Element));
               Self.Stack.Delete_Last;
               return;
            end if;
         end if;
      end if;

      --  setting up object's attributes
      for J in 1 .. Attributes.Length loop
         Self.Stack.Last_Element.Set_Content
           (Attributes.Local_Name (J), Attributes.Value (J));
      end loop;

   end Start_Element;

   -------------
   --  Error  --
   -------------
   overriding procedure Error
    (Self       : in out XMPP_Session;
     Occurrence : XML.SAX.Parse_Exceptions.SAX_Parse_Exception;
     Success    : in out Boolean)
   is
   begin
      Put_Line ("EEE (Error) " & Occurrence.Message & "'");
   end Error;

   ------------------------
   --  Proceed_TLS_Auth  --
   ------------------------
   procedure Proceed_TLS_Auth (Self : not null access XMPP_Session) is
      KX_Priority     : GNUTLS.KX_Algorithm_Array := (GNUTLS.GNUTLS_KX_RSA, 0);

      Proto_Priority  : GNUTLS.Protocol_Array
        := (GNUTLS.GNUTLS_TLS1, GNUTLS.GNUTLS_SSL3, 0);

      Cipher_Priority : GNUTLS.Cipher_Algorithm_Array
        := (GNUTLS.GNUTLS_CIPHER_3DES_CBC, GNUTLS.GNUTLS_CIPHER_ARCFOUR, 0);

      Comp_Priority   : GNUTLS.Compression_Method_Array
        := (GNUTLS.GNUTLS_COMP_ZLIB, GNUTLS.GNUTLS_COMP_NULL, 0);

      Mac_Priority    : GNUTLS.Mac_Algorithm_Array
        := (GNUTLS.GNUTLS_MAC_SHA, GNUTLS.GNUTLS_MAC_MD5, 0);

   begin
      --  Initializing gnutls
      GNUTLS.Global_Set_Log_Level (65537);

      GNUTLS.Global_Init;

      Ada.Text_IO.Put_Line ("GNUTLS.Anon_Allocate_Client_Credentials");

      GNUTLS.Certificate_Allocate_Credentials (Self.Credential);
      --  GNUTLS.Anon_Allocate_Client_Credentials (Self.Cred);

      --  Initialize TLS session
      Ada.Text_IO.Put_Line ("Init");
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

      Ada.Wide_Wide_Text_IO.Put_Line ("GNUTLS.Transport_Set_Ptr");
      GNUTLS.Transport_Set_Ptr (Self.TLS_Session, Self.Get_Socket);

      Ada.Text_IO.Put_Line ("GNUTLS.Handshake");
--        begin
--           GNUTLS.Handshake (Self.TLS_Session);
--
--        exception
--           when others =>
--              Ada.Text_IO.Put_Line
--                (GNUTLS.IO_Direction'Image
--                   (GNUTLS.Get_Direction (Self.TLS_Session)));
--        end;
--        Ada.Text_IO.Put_Line ("End of GNUTLS.Handshake");
--
      Self.Set_TLS_Session (Self.TLS_Session);
      Self.Source.Set_TLS_Session (Self.TLS_Session);
      Self.Source.Object := Self.all'Unchecked_Access;

      Self.Source.Start_Handshake;

      --  On Success handshake,
      --  we should reopen stream via TLS session.

      --        Ada.Wide_Wide_Text_IO.Put_Line
      --          ("TLS Session established. Sending Stream");
      --  Self.On_Connect;
   exception
      when E : others =>
         Ada.Text_IO.Put_Line
           (Ada.Exceptions.Exception_Name (E) &
              ": " & Ada.Exceptions.Exception_Message (E));

         Self.Disconnect;
   end Proceed_TLS_Auth;

   ----------------
   --  Put_Line  --
   ----------------
   procedure Put_Line (Item : League.Strings.Universal_String) is
   begin
      Ada.Wide_Wide_Text_IO.Put_Line (Item.To_Wide_Wide_String);
   end Put_Line;

   -----------------
   --  Read_Data  --
   -----------------
   overriding
   procedure Read_Data (Self   : not null access XMPP_Session)
   is
   begin
      Self.Reader.Parse;
   end Read_Data;

   -----------------------------
   --  Send_Wide_Wide_String  --
   -----------------------------
   procedure Send_Wide_Wide_String (Self : in out XMPP_Session;
                                    Str  : Wide_Wide_String) is
   begin
      --  DEBUG
      Ada.Wide_Wide_Text_IO.Put_Line ("Sending XML : " & Str);

      Self.Send
        (XMPP.Utils.To_Stream_Element_Array
           (Ada.Characters.Conversions.To_String (Str)),
        Self.Source.Is_TLS_Established);
   end Send_Wide_Wide_String;

   ---------------
   --  Set_JID  --
   ---------------
   procedure Set_JID (Self : in out XMPP_Session;
                      JID  : League.Strings.Universal_String) is
   begin
      Self.JID := JID;
   end Set_JID;

   ----------------
   --  Set_Host  --
   ----------------
   procedure Set_Host (Self : in out XMPP_Session;
                       Host : League.Strings.Universal_String) is
   begin
      Self.Host := Host;
   end Set_Host;

   --------------------
   --  Set_Password  --
   --------------------
   procedure Set_Password (Self     : in out XMPP_Session;
                           Password : League.Strings.Universal_String) is
   begin
      Self.Password := Password;
   end Set_Password;

   ---------------------
   --  Set_Host_Addr  --
   ---------------------
   procedure Set_Host_Addr (Self : in out XMPP_Session;
                            Addr : League.Strings.Universal_String) is
   begin
      Self.Addr := Addr;
   end Set_Host_Addr;

   procedure Send_Object (Self   : not null access XMPP_Session;
                          Object : XMPP.Objects.XMPP_Object'Class) is
   begin
      Self.Send_Wide_Wide_String (Object.Serialize.To_Wide_Wide_String);
   end Send_Object;

end XMPP.Sessions;
