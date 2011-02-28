------------------------------------------------------------------------------
--                                                                          --
--                                 AXMPP                                    --
--                                                                          --
------------------------------------------------------------------------------
--                                                                          --
-- Copyright © 2010 - 2011 Alexander Basov <coopht@gmail.com>               --
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

with XML.SAX.Pretty_Writers;

with XMPP.Objects;

package XMPP.Messages is

   XML_URI                : constant League.Strings.Universal_String
     := League.Strings.To_Universal_String
         ("http://www.w3.org/XML/1998/namespace");

   Body_Element           : constant League.Strings.Universal_String
     := League.Strings.To_Universal_String ("body");

   Lang_Attribute         : constant League.Strings.Universal_String
     := League.Strings.To_Universal_String ("lang");

   Message_Element        : constant League.Strings.Universal_String
     := League.Strings.To_Universal_String ("message");

   Message_To_Attribute   : constant League.Strings.Universal_String
     := League.Strings.To_Universal_String ("to");

   Message_From_Attribute : constant League.Strings.Universal_String
     := League.Strings.To_Universal_String ("from");

   Message_Id_Attribute   : constant League.Strings.Universal_String
     := League.Strings.To_Universal_String ("id");

   Message_Type_Attribute : constant League.Strings.Universal_String
     := League.Strings.To_Universal_String ("type");

   Subject_Element        : constant League.Strings.Universal_String
     := League.Strings.To_Universal_String ("subject");

   Thread_Element         : constant League.Strings.Universal_String
     := League.Strings.To_Universal_String ("thread");

   type Message_Type is (Chat, Error, Group_Chat, Headline, Normal);

   type Chat_State_Type is (Active, Composing, Paused, Inactive, Gone);

   type Priority_Type is new Integer range -128 .. 127;

   type XMPP_Message is new XMPP.Objects.XMPP_Object with private;

   type XMPP_Message_Access is access all XMPP_Message'Class;

   --  Public API  --

   procedure Set_Type (Self : in out XMPP_Message; T : Message_Type);
   --  Sets type of message. Message type can be one of the following:
   --  1. Chat
   --  2. Error
   --  3. Group_Chat
   --  4. Headline
   --  5. Normal

   function Get_Type (Self : XMPP_Message) return Message_Type;
   --  Returns type of the message.

   procedure Set_Subject (Self : in out XMPP_Message;
                          Subj : League.Strings.Universal_String);
   --  Sets subject of the message.

   function Get_Subject (Self : XMPP_Message)
      return League.Strings.Universal_String;
   --  Returns subject of the message.

   procedure Set_Body (Self : in out XMPP_Message;
                       Val : League.Strings.Universal_String);
   --  Sets message body

   function Get_Body (Self : XMPP_Message)
      return League.Strings.Universal_String;
   --  Reuturns message body

   procedure Set_Thread (Self : in out XMPP_Message;
                         Val : League.Strings.Universal_String);
   --  Sets conversation thread.

   function Get_Thread (Self : XMPP_Message)
      return League.Strings.Universal_String;
   --  Returns conversation thread.

   function Get_To (Self : XMPP_Message)
      return League.Strings.Universal_String;
   --  Returns message recepient

   procedure Set_To (Self : in out XMPP_Message;
                     To   : League.Strings.Universal_String);
   --  Sets message recepient

   function Get_Id (Self : XMPP_Message)
      return League.Strings.Universal_String;
   --  Returns message ID

   procedure Set_Id (Self : in out XMPP_Message;
                     Id   : League.Strings.Universal_String);
   --  Sets message ID

   function Get_From (Self : XMPP_Message)
      return League.Strings.Universal_String;
   --  Returns message sender.

   procedure Set_From (Self : in out XMPP_Message;
                       From : League.Strings.Universal_String);
   --  Sets message sender.

   function Get_Chat_State (Self : XMPP_Message) return Chat_State_Type;
   --  Returns chat state.

   procedure Set_Chat_State (Self  : in out XMPP_Message;
                             Value : Chat_State_Type);
   --  Sets chat states.

   function Create return not null XMPP_Message_Access;
   --  Returns heap allocated message object

   --  End of public API  --

   --  Private API, should not be used by application
   overriding function Get_Kind (Self : XMPP_Message)
      return Objects.Object_Kind;

   overriding procedure Serialize
    (Self   : XMPP_Message;
     Writer : in out XML.SAX.Pretty_Writers.SAX_Pretty_Writer'Class);

   overriding
   procedure Set_Content (Self      : in out XMPP_Message;
                          Parameter : League.Strings.Universal_String;
                          Value     : League.Strings.Universal_String);

private

   type XMPP_Message is new XMPP.Objects.XMPP_Object with
   record
      To              : League.Strings.Universal_String;
      From            : League.Strings.Universal_String;
      Subject         : League.Strings.Universal_String;
      --  TODO: Add support of multiply subject instanses
      Message_Body    : League.Strings.Universal_String;
      --  TODO: Add support of multiply body instanses
      Type_Of_Message : Message_Type := Chat;
      Thread          : League.Strings.Universal_String;
      --  TODO: Add proper type for this attribute
      Language        : League.Strings.Universal_String
        := League.Strings.To_Universal_String ("en");
      Id              : League.Strings.Universal_String;

      --  Chat States
      Chat_State : Chat_State_Type := Active;
   end record;

end XMPP.Messages;
