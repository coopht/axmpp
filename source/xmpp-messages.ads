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

package XMPP.Messages is

   type Message_Type is (Chat, Error, Group_Chat, Headline, Normal);

   type Priority_Type is new Integer range -128 .. 127;

   type XMPP_Message is new XMPP.Objects.XMPP_Object with private;

   type XMPP_Message_Access is access all XMPP_Message'Class;

   overriding function Get_Kind (Self : XMPP_Message)
      return Objects.Object_Kind;

   overriding function Serialize (Self : in XMPP_Message)
      return League.Strings.Universal_String;

   overriding
   procedure Set_Content (Self      : in out XMPP_Message;
                          Parameter : League.Strings.Universal_String;
                          Value     : League.Strings.Universal_String);

   procedure Set_Type (Self : in out XMPP_Message; T : Message_Type);

   function Get_Type (Self : XMPP_Message) return Message_Type;

   procedure Set_Subject (Self : in out XMPP_Message;
                          Subj : League.Strings.Universal_String);

   function Get_Subject (Self : XMPP_Message)
      return League.Strings.Universal_String;

   procedure Set_Body (Self : in out XMPP_Message;
                       Val : League.Strings.Universal_String);

   function Get_Body (Self : XMPP_Message)
      return League.Strings.Universal_String;

   procedure Set_Thread (Self : in out XMPP_Message;
                         Val : League.Strings.Universal_String);

   function Get_Thread (Self : XMPP_Message)
      return League.Strings.Universal_String;

   function Get_To (Self : XMPP_Message)
      return League.Strings.Universal_String;

   procedure Set_To (Self : in out XMPP_Message;
                     To   : League.Strings.Universal_String);

   function Get_Id (Self : XMPP_Message)
      return League.Strings.Universal_String;

   procedure Set_Id (Self : in out XMPP_Message;
                     Id   : League.Strings.Universal_String);

   function Get_From (Self : XMPP_Message)
      return League.Strings.Universal_String;

   procedure Set_From (Self : in out XMPP_Message;
                       From : League.Strings.Universal_String);

   procedure Set_Is_Composing (Self  : in out XMPP_Message;
                               Value : Boolean);

   function Is_Composing (Self : XMPP_Message) return Boolean;

   function Create return not null XMPP_Message_Access;

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
      Is_Composing    : Boolean := False;
   end record;

end XMPP.Messages;

