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
with XML.SAX.Attributes;

with XMPP.Logger;

package body XMPP.Messages is

   use League.Strings;

   --------------
   --  Create  --
   --------------
   function Create return not null XMPP_Message_Access is
   begin
      return new XMPP_Message;
   end Create;

   ----------------
   --  Get_Body  --
   ----------------
   function Get_Body (Self : XMPP_Message)
      return League.Strings.Universal_String is
   begin
      return Self.Message_Body;
   end Get_Body;

   ----------------------
   --  Get_Chat_State  --
   ----------------------
   function Get_Chat_State (Self : XMPP_Message) return Chat_State_Type is
   begin
      return Self.Chat_State;
   end Get_Chat_State;

   ----------------
   --  Get_From  --
   ----------------
   function Get_From (Self : XMPP_Message)
      return League.Strings.Universal_String is
   begin
      return Self.From;
   end Get_From;

   --------------
   --  Get_Id  --
   --------------
   function Get_Id (Self : XMPP_Message)
      return League.Strings.Universal_String is
   begin
      return Self.Id;
   end Get_Id;

   ----------------
   --  Get_Kind  --
   ----------------
   overriding function Get_Kind (Self : XMPP_Message)
      return Objects.Object_Kind is
      pragma Unreferenced (Self);

   begin
      return XMPP.Objects.Message;
   end Get_Kind;

   -------------------
   --  Get_Subject  --
   -------------------
   function Get_Subject (Self : XMPP_Message)
      return League.Strings.Universal_String is
   begin
      return Self.Subject;
   end Get_Subject;

   ------------------
   --  Get_Thread  --
   ------------------
   function Get_Thread (Self : XMPP_Message)
      return League.Strings.Universal_String is
   begin
      return Self.Thread;
   end Get_Thread;

   --------------
   --  Get_To  --
   --------------
   function Get_To (Self : XMPP_Message)
      return League.Strings.Universal_String is
   begin
      return Self.To;
   end Get_To;

   ----------------
   --  Get_Type  --
   ----------------
   function Get_Type (Self : XMPP_Message) return Message_Type is
   begin
      return Self.Type_Of_Message;
   end Get_Type;

   -----------------
   --  Serialize  --
   -----------------
   overriding procedure Serialize
    (Self   : XMPP_Message;
     Writer : in out XML.SAX.Pretty_Writers.SAX_Pretty_Writer'Class) is

      Attrs : XML.SAX.Attributes.SAX_Attributes;

   begin
      --  setting 'to' attr
      if not Self.To.Is_Empty then
         Attrs.Set_Value (Qualified_Name => Message_To_Attribute,
                          Value          => Self.To);
      end if;

      --  setting 'from' attr
      if not Self.From.Is_Empty then
         Attrs.Set_Value (Qualified_Name => Message_From_Attribute,
                          Value          => Self.From);
      end if;

      --  setting 'id' attr
      if not Self.Id.Is_Empty then
         Attrs.Set_Value (Qualified_Name => Message_Id_Attribute,
                          Value          => Self.Id);
      end if;

      --  setting 'type' attr
      case Self.Type_Of_Message is
         when Chat =>
            Attrs.Set_Value
             (Qualified_Name => Message_Type_Attribute,
              Value          => To_Universal_String ("chat"));
         when Error =>
            Attrs.Set_Value
             (Qualified_Name => Message_Type_Attribute,
              Value          => To_Universal_String ("error"));

         when Group_Chat =>
            Attrs.Set_Value
             (Qualified_Name => Message_Type_Attribute,
              Value          => To_Universal_String ("groupchat"));

         when Headline =>
            Attrs.Set_Value
             (Qualified_Name => Message_Type_Attribute,
              Value          => To_Universal_String ("headline"));

         when Normal =>
            Attrs.Set_Value
             (Qualified_Name => Message_Type_Attribute,
              Value          => To_Universal_String ("normal"));
      end case;

      --  setting xml:lang attr
      Attrs.Set_Value
       (Namespace_URI => XML_URI,
        Local_Name    => Lang_Attribute,
        Value         => Self.Language);

      Writer.Start_Element (Qualified_Name => Message_Element,
                            Attributes     => Attrs);

      if not Self.Subject.Is_Empty then
         Writer.Start_Element (Qualified_Name => Subject_Element);
         Writer.Characters (Self.Subject);
         Writer.End_Element (Qualified_Name => Subject_Element);
      end if;

      if not Self.Message_Body.Is_Empty then
         Writer.Start_Element (Qualified_Name => Body_Element);
         Writer.Characters (Self.Message_Body);
         Writer.End_Element (Qualified_Name => Body_Element);
      end if;

      --  setting 'thread
      if not Self.Thread.Is_Empty then
         Writer.Start_Element (Qualified_Name => Thread_Element);
         Writer.Characters (Self.Thread);
         Writer.End_Element (Qualified_Name => Thread_Element);
      end if;

      Writer.End_Element (Qualified_Name => Message_Element);
   end Serialize;

   ----------------
   --  Set_Body  --
   ----------------
   procedure Set_Body (Self : in out XMPP_Message;
                       Val  : League.Strings.Universal_String) is
   begin
      Self.Message_Body := Val;
   end Set_Body;

   ----------------------
   --  Set_Chat_State  --
   ----------------------
   procedure Set_Chat_State (Self  : in out XMPP_Message;
                             Value : Chat_State_Type) is
   begin
      Self.Chat_State := Value;
   end Set_Chat_State;

   -------------------
   --  Set_Content  --
   -------------------
   overriding
   procedure Set_Content (Self      : in out XMPP_Message;
                          Parameter : League.Strings.Universal_String;
                          Value     : League.Strings.Universal_String) is
   begin
      if Parameter = To_Universal_String ("to") then
         Self.To := Value;

      elsif Parameter = To_Universal_String ("from") then
         Self.From := Value;

      elsif Parameter = To_Universal_String ("type") then
         if Value = To_Universal_String ("chat") then
            Self.Type_Of_Message := Chat;

         elsif Value = To_Universal_String ("error") then
            Self.Type_Of_Message := Error;

         elsif Value = To_Universal_String ("groupchat") then
            Self.Type_Of_Message := Group_Chat;

         elsif Value = To_Universal_String ("headline") then
            Self.Type_Of_Message := Headline;

         elsif Value = To_Universal_String ("normal") then
            Self.Type_Of_Message := Normal;

         else
            XMPP.Logger.Log ("Unknown message type: " & Value);
         end if;

      elsif Parameter = To_Universal_String ("subject") then
         Self.Subject := Value;

      elsif Parameter = To_Universal_String ("body") then
         Self.Message_Body := Value;

      elsif Parameter = To_Universal_String ("thread") then
         Self.Thread := Value;

      elsif Parameter = To_Universal_String ("id") then
         Self.Id := Value;

      elsif Parameter = To_Universal_String ("composing") then
         Self.Chat_State := Composing;

      elsif Parameter = To_Universal_String ("active") then
         Self.Chat_State := Active;

      elsif Parameter = To_Universal_String ("paused") then
         Self.Chat_State := Paused;

      elsif Parameter = To_Universal_String ("inactive") then
         Self.Chat_State := Inactive;

      elsif Parameter = To_Universal_String ("gone") then
         Self.Chat_State := Gone;

      --  XXX: this should not happen,but it happens =(
      elsif Parameter = To_Universal_String ("message") then
         null;

      else
         XMPP.Logger.Log ("WARNING: Unknown parameter : " & Parameter);
      end if;
   end Set_Content;

   ----------------
   --  Set_From  --
   ----------------
   procedure Set_From (Self : in out XMPP_Message;
                       From : League.Strings.Universal_String) is
   begin
      Self.From := From;
   end Set_From;

   --------------
   --  Set_Id  --
   --------------
   procedure Set_Id (Self : in out XMPP_Message;
                     Id   : League.Strings.Universal_String) is
   begin
      Self.Id := Id;
   end Set_Id;

   -------------------
   --  Set_Subject  --
   -------------------
   procedure Set_Subject (Self : in out XMPP_Message;
                          Subj : League.Strings.Universal_String) is
   begin
      Self.Subject := Subj;
   end Set_Subject;

   ------------------
   --  Set_Thread  --
   ------------------
   procedure Set_Thread (Self : in out XMPP_Message;
                         Val : League.Strings.Universal_String) is
   begin
      Self.Thread := Val;
   end Set_Thread;

   --------------
   --  Set_To  --
   --------------
   procedure Set_To (Self : in out XMPP_Message;
                     To   : League.Strings.Universal_String) is
   begin
      Self.To := To;
   end Set_To;

   ----------------
   --  Set_Type  --
   ----------------
   procedure Set_Type (Self : in out XMPP_Message; T : Message_Type) is
   begin
      Self.Type_Of_Message := T;
   end Set_Type;

end XMPP.Messages;
