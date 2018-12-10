------------------------------------------------------------------------------
--                                                                          --
--                              AXMPP Project                               --
--                                                                          --
--                           XMPP Library for Ada                           --
--                                                                          --
------------------------------------------------------------------------------
--                                                                          --
-- Copyright © 2011-2016, Alexander Basov <coopht@gmail.com>                --
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
      return Object_Kind is
      pragma Unreferenced (Self);

   begin
      return XMPP.Message;
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
     Writer : in out XML.SAX.Pretty_Writers.XML_Pretty_Writer'Class) is

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
--      Attrs.Set_Value
--       (Namespace_URI => XML_URI,
--        Local_Name    => Lang_Attribute,
--        Value         => Self.Language);

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

      XMPP_Message'Class (Self).Custom_Content (Writer);

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
