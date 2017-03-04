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

package body XMPP.Presences is

   use League.Strings;

   --------------
   --  Create  --
   --------------
   function Create return XMPP_Presence_Access is
   begin
      return new XMPP_Presence;
   end Create;

   ----------------
   --  Get_From  --
   ----------------
   function Get_From (Self : XMPP_Presence)
      return League.Strings.Universal_String is
   begin
      return Self.From;
   end Get_From;

   ----------------
   --  Get_Kind  --
   ----------------
   overriding function Get_Kind (Self : XMPP_Presence) return Object_Kind is
      pragma Unreferenced (Self);

   begin
      return XMPP.Presence;
   end Get_Kind;

   ---------------
   --  Get_MUC  --
   ---------------
   function Get_MUC (Self : XMPP_Presence) return XMPP.MUCS.XMPP_MUC is
   begin
      return Self.MUC;
   end Get_MUC;

   --------------------
   --  Get_Priority  --
   --------------------
   function Get_Priority (Self : XMPP_Presence) return Priority_Type is
   begin
      return Self.Priority;
   end Get_Priority;

   ----------------
   --  Get_Show  --
   ----------------
   function Get_Show (Self : XMPP_Presence) return Show_Kind is
   begin
      return Self.Show;
   end Get_Show;

   ------------------
   --  Get_Status  --
   ------------------
   function Get_Status (Self : XMPP_Presence)
      return League.Strings.Universal_String is
   begin
      return Self.Status;
   end Get_Status;

   --------------
   --  Get_To  --
   --------------
   function Get_To (Self : XMPP_Presence)
      return League.Strings.Universal_String is
   begin
      return Self.To;
   end Get_To;

   ----------------
   --  Get_Type  --
   ----------------
   function Get_Type (Self : XMPP_Presence) return Presence_Type is
   begin
      return Self.Type_Of_Presence;
   end Get_Type;

   ---------------------
   --  Is_Multi_Chat  --
   ---------------------
   function Is_Multi_Chat (Self : XMPP_Presence) return Boolean is
   begin
      return Self.Multi_Chat;
   end Is_Multi_Chat;

   -----------------
   --  Serialize  --
   -----------------
   overriding procedure Serialize
    (Self   : XMPP_Presence;
     Writer : in out XML.SAX.Pretty_Writers.XML_Pretty_Writer'Class) is

      Attrs   : XML.SAX.Attributes.SAX_Attributes;

   begin
      if not Self.To.Is_Empty then
         Attrs.Set_Value (Qualified_Name => Presence_To_Attribute,
                          Value          => Self.To);
      end if;

      if not Self.From.Is_Empty then
         Attrs.Set_Value (Qualified_Name => Presence_From_Attribute,
                          Value          => Self.From);
      end if;

      Writer.Start_Element (Qualified_Name => Presence_Element,
                            Attributes     => Attrs);

      if Self.Priority /= -129 then
         Writer.Start_Element (Qualified_Name => Priority_Element);
         Writer.Characters
          (League.Strings.To_Universal_String
            (Priority_Type'Wide_Wide_Image (Self.Priority)));
         Writer.End_Element (Qualified_Name => Priority_Element);
      end if;

      if not Self.Status.Is_Empty then
         Writer.Start_Element (Qualified_Name => Status_Element);
         Writer.Characters (Self.Status);
         Writer.End_Element (Qualified_Name => Status_Element);
      end if;

      if Self.Show /= Online then
         Writer.Start_Element (Qualified_Name => Show_Element);

         case Self.Show is
            when Away =>
               Writer.Characters (To_Universal_String ("away"));

            when Chat =>
               Writer.Characters (To_Universal_String ("chat"));

            when DND =>
               Writer.Characters (To_Universal_String ("dnd"));

            when XA =>
               Writer.Characters (To_Universal_String ("xa"));

            when Online =>
               raise Program_Error;
         end case;

         Writer.End_Element (Qualified_Name => Show_Element);
      end if;

      if Self.Multi_Chat then
         Self.MUC.Serialize (Writer);
      end if;

      Writer.End_Element (Qualified_Name => Presence_Element);
   end Serialize;

   -------------------
   --  Set_Content  --
   -------------------
   overriding procedure Set_Content
     (Self      : in out XMPP_Presence;
      Parameter : League.Strings.Universal_String;
      Value     : League.Strings.Universal_String) is
   begin
      if Parameter = To_Universal_String ("from") then
         Self.From := Value;

      elsif Parameter = To_Universal_String ("to") then
         Self.To := Value;

      elsif Parameter = To_Universal_String ("show") then
         if Value = To_Universal_String ("away") then
            Self.Show := Away;

         elsif Value = To_Universal_String ("chat") then
            Self.Show := Chat;

         elsif Value = To_Universal_String ("dnd") then
            Self.Show := DND;

         elsif Value = To_Universal_String ("xa") then
            Self.Show := XA;

         elsif Value = To_Universal_String ("online") then
            Self.Show := Online;
         end if;

      elsif Parameter = To_Universal_String ("status") then
         Self.Status := Value;

      elsif Parameter = To_Universal_String ("type") then
         if Value = To_Universal_String ("error") then
            Self.Type_Of_Presence := Error;

         elsif Value = To_Universal_String ("probe") then
            Self.Type_Of_Presence := Probe;

         elsif Value = To_Universal_String ("subscribe") then
            Self.Type_Of_Presence := Subscribe;

         elsif Value = To_Universal_String ("subscribed") then
            Self.Type_Of_Presence := Subscribed;

         elsif Value = To_Universal_String ("unavailable") then
            Self.Type_Of_Presence := Unavailable;

         elsif Value = To_Universal_String ("unsubscribe") then
            Self.Type_Of_Presence := Unsubscribe;

         elsif Value = To_Universal_String ("unsubscribed") then
            Self.Type_Of_Presence := Unsubscribed;
         end if;

      elsif Parameter = To_Universal_String ("priority") then
         Self.Priority
           := Priority_Type'Wide_Wide_Value (Value.To_Wide_Wide_String);

      elsif Parameter = To_Universal_String ("presence") then
         null;

      else
         XMPP.Logger.Log ("Unknown Parameter : " & Parameter);
      end if;
   end Set_Content;

   ----------------
   --  Set_From  --
   ----------------
   procedure Set_From (Self  : in out XMPP_Presence;
                       Value  : League.Strings.Universal_String) is
   begin
      Self.From := Value;
   end Set_From;

   ----------------------
   --  Set_Multi_Chat  --
   ----------------------
   procedure Set_Multi_Chat (Self : in out XMPP_Presence;
                             MUC  : XMPP.MUCS.XMPP_MUC) is
   begin
      Self.MUC := MUC;
      Self.Multi_Chat := True;
   end Set_Multi_Chat;

   --------------------
   --  Set_Priority  --
   --------------------
   procedure Set_Priority (Self : in out XMPP_Presence; P : Priority_Type) is
   begin
      Self.Priority := P;
   end Set_Priority;

   ----------------
   --  Set_Show  --
   ----------------
   procedure Set_Show (Self : in out XMPP_Presence; Show : Show_Kind) is
   begin
      Self.Show := Show;
   end Set_Show;

   ------------------
   --  Set_Status  --
   ------------------
   procedure Set_Status (Self   : in out XMPP_Presence;
                         Status : League.Strings.Universal_String) is
   begin
      Self.Status := Status;
   end Set_Status;

   --------------
   --  Set_To  --
   --------------
   procedure Set_To (Self  : in out XMPP_Presence;
                     Value : League.Strings.Universal_String) is
   begin
      Self.To := Value;
   end Set_To;

   ----------------
   --  Set_Type  --
   ----------------
   procedure Set_Type (Self : in out XMPP_Presence; Value : Presence_Type) is
   begin
      Self.Type_Of_Presence := Value;
   end Set_Type;

end XMPP.Presences;
