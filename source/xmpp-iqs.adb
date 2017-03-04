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

package body XMPP.IQS is

   use League.Strings;

   --------------
   --  End_IQ  --
   --------------
   not overriding procedure End_IQ
    (Self   : XMPP_IQ;
     Writer : in out XML.SAX.Pretty_Writers.XML_Pretty_Writer'Class) is
     pragma Unreferenced (Self);

   begin
      Writer.End_Element (Qualified_Name => IQ_Element);
   end End_IQ;

   ----------------
   --  Get_From  --
   ----------------
   function Get_From (Self : XMPP_IQ) return League.Strings.Universal_String is
   begin
      return Self.From;
   end Get_From;

   --------------
   --  Get_Id  --
   --------------
   function Get_Id (Self : XMPP_IQ) return League.Strings.Universal_String is
   begin
      return Self.Id;
   end Get_Id;

   -------------------
   --  Get_IQ_Kind  --
   -------------------
   function Get_IQ_Kind (Self : XMPP_IQ) return IQ_Kind is
   begin
      return Self.Kind_Of_IQ;
   end Get_IQ_Kind;

   ----------------
   --  Get_Kind  --
   ----------------
   overriding function Get_Kind (Self : XMPP_IQ) return Object_Kind is
      pragma Unreferenced (Self);

   begin
      return XMPP.IQ;
   end Get_Kind;

   --------------
   --  Get_To  --
   --------------
   function Get_To (Self : XMPP_IQ) return League.Strings.Universal_String is
   begin
      return Self.To;
   end Get_To;

   -------------------
   --  Set_Content  --
   -------------------
   overriding procedure Set_Content
     (Self      : in out XMPP_IQ;
      Parameter : League.Strings.Universal_String;
      Value     : League.Strings.Universal_String) is
   begin
      if Parameter.To_Wide_Wide_String = "type" then
         if Value.To_Wide_Wide_String = "set" then
            Self.Kind_Of_IQ := Set;

         elsif Value.To_Wide_Wide_String = "get" then
            Self.Kind_Of_IQ := Get;

         elsif Value.To_Wide_Wide_String = "result" then
            Self.Kind_Of_IQ := Result;

         elsif Value.To_Wide_Wide_String = "error" then
            Self.Kind_Of_IQ := Error;
         end if;

      elsif Parameter.To_Wide_Wide_String = "id" then
         Self.Id := Value;

      elsif Parameter.To_Wide_Wide_String = "to" then
         Self.To := Value;

      elsif Parameter.To_Wide_Wide_String = "from" then
         Self.From := Value;

      elsif Parameter.To_Wide_Wide_String = "iq" then
         null;

      else
         XMPP.Logger.Log ("XMPP_IQ : Unknown parameter : " & Parameter);
      end if;
   end Set_Content;

   ----------------
   --  Set_From  --
   ----------------
   procedure Set_From (Self : in out XMPP_IQ;
                       Val  : League.Strings.Universal_String) is
   begin
      Self.From := Val;
   end Set_From;

   --------------
   --  Set_Id  --
   --------------
   procedure Set_Id (Self : in out XMPP_IQ;
                     Val  : League.Strings.Universal_String) is
   begin
      Self.Id := Val;
   end Set_Id;

   -------------------
   --  Set_IQ_Kind  --
   -------------------
   procedure Set_IQ_Kind (Self : in out XMPP_IQ; Val : IQ_Kind) is
   begin
      Self.Kind_Of_IQ := Val;
   end Set_IQ_Kind;

   --------------
   --  Set_To  --
   --------------
   procedure Set_To (Self : in out XMPP_IQ;
                     Val  : League.Strings.Universal_String) is
   begin
      Self.To := Val;
   end Set_To;

   ----------------
   --  Start_IQ  --
   ----------------
   not overriding procedure Start_IQ
    (Self   : XMPP_IQ;
     Writer : in out XML.SAX.Pretty_Writers.XML_Pretty_Writer'Class) is

      Attrs   : XML.SAX.Attributes.SAX_Attributes;

   begin
      --  Generating IQ container xml
      case Self.Kind_Of_IQ is
         when Set =>
            Attrs.Set_Value
             (Qualified_Name => IQ_Type_Attribute,
              Value          => To_Universal_String ("set"));

         when Get =>
            Attrs.Set_Value
             (Qualified_Name => IQ_Type_Attribute,
              Value          => To_Universal_String ("get"));

         when Result =>
            Attrs.Set_Value
             (Qualified_Name => IQ_Type_Attribute,
              Value          => To_Universal_String ("result"));

         when Error =>
            Attrs.Set_Value
             (Qualified_Name => IQ_Type_Attribute,
              Value          => To_Universal_String ("error"));
      end case;

      Attrs.Set_Value
        (Qualified_Name => IQ_Id_Attribute,
         Value          => Self.Get_Id);

      if not Self.To.Is_Empty then
         Attrs.Set_Value
          (Qualified_Name => IQ_To_Attribute,
           Value          => Self.To);
      end if;

      if not Self.From.Is_Empty then
         Attrs.Set_Value
          (Qualified_Name => IQ_From_Attribute,
           Value          => Self.From);
      end if;

      Writer.Start_Element (Qualified_Name => IQ_Element,
                            Attributes     => Attrs);
   end Start_IQ;

end XMPP.IQS;
