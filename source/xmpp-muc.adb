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
package body XMPP.MUC is

   --------------
   --  Create  --
   --------------
   function Create return XMPP_MUC_Access is
   begin
      return new XMPP_MUC;
   end Create;

   ----------------
   --  Get_Kind  --
   ----------------
   overriding function Get_Kind (Self : XMPP_MUC) return Objects.Object_Kind is
      pragma Unreferenced (Self);

   begin
      return XMPP.Objects.MUC;
   end Get_Kind;

   -----------------
   --  Serialize  --
   -----------------
   overriding procedure Serialize
    (Self   : XMPP_MUC;
     Writer : in out XML.SAX.Pretty_Writers.SAX_Pretty_Writer'Class) is
      pragma Unreferenced (Self);

   begin
      Writer.Start_Prefix_Mapping (Namespace_URI => MUC_URI);

      Writer.Start_Element (Namespace_URI => MUC_URI,
                            Local_Name    => MUC_Element);

      Writer.End_Element (Namespace_URI => MUC_URI,
                          Local_Name => MUC_Element);
   end Serialize;

   -------------------
   --  Set_Content  --
   -------------------
   overriding procedure Set_Content
     (Self      : in out XMPP_MUC;
      Parameter : League.Strings.Universal_String;
      Value     : League.Strings.Universal_String) is
   begin
      raise Program_Error with "Not yet implemented";
   end Set_Content;

   ----------------
   --  Set_Item  --
   ----------------
   procedure Set_Item (Self : in out XMPP_MUC; Item : MUC_Item) is
   begin
      Self.Item := Item;
   end Set_Item;

end XMPP.MUC;
