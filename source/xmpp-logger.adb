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
with Ada.Wide_Wide_Text_IO;

package body XMPP.Logger is

   Do_Debug_Output : Boolean := False;

   ---------------------
   --  Disable_Debug  --
   ---------------------

   procedure Disable_Debug is
   begin
      Do_Debug_Output := False;
   end Disable_Debug;

   --------------------
   --  Enable_Debug  --
   --------------------

   procedure Enable_Debug is
   begin
      Do_Debug_Output := True;
   end Enable_Debug;

   -------------------------------
   --  Is_Debug_Output_Enabled  --
   -------------------------------

   function Is_Debug_Output_Enabled return Boolean is
   begin
      return Do_Debug_Output;
   end Is_Debug_Output_Enabled;

   -----------
   --  Log  --
   -----------

   procedure Log (Msg : Wide_Wide_String) is
   begin
      if Do_Debug_Output then
         Ada.Wide_Wide_Text_IO.Put_Line (Msg);
      end if;
   end Log;

   -----------
   --  Log  --
   -----------

   procedure Log (Msg : League.Strings.Universal_String) is
   begin
      if Do_Debug_Output then
         Ada.Wide_Wide_Text_IO.Put_Line (Msg.To_Wide_Wide_String);
      end if;
   end Log;

end XMPP.Logger;
