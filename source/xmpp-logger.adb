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
--  <Unit> XMPP.Logger
--  <ImplementationNotes>
--
------------------------------------------------------------------------------
--  $Revision$ $Author$
--  $Date$
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
