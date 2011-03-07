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
with League.Strings.Internals;
with Matreshka.Internals.Strings.Operations;
with Matreshka.Internals.Utf16;

with XMPP.Logger;

package body XML.SAX.Input_Sources.Streams.Sockets.Debug is

   ----------
   -- Next --
   ----------

   overriding procedure Next
    (Self        : in out Debug_Socket_Input_Source;
     Buffer      : in out
       not null Matreshka.Internals.Strings.Shared_String_Access;
     End_Of_Data : out Boolean)
   is
      use type Matreshka.Internals.Utf16.Utf16_String_Index;

      Old_Unused : constant Matreshka.Internals.Utf16.Utf16_String_Index
        := Buffer.Unused;
      Old_Length : constant Natural := Buffer.Length;
      New_Data   : Matreshka.Internals.Strings.Shared_String_Access;

   begin
      Socket_Input_Source (Self).Next (Buffer, End_Of_Data);
      New_Data :=
        Matreshka.Internals.Strings.Operations.Slice
         (Buffer,
          Old_Unused,
          Buffer.Unused - Old_Unused,
          Buffer.Length - Old_Length);
      XMPP.Logger.Log (League.Strings.Internals.Wrap (New_Data));
   end Next;

end XML.SAX.Input_Sources.Streams.Sockets.Debug;
