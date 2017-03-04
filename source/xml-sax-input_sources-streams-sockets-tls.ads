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
with GNUTLS;
limited with XMPP.Networks;

package XML.SAX.Input_Sources.Streams.Sockets.TLS is

   type Source_State is
     (Raw,       --  Unprotected data sent/received through socket connection.
      Handshake, --  SSL/TLS handshake in progress
      TLS);      --  Protected data sent/received through socket connection.

   type TLS_Socket_Input_Source is new Socket_Input_Source with record
      TLS_State   : Source_State := Raw;
      TLS_Session : GNUTLS.Session;
      Object      : access XMPP.Networks.Notification'Class;
   end record;

   --  just for debuggig
   overriding procedure Next
    (Self        : in out TLS_Socket_Input_Source;
     Buffer      : in out
       not null Matreshka.Internals.Strings.Shared_String_Access;
     End_Of_Data : out Boolean);

   overriding procedure Read
    (Self        : in out TLS_Socket_Input_Source;
     Buffer      : out Ada.Streams.Stream_Element_Array;
     Last        : out Ada.Streams.Stream_Element_Offset;
     End_Of_Data : out Boolean);

   function Is_TLS_Established (Self : TLS_Socket_Input_Source) return Boolean;

--     procedure Set_TLS_Established (Self  : in out TLS_Socket_Input_Source;
--                                    Value : Boolean);

   procedure Set_TLS_Session (Self  : in out TLS_Socket_Input_Source;
                              S     : GNUTLS.Session);

   procedure Start_Handshake (Self : in out TLS_Socket_Input_Source);

end XML.SAX.Input_Sources.Streams.Sockets.TLS;
