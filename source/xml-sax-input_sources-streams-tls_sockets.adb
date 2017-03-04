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
with Ada.Characters.Conversions;
with Ada.Text_IO;

with XMPP.Logger;

package body XML.SAX.Input_Sources.Streams.TLS_Sockets is

   ------------
   --  Read  --
   ------------
   overriding procedure Read
    (Self        : in out TLS_Socket_Input_Source;
     Buffer      : out Ada.Streams.Stream_Element_Array;
     Last        : out Ada.Streams.Stream_Element_Offset;
     End_Of_Data : out Boolean) is

      X : GNAT.Sockets.Request_Type (GNAT.Sockets.N_Bytes_To_Read);

   begin
      --  Getting how much data available in Socket
      GNAT.Sockets.Control_Socket (Self.Socket, X);
      if X.Size > 0 then
         case Self.TLS_State is
            when Handshake =>
               Last := Buffer'First - 1;

               begin
                  GNUTLS.Handshake (Self.TLS_Session);
                  Self.TLS_State := TLS;
                  XMPP.Logger.Log ("Handshake complete");
                  Self.Object.On_Connect;

               exception
                  when others =>
                     XMPP.Logger.Log
                       (Ada.Characters.Conversions.To_Wide_Wide_String
                          (GNUTLS.IO_Direction'Image
                             (GNUTLS.Get_Direction (Self.TLS_Session))));
               end;

            when TLS =>
               declare
                  E      : constant GNAT.Sockets.Vector_Element :=
                    (Base => Buffer (Buffer'First)'Unchecked_Access,
                     Length => Buffer'Length);
                  Length : Ada.Streams.Stream_Element_Offset;
                  Vector : GNAT.Sockets.Vector_Type (0 .. 0);

               begin
                  Vector (0) := E;

                  GNUTLS.Record_Recv (Self.TLS_Session, Vector, Length);
                  Last := Buffer'First + Length - 1;

                  XMPP.Logger.Log ("=================================");
                  --  Log
                  --   ("Data of"
                  --   & Ada.Streams.Stream_Element_Offset'Image (Buffer'First)
                  --     & " .."
                  --     & Ada.Streams.Stream_Element_Offset'Image (Last)
                  --     & " received from GNUTLS.Record_Recv");

                  XMPP.Logger.Log ("Recieved from GNUTLS.Record_Recv : ");

                  if XMPP.Logger.Is_Debug_Output_Enabled then
                     for J in Buffer'First .. Last loop
                        Ada.Text_IO.Put (Character'Val (Buffer (J)));
                     end loop;
                  end if;

                  XMPP.Logger.Log ("");
                  XMPP.Logger.Log ("=================================");
               end;
         end case;

      else
         Last := Buffer'First - 1;
      end if;

      End_Of_Data := False;
   end Read;

   -----------------------
   --  Start_Handshake  --
   -----------------------

   procedure Start_Handshake
     (Self    : in out TLS_Socket_Input_Source;
      Socket  : GNAT.Sockets.Socket_Type;
      Session : GNUTLS.Session) is
   begin
      Self.TLS_State := Handshake;
      Self.TLS_Session := Session;
      Self.Socket := Socket;
      GNUTLS.Handshake (Self.TLS_Session);
      Self.TLS_State := TLS;
      Self.Object.On_Connect;

   exception
      when others =>
         XMPP.Logger.Log
          (Ada.Characters.Conversions.To_Wide_Wide_String
            (GNUTLS.IO_Direction'Image
              (GNUTLS.Get_Direction (Self.TLS_Session))));
   end Start_Handshake;

end XML.SAX.Input_Sources.Streams.TLS_Sockets;
