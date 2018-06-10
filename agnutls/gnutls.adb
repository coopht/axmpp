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

package body GNUTLS is
   use type Ada.Streams.Stream_Element_Count;
   use type Interfaces.C.size_t;
   use type Interfaces.C.int;

   --
   function gnutls_anon_allocate_client_credentials
     (SC : access Anon_Client_Credentials) return Interfaces.C.int;
   pragma Import (C,
                  gnutls_anon_allocate_client_credentials,
                  "gnutls_anon_allocate_client_credentials");

   --
   function gnutls_global_init return Interfaces.C.int;
   pragma Import (C, gnutls_global_init, "gnutls_global_init");

   --
   function gnutls_init (S : access Session; CE : Connection_End)
      return Interfaces.C.int;
   pragma Import (C, gnutls_init, "gnutls_init");

   --
   function gnutls_priority_set_direct
     (S          : Session;
      Priorities : Interfaces.C.char_array;
      Error_Pos  : Interfaces.C.char_array)
      return Interfaces.C.int;
   pragma Import (C, gnutls_priority_set_direct, "gnutls_priority_set_direct");

   --
   function gnutls_set_default_priority (S : Session) return Interfaces.C.int;
   pragma Import
     (C, gnutls_set_default_priority, "gnutls_set_default_priority");

   --
   function gnutls_credentials_set_anon (S      : Session;
                                         C_Type : Credentials_Type;
                                         Cred   : Anon_Client_Credentials)
     return Interfaces.C.int;
   pragma Import (C,
                  gnutls_credentials_set_anon,
                  "gnutls_credentials_set");

   --
   function gnutls_credentials_set (S      : Session;
                                    C_Type : Credentials_Type;
                                    Cred   : Certificate_Client_Credentials)
      return Interfaces.C.int;
   pragma Import (C, gnutls_credentials_set, "gnutls_credentials_set");

   --
   function gnutls_certificate_allocate_credentials
     (SC : access Certificate_Client_Credentials)
      return Interfaces.C.int;
   pragma Import (C,
                  gnutls_certificate_allocate_credentials,
                  "gnutls_certificate_allocate_credentials");

   --
   procedure gnutls_transport_set_ptr (S   : Session;
                                       Ptr : Interfaces.C.int);
   pragma Import (C, gnutls_transport_set_ptr, "gnutls_transport_set_ptr");

   --
   function gnutls_handshake (S : Session) return Interfaces.C.int;
   pragma Import (C, gnutls_handshake, "gnutls_handshake");

   --
   function gnutls_record_send
     (S         : Session;
      Data      : GNAT.Sockets.Stream_Element_Reference;
      Data_Size : Interfaces.C.size_t)
      return Interfaces.C.size_t;
   pragma Import (C, gnutls_record_send, "gnutls_record_send");

   --
   function gnutls_record_recv
     (S         : Session;
      Data      : GNAT.Sockets.Stream_Element_Reference;
      Data_Size : Interfaces.C.size_t)
     return Interfaces.C.size_t;
   pragma Import (C, gnutls_record_recv, "gnutls_record_recv");

   --
   function gnutls_bye (S : Session; How : Close_Request)
      return Interfaces.C.int;
   pragma Import (C, gnutls_bye, "gnutls_bye");

   --
   procedure gnutls_deinit (S : Session);
   pragma Import (C, gnutls_deinit, "gnutls_deinit");

   --
   procedure gnutls_anon_free_client_credentials
     (SC : Anon_Client_Credentials);
   pragma Import (C,
                  gnutls_anon_free_client_credentials,
                  "gnutls_anon_free_client_credentials");

   --
   procedure gnutls_global_deinit;
   pragma Import (C, gnutls_global_deinit, "gnutls_global_deinit");

   --
   procedure gnutls_perror (Code : Interfaces.C.int);
   pragma Import (C, gnutls_perror, "gnutls_perror");

   --
   procedure gnutls_global_set_log_level (Level : Interfaces.C.int);
   pragma Import (C,
                  gnutls_global_set_log_level,
                  "gnutls_global_set_log_level");

   --
   function gnutls_record_get_direction (S : Session) return Interfaces.C.int;
   pragma Import
     (C, gnutls_record_get_direction, "gnutls_record_get_direction");

   --
   function gnutls_error_is_fatal (E : Interfaces.C.int)
      return Interfaces.C.int;
   pragma Import (C, gnutls_error_is_fatal, "gnutls_error_is_fatal");

   ----------------------------------------
   --  Anon_Allocate_Client_Credentials  --
   ----------------------------------------
   procedure Anon_Allocate_Client_Credentials
     (SC : out Anon_Client_Credentials)
   is
      Tmp : aliased Anon_Client_Credentials;
      Ret : constant Interfaces.C.int
        := gnutls_anon_allocate_client_credentials (Tmp'Access);

   begin
      if Ret /= 0 then
         gnutls_perror (Ret);
         raise GNUTLS_Error with "cannot do Anon_Allocate_Client_Credentials";
      end if;

      SC := Tmp;
   end Anon_Allocate_Client_Credentials;

   ------------------------------------
   --  Anon_Free_Client_Credentials  --
   ------------------------------------
   procedure Anon_Free_Client_Credentials (Cert : Anon_Client_Credentials)
   is
   begin
      gnutls_anon_free_client_credentials (Cert);
   end Anon_Free_Client_Credentials;

   -----------
   --  Bye  --
   -----------
   procedure Bye (S : Session; How : Close_Request)
   is
      Ret : constant Interfaces.C.int := gnutls_bye (S, How);

   begin
      if Ret /= 0 then
         gnutls_perror (Ret);
         raise GNUTLS_Error with "Bye failed";
      end if;
   end Bye;

   ----------------------------------------
   --  Certificate_Allocate_Credentials  --
   ----------------------------------------
   procedure Certificate_Allocate_Credentials
     (SC : out Certificate_Client_Credentials)
   is
      Tmp : aliased Certificate_Client_Credentials;
      Ret : constant Interfaces.C.int
        := gnutls_certificate_allocate_credentials (Tmp'Access);

   begin
      if Ret /= 0 then
         gnutls_perror (Ret);
         raise GNUTLS_Error with "Certificate_Allocate_Credentials failed";
      end if;

      SC := Tmp;
   end Certificate_Allocate_Credentials;

   -----------------------
   --  Credentials_Set  --
   -----------------------
   procedure Credentials_Set (S    : Session;
                              T    : Credentials_Type;
                              Cred : Anon_Client_Credentials)
   is
      Ret : constant Interfaces.C.int
        := gnutls_credentials_set_anon (S, T, Cred);

   begin
      if  Ret /= 0 then
         gnutls_perror (Ret);
         raise GNUTLS_Error with "Credentials_Set error";
      end if;
   end Credentials_Set;

   -----------------------
   --  Credentials_Set  --
   -----------------------
   procedure Credentials_Set (S    : Session;
                              T    : Credentials_Type;
                              Cred : Certificate_Client_Credentials)
   is
      Ret : constant Interfaces.C.int := gnutls_credentials_set (S, T, Cred);

   begin
      if  Ret /= 0 then
         gnutls_perror (Ret);
         raise GNUTLS_Error with "Credentials_Set error";
      end if;
   end Credentials_Set;

   --------------
   --  Deinit  --
   --------------
   procedure Deinit (S : Session)
   is
   begin
      gnutls_deinit (S);
   end Deinit;

   ----------------------
   --  Error_Is_Fatal  --
   ----------------------
   function Error_Is_Fatal (E : Integer) return Error_Kind is
   begin
      case gnutls_error_is_fatal (Interfaces.C.int (E)) is
         when 0 =>
            return NON_FATAL_ERROR;

         when 1 =>
            return FATAL_ERROR;

         when others =>
            return UNKNOWN_ERROR;
      end case;
   end Error_Is_Fatal;

   ---------------------
   --  Get_Direction  --
   ---------------------

   function Get_Direction (S : Session) return IO_Direction is
   begin
      if gnutls_record_get_direction (S) = 0 then
         return Read;

      else
         return Write;
      end if;
   end Get_Direction;

   -------------------
   --  Global_Init  --
   -------------------
   procedure Global_Deinit
   is
   begin
      gnutls_global_deinit;
   end Global_Deinit;

   -------------------
   --  Global_Init  --
   -------------------
   procedure Global_Init
   is
      Ret : constant Interfaces.C.int := gnutls_global_init;

   begin
      if Ret /= 0 then
         gnutls_perror (Ret);
         raise GNUTLS_Error
           with "Some error occuered during gnutls initialization";
      end if;
   end Global_Init;

   ---------------------
   --  Set_Log_Level  --
   ---------------------
   procedure Global_Set_Log_Level (Level : Integer) is
   begin
      gnutls_global_set_log_level (Interfaces.C.int (Level));
   end Global_Set_Log_Level;

   -----------------
   --  Handshake  --
   -----------------
   procedure Handshake (S : Session)
   is
      Ret : constant Interfaces.C.int := gnutls_handshake (S);

   begin
      if Ret /= 0 then
         gnutls_perror (Ret);
         raise GNUTLS_Error with "Handshake failed";
      end if;
   end Handshake;

   ------------
   --  Init  --
   ------------
   procedure Init (S : out Session; CE : Connection_End)
   is
      Tmp : aliased Session;
      Ret : constant Interfaces.C.int := gnutls_init (Tmp'Access, CE);

   begin
      if Ret /= 0 then
         gnutls_perror (Ret);
         raise GNUTLS_Error with "Cannot do Init";
      end if;
      S := Tmp;
   end Init;

   ---------------------------
   --  Priority_Set_Direct  --
   ---------------------------
   procedure Priority_Set_Direct (S : Session;
                                  Priorities : String;
                                  Error_Pos  : out String)
   is
      X : Interfaces.C.char_array (1 .. 256);
      pragma Warnings (Off, X);
      Ret : constant Interfaces.C.int
        := gnutls_priority_set_direct (S, Interfaces.C.To_C (Priorities), X);

   begin
      if Ret /= 0 then
         gnutls_perror (Ret);
         Error_Pos := Interfaces.C.To_Ada (X);
         raise GNUTLS_Error with "Priority_Set_Direct error";
      end if;
   end Priority_Set_Direct;

   -------------------
   --  Record_Recv  --
   -------------------
   procedure Record_Recv (S      : Session;
                          Data   : GNAT.Sockets.Vector_Type;
                          Length : out Ada.Streams.Stream_Element_Count)
   is
      N_Read : Interfaces.C.size_t;

   begin
      Length := 0;

      for I in Data'Range loop
         N_Read := gnutls_record_recv (S, Data (I).Base, Data (I).Length);

         if N_Read = 0 then
            gnutls_perror (Interfaces.C.int (N_Read));
            raise GNUTLS_Error with "Data read = 0";
         end if;

         Length := Length + Ada.Streams.Stream_Element_Count (N_Read);

         if N_Read < Data (I).Length then
            --  nothing to read
            return;
         end if;
      end loop;

   end Record_Recv;

   -------------------
   --  Record_Send  --
   -------------------
   procedure Record_Send (S      : Session;
                          Data   : GNAT.Sockets.Vector_Type;
                          Length : out Ada.Streams.Stream_Element_Count)
   is
      N_Write : Interfaces.C.size_t;

   begin
      Length := 0;

      for I in Data'Range loop
         N_Write := gnutls_record_send (S, Data (I).Base, Data (I).Length);

         if N_Write = 0 then
            raise GNUTLS_Error with "Data written = 0";
         end if;

         Length := Length + Ada.Streams.Stream_Element_Count (N_Write);

         if N_Write < Data (I).Length then
            --  nothing to write
            return;
         end if;
      end loop;

   end Record_Send;

   ----------------------------
   --  Set_Default_Priority  --
   ----------------------------

   procedure Set_Default_Priority (S : Session) is
      Ret : constant Interfaces.C.int := gnutls_set_default_priority (S);

   begin
      if Ret /= 0 then
         gnutls_perror (Ret);
         raise GNUTLS_Error with "Set_Default_Priority failed";
      end if;
   end Set_Default_Priority;

   -------------------------
   --  Transport_Set_Ptr  --
   -------------------------
   procedure Transport_Set_Ptr (S      : Session;
                                Socket : GNAT.Sockets.Socket_Type)
   is
      Sock : constant Interfaces.C.int
        := Interfaces.C.int (GNAT.Sockets.To_C (Socket));

   begin
      gnutls_transport_set_ptr (S, Sock);
   end Transport_Set_Ptr;

end GNUTLS;
