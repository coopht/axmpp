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
with Ada.Streams;
with GNAT.Sockets;
with Interfaces.C;

package GNUTLS is

   GNUTLS_Error : exception;

   type Error_Kind is (NON_FATAL_ERROR, FATAL_ERROR, UNKNOWN_ERROR);

   type Connection_End is
     (GNUTLS_SERVER,
      GNUTLS_CLIENT);
   pragma Convention (C, Connection_End);
   for Connection_End'Size use Interfaces.C.unsigned'Size;
   for Connection_End use
     (GNUTLS_SERVER => 1,
      GNUTLS_CLIENT => 2);

   type Credentials_Type is
     (GNUTLS_CRD_CERTIFICATE,
      GNUTLS_CRD_ANON,
      GNUTLS_CRD_SRP,
      GNUTLS_CRD_PSK,
      GNUTLS_CRD_IA);
   pragma Convention (C, Credentials_Type);
   for Credentials_Type'Size use Interfaces.C.unsigned'Size;
   for Credentials_Type use
     (GNUTLS_CRD_CERTIFICATE => 1,
      GNUTLS_CRD_ANON        => 2,
      GNUTLS_CRD_SRP         => 3,
      GNUTLS_CRD_PSK         => 4,
      GNUTLS_CRD_IA          => 5);

   type Close_Request is
     (GNUTLS_SHUT_RDWR,
      GNUTLS_SHUT_WR);
   pragma Convention (C, Close_Request);
   for Close_Request'Size use Interfaces.C.unsigned'Size;

   type KX_Algorithm is new Interfaces.C.int;
   type KX_Algorithm_Array is array (Positive range <>) of KX_Algorithm;

   GNUTLS_KX_UNKNOWN    : constant KX_Algorithm := 0;
   GNUTLS_KX_RSA        : constant KX_Algorithm := 1;
   GNUTLS_KX_DHE_DSS    : constant KX_Algorithm := 2;
   GNUTLS_KX_DHE_RSA    : constant KX_Algorithm := 3;
   GNUTLS_KX_ANON_DH    : constant KX_Algorithm := 4;
   GNUTLS_KX_SRP        : constant KX_Algorithm := 5;
   GNUTLS_KX_RSA_EXPORT : constant KX_Algorithm := 6;
   GNUTLS_KX_SRP_RSA    : constant KX_Algorithm := 7;
   GNUTLS_KX_SRP_DSS    : constant KX_Algorithm := 8;
   GNUTLS_KX_PSK        : constant KX_Algorithm := 9;
   GNUTLS_KX_DHE_PSK    : constant KX_Algorithm := 10;

   type Protocol is new Interfaces.C.int;
   type Protocol_Array is array (Positive range <>) of Protocol;

   GNUTLS_SSL3   : constant Protocol := 1;
   GNUTLS_TLS1_0 : constant Protocol := 2;
   GNUTLS_TLS1_1 : constant Protocol := 3;
   GNUTLS_TLS1_2 : constant Protocol := 4;
   GNUTLS_TLS1   : constant Protocol := GNUTLS_TLS1_0;
   GNUTLS_VERSION_UNKNOWN : constant Protocol := 16#ff#;

   type Cipher_Algorithm is new Interfaces.C.int;
   type Cipher_Algorithm_Array
      is array (Positive range <>) of Cipher_Algorithm;

   --  #define GNUTLS_CIPHER_RIJNDAEL_128_CBC GNUTLS_CIPHER_AES_128_CBC
   --  #define GNUTLS_CIPHER_RIJNDAEL_256_CBC GNUTLS_CIPHER_AES_256_CBC
   --  #define GNUTLS_CIPHER_RIJNDAEL_CBC GNUTLS_CIPHER_AES_128_CBC

   GNUTLS_CIPHER_UNKNOWN          : constant Cipher_Algorithm := 0;
   GNUTLS_CIPHER_NULL             : constant Cipher_Algorithm := 1;
   GNUTLS_CIPHER_ARCFOUR_128      : constant Cipher_Algorithm := 3;
   GNUTLS_CIPHER_ARCFOUR          : constant Cipher_Algorithm
     := GNUTLS_CIPHER_ARCFOUR_128;
   GNUTLS_CIPHER_3DES_CBC         : constant Cipher_Algorithm := 4;
   GNUTLS_CIPHER_AES_128_CBC      : constant Cipher_Algorithm := 5;
   GNUTLS_CIPHER_AES_256_CBC      : constant Cipher_Algorithm := 6;
   GNUTLS_CIPHER_ARCFOUR_40       : constant Cipher_Algorithm := 7;
   GNUTLS_CIPHER_CAMELLIA_128_CBC : constant Cipher_Algorithm := 8;
   GNUTLS_CIPHER_CAMELLIA_256_CBC : constant Cipher_Algorithm := 9;
   GNUTLS_CIPHER_RC2_40_CBC       : constant Cipher_Algorithm := 90;
   GNUTLS_CIPHER_DES_CBC          : constant Cipher_Algorithm := 91;

   --  used only for PGP internals. Ignored in TLS/SSL

   GNUTLS_CIPHER_IDEA_PGP_CFB        : constant Cipher_Algorithm := 200;
   GNUTLS_CIPHER_3DES_PGP_CFB        : constant Cipher_Algorithm := 201;
   GNUTLS_CIPHER_CAST5_PGP_CFB       : constant Cipher_Algorithm := 202;
   GNUTLS_CIPHER_BLOWFISH_PGP_CFB    : constant Cipher_Algorithm := 203;
   GNUTLS_CIPHER_SAFER_SK128_PGP_CFB : constant Cipher_Algorithm := 204;
   GNUTLS_CIPHER_AES128_PGP_CFB      : constant Cipher_Algorithm := 205;
   GNUTLS_CIPHER_AES192_PGP_CFB      : constant Cipher_Algorithm := 206;
   GNUTLS_CIPHER_AES256_PGP_CFB      : constant Cipher_Algorithm := 207;
   GNUTLS_CIPHER_TWOFISH_PGP_CFB     : constant Cipher_Algorithm := 208;

   type Compression_Method is new Interfaces.C.int;
   type Compression_Method_Array
      is array (Positive range <>) of Compression_Method;

   GNUTLS_COMP_UNKNOWN : constant Compression_Method := 0;
   GNUTLS_COMP_NULL    : constant Compression_Method := 1;
   GNUTLS_COMP_DEFLATE : constant Compression_Method := 2;
   GNUTLS_COMP_ZLIB    : constant Compression_Method := GNUTLS_COMP_DEFLATE;
   GNUTLS_COMP_LZO     : constant Compression_Method := 3;
   --  only available if gnutls-extra has  been initialized

   type Mac_Algorithm is new Interfaces.C.int;
   type Mac_Algorithm_Array is array (Positive range <>) of Mac_Algorithm;

   --  #define GNUTLS_DIG_SHA GNUTLS_DIG_SHA1

   GNUTLS_MAC_UNKNOWN : constant Mac_Algorithm := 0;
   GNUTLS_MAC_NULL    : constant Mac_Algorithm := 1;
   GNUTLS_MAC_MD5     : constant Mac_Algorithm := 2;
   GNUTLS_MAC_SHA1    : constant Mac_Algorithm := 3;
   GNUTLS_MAC_SHA     : constant Mac_Algorithm := GNUTLS_MAC_SHA1;
   GNUTLS_MAC_RMD160  : constant Mac_Algorithm := 4;
   GNUTLS_MAC_MD2     : constant Mac_Algorithm := 5;
   GNUTLS_MAC_SHA256  : constant Mac_Algorithm := 6;
   GNUTLS_MAC_SHA384  : constant Mac_Algorithm := 7;
   GNUTLS_MAC_SHA512  : constant Mac_Algorithm := 8;
   --  If you add anything here, make sure you align with
   --  gnutls_digest_algorithm_t, in particular SHA-224.

   type Session is private;

   type Anon_Client_Credentials is private;

   type Certificate_Client_Credentials is private;

   procedure Global_Init;

   procedure Anon_Allocate_Client_Credentials
     (SC : out Anon_Client_Credentials);

   procedure Init (S : out Session; CE : Connection_End);

   procedure Priority_Set_Direct (S : Session;
                                  Priorities : String;
                                  Error_Pos  : out String);

   --  Cred should be void or some base type
   procedure Credentials_Set (S    : Session;
                              T    : Credentials_Type;
                              Cred : Anon_Client_Credentials);

   --  Cred should be void or some base type
   procedure Credentials_Set (S    : Session;
                              T    : Credentials_Type;
                              Cred : Certificate_Client_Credentials);

   procedure Transport_Set_Ptr (S      : Session;
                                Socket : GNAT.Sockets.Socket_Type);

   procedure Handshake (S : Session);

   procedure Record_Send (S      : Session;
                          Data   : GNAT.Sockets.Vector_Type;
                          Length : out Ada.Streams.Stream_Element_Count);

   procedure Record_Recv (S      : Session;
                          Data   : GNAT.Sockets.Vector_Type;
                          Length : out Ada.Streams.Stream_Element_Count);

   procedure Bye  (S : Session; How : Close_Request);

   procedure Deinit (S : Session);

   procedure Anon_Free_Client_Credentials (Cert : Anon_Client_Credentials);

   procedure Global_Deinit;

   procedure Global_Set_Log_Level (Level : Integer);

   procedure Set_Default_Priority (S : Session);

   procedure Certificate_Allocate_Credentials
     (SC : out Certificate_Client_Credentials);

   type IO_Direction is (Read, Write);

   function Get_Direction (S : Session) return IO_Direction;

   function Error_Is_Fatal (E : Integer) return Error_Kind;

private

   type Session_Int is null record;
   pragma Convention (C, Session_Int);

   type Session is access all Session_Int;
   Null_Session : constant Session := null;

   type Anon_Client_Credentials_Record is null record;
   pragma Convention (C, Anon_Client_Credentials_Record);

   type Anon_Client_Credentials is access all Anon_Client_Credentials_Record;

   type Certificate_Client_Credentials_Record is null record;
   pragma Convention (C, Certificate_Client_Credentials_Record);

   type Certificate_Client_Credentials
      is access all Certificate_Client_Credentials_Record;

end GNUTLS;
