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
--  <Unit> XML.SAX.Input_Sources.Streams.Sockets.TLS
--  <ImplementationNotes>
--
------------------------------------------------------------------------------
--  $Revision$ $Author$
--  $Date$
------------------------------------------------------------------------------
with GNUTLS;

package XML.SAX.Input_Sources.Streams.Sockets.TLS is

   type TLS_Socket_Input_Source is new Socket_Input_Source with
   record
      TLS_Established : Boolean := False;
      TLS_Session     : GNUTLS.Session;
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

   procedure Set_TLS_Established (Self  : in out TLS_Socket_Input_Source;
                                  Value : Boolean);

   procedure Set_TLS_Session (Self  : in out TLS_Socket_Input_Source;
                              S     : GNUTLS.Session);

end XML.SAX.Input_Sources.Streams.Sockets.TLS;
