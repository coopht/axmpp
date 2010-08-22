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
--  <Unit> XMPP.Networks
--  <ImplementationNotes>
--
------------------------------------------------------------------------------
--  $Revision$ $Author$
--  $Date$
------------------------------------------------------------------------------
with Ada.Streams;

with GNAT.Sockets;

with GNUTLS;

package XMPP.Networks is

   use GNAT.Sockets;

   type Network is abstract tagged limited private;

   type Network_Access is access all Network;

   --  functions, which should be overriden
   not overriding
   procedure On_Connect (Self : not null access Network) is abstract;

   not overriding
   procedure On_Disconnect (Self : not null access Network) is abstract;

   not overriding
   procedure On_Recieve (Self   : not null access Network;
                         Data   : Ada.Streams.Stream_Element_Array)
      is null;

   not overriding
   procedure Read_Data (Self : not null access Network);
   --  If this function is not reimplemented, than default implementation is
   --  used. By default, Read_Data reads data from socket in non-blocking mode
   --  end notifies user with read data using On_Recieve function. So, if you
   --  do not plan reimplementation of read_data function, you should
   --  reimplement On_Recieve function to be able to receive data from socket.

   --  end of functions, which should be overriden

   procedure Connect (Self : not null access Network'Class;
                      Host : Wide_Wide_String;
                      Port : Natural);

   procedure Disconnect (Self : not null access Network'Class);

   procedure Send (Self    : not null access Network'Class;
                   Data    : Ada.Streams.Stream_Element_Array;
                   Via_TLS : Boolean := False);

   procedure Idle (Self : in out Network);

   procedure Recieve (Self : not null access Network'Class);

   --  XXX: this function must be removed.
   function Get_Socket (Self : not null access Network'Class)
      return Socket_Type;

   --  XXX: this function must be removed.
   function Get_Channel (Self : not null access Network'Class)
      return Stream_Access;

   procedure Set_TLS_Session (Self : not null access Network'Class;
                              S    : GNUTLS.Session);

private

   type Network is abstract tagged limited record
      Addr         : Sock_Addr_Type;
      Sock         : Socket_Type;
      Channel      : Stream_Access;
      Selector     : Selector_Type;
      Time_To_Stop : Boolean := False;
      WSet         : Socket_Set_Type;
      RSet         : Socket_Set_Type;
      Status       : Selector_Status;
      TLS          : GNUTLS.Session;
   end record;

end XMPP.Networks;
