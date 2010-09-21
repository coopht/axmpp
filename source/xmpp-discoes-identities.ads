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
--  <Unit> XMPP.Discoes.Identities
--  <ImplementationNotes>
--
------------------------------------------------------------------------------
--  $Revision$ $Author$
--  $Date$
------------------------------------------------------------------------------
with League.Strings;

package XMPP.Discoes.Identities is

   type Category_Type is
     (Account,
      Auth,
      Automation,
      Client,
      Collaboration,
      Component,
      Conference,
      Directory,
      Gateway,
      Headline,
      Hierarchy,
      Proxy,
      Pubsub,
      Server,
      Store);

   --  The "account" category is to be used by a server when responding
   --  to a disco request sent to the bare JID (user@host addresss)
   --  of an account hosted by the server.
   type Account_Type is
     (Admin,
      Anonymous,
      Registered);

   --  The "auth" category consists of server components that provide
   --  authentication services within a server implementation.
   type Auth_Type is
     (Cert,
      Generic_Auth,
      LDAP,
      NTLM,
      PAM,
      Radius);

   --  The "automation" category consists of entities and nodes that provide
   --  automated or programmed interaction.
   type Automation_Type is
     (Command_List,
      Command_Node,
      RPC,
      SOAP,
      Translation);

   --  The "client" category consists of different types
   --  of clients, mostly for instant messaging.
   type Client_Type is
     (Bot,
      Console,
      Handheld,
      Pc,
      Phone,
      Web);

   --  The "collaboration" category consists of services that enable multiple
   --  individuals to work together in real time.
   type Collaboration_Type is
     (Whiteboard);

   --  The "component" category consists of services that are internal to
   --  server implementations and not normally exposed outside a server.
   type Component_Type is
     (Archive,
      C2S,
      Generic_Component,
      Load,
      Log,
      Presence,
      Router,
      S2S,
      SM,
      Stats);

   --  The "conference" category consists of online conference services such
   --  as multi-user chatroom services.
   type Conference_Type is
     (IRC,
      Text);

   --  The "directory" category consists of information retrieval services
   --  that enable users to search online directories or otherwise be
   --  informed about the existence of other XMPP entities.
   type Directory_Type is
     (Chatroom,
      Group,
      User,
      Waitinglist);

   --  The "gateway" category consists of translators between Jabber/XMPP
   --  services and non-XMPP services.
   type Gateway_Type is
     (AIM,
      Facebook,
      Gadu_Gadu,
      HTTP_WS,
      ICQ,
      IRC,
      LCS,
      MRIM,
      MSN,
      Myspaceim,
      OCS,
      QQ,
      Sametime,
      Simple,
      Skype,
      SMS,
      SMTP,
      Tlen,
      Xfire,
      XMPP,
      Yahoo);

   --  The "headline" category consists of services that provide real-time
   --  news or information (often but not necessarily in a message of type
   --  "headline").
   type Headline_Type is
     (Newmail,
      Rss,
      weather);

   --  The "hierarchy" category is used to describe nodes within a hierarchy
   --  of nodes; the "branch" and "leaf" types are exhaustive.
   type Hierarchy_Type is
     (Branch,
      Leaf);

   --  The "proxy" category consists of servers or services that act as
   --  special-purpose proxies or intermediaries between two or more XMPP
   --  endpoints.
   type Proxy_Type is
     (Bytestreams);

   --  Services and nodes that adhere to XEP-0060.
   type Pubsub_Type is
     (Collection,
      Leaf,
      Pep,
      Service);

   --  The "server" category consists of any Jabber/XMPP server.
   type Server_Type is
     (IM);

   --  The "store" category consists of internal server components that
   --  provide data storage and retrieval services.
   type Store_Type is
     (Berkeley,
      File,
      Generic_Store,
      Ldap,
      Mysql,
      Oracle,
      Postgres);

   --  XXX: May be this type is too complex;
   type Identity (Category : Category_Type) is record
      case Category is
         when Account =>
            Account : Account_Type;

         when Auth =>
            Auth : Auth_Type;

         when Automation =>
            Automation : Automation_Type;

         when Client =>
            Client : Client_Type;

         when Collaboration =>
            Collaboration : Collaboration_Type;

         when Component =>
            Component : Component_Type;

         when Conference =>
            Conference : Conference_Type;

         when Directory =>
            Directory : Directory_Type;

         when Gateway =>
            Gateway : Gateway_Type;

         when Headline =>
            Headline : Headline_Type;

         when Hierarchy =>
            Hierarchy : Hierarchy_Type;

         when Proxy =>
            Proxy : Proxy_Type;

         when Pubsub =>
            Pubsub : Pubsub_Type;

         when Server =>
            Server : Server_Type;

         when Store =>
            Store : Store_Type;

      end case;

   end record;

end XMPP.Discoes.Identities;

