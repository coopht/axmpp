
package XMPP.Stream_Handlers is

   type XMPP_Stream_Handler is limited interface;

   type XMPP_Stream_Handler_Access is access all XMPP_Stream_Handler;

   not overriding procedure Start_Stream
     (Self : in out XMPP_Stream_Handler) is null;

   not overriding procedure Error
     (Self : in out XMPP_Stream_Handler) is null;

   not overriding procedure Message
     (Self : in out XMPP_Stream_Handler) is null;

   not overriding procedure Presence
     (Self : in out XMPP_Stream_Handler) is null;

   not overriding procedure IQ
     (Self : in out XMPP_Stream_Handler) is null;

   not overriding procedure End_Stream
     (Self : in out XMPP_Stream_Handler) is null;

end XMPP.Stream_Handlers;
