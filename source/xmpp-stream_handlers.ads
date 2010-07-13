
package XMPP.Stream_Handlers is

   type XMPP_Stream_Handler is limited interface;

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
