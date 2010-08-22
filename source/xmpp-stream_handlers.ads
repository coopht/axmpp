with XMPP.Streams;
with XMPP.Stream_Features;

package XMPP.Stream_Handlers is

   type XMPP_Stream_Handler is limited interface;

   type XMPP_Stream_Handler_Access is access all XMPP_Stream_Handler'Class;

   not overriding procedure Connected
     (Self   : in out XMPP_Stream_Handler;
      Object : XMPP.Stream_Features.XMPP_Stream_Feature_Access)
      is null;
   --  Connected handler is called after succesfull authentification

   not overriding procedure Start_Stream
     (Self   : in out XMPP_Stream_Handler;
      Object : not null XMPP.Streams.XMPP_Stream_Access) is null;

   not overriding procedure Stream_Features
     (Self   : in out XMPP_Stream_Handler;
      Object : not null XMPP.Stream_Features.XMPP_Stream_Feature_Access)
      is null;

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
