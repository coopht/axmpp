with XMPP.Stream_Handlers;
with XMPP.Raw_Handlers;

package XMPP.Sessions is

   type XMPP_Session is tagged limited private;

   procedure Set_Stream_Handler
    (Self    : XMPP_Session;
     Handler : not null access XMPP.Stream_Handlers.XMPP_Stream_Handler'Class)
       is null;

   procedure Set_Raw_Handler
    (Self    : XMPP_Session;
     Handler : not null access XMPP.Raw_Handlers.XMPP_Raw_Handler'Class)
       is null;

private

   type XMPP_Session is tagged limited null record;

end XMPP.Sessions;
