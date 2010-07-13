with Ada.Streams;

package XMPP.Raw_Handlers is

   type XMPP_Raw_Handler is limited interface;

   not overriding procedure Data
     (Self : in out XMPP_Raw_Handler;
      Data : Ada.Streams.Stream_Element_Array) is null;

end XMPP.Raw_Handlers;
