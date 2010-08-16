
package XML.SAX.Input_Sources.Streams.Sockets.Debug is

   type Debug_Socket_Input_Source is new Socket_Input_Source with null record;

   overriding procedure Next
    (Self        : in out Debug_Socket_Input_Source;
     Buffer      : in out
       not null Matreshka.Internals.Strings.Shared_String_Access;
     End_Of_Data : out Boolean);

end XML.SAX.Input_Sources.Streams.Sockets.Debug;
