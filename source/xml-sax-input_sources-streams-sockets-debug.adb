with League.Strings.Internals;
with Matreshka.Internals.Strings.Operations;
with Matreshka.Internals.Utf16;

with XMPP.Sessions;

package body XML.SAX.Input_Sources.Streams.Sockets.Debug is

   ----------
   -- Next --
   ----------

   overriding procedure Next
    (Self        : in out Debug_Socket_Input_Source;
     Buffer      : in out
       not null Matreshka.Internals.Strings.Shared_String_Access;
     End_Of_Data : out Boolean)
   is
      use type Matreshka.Internals.Utf16.Utf16_String_Index;

      Old_Unused : constant Matreshka.Internals.Utf16.Utf16_String_Index
        := Buffer.Unused;
      Old_Length : constant Natural := Buffer.Length;
      New_Data   : Matreshka.Internals.Strings.Shared_String_Access;

   begin
      Socket_Input_Source (Self).Next (Buffer, End_Of_Data);
      New_Data :=
        Matreshka.Internals.Strings.Operations.Slice
         (Buffer,
          Old_Unused,
          Buffer.Unused - Old_Unused,
          Buffer.Length - Old_Length);
      XMPP.Sessions.Put_Line (League.Strings.Internals.Wrap (New_Data));
   end Next;

end XML.SAX.Input_Sources.Streams.Sockets.Debug;
