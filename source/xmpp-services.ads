------------------------------------------------------------------------------
--                                                                          --
--                              AXMPP Project                               --
--                                                                          --
--                           XMPP Library for Ada                           --
--                                                                          --
------------------------------------------------------------------------------
--                                                                          --
-- Copyright © 2011-2016, Alexander Basov <coopht@gmail.com>                --
-- All rights reserved.                                                     --
--                                                                          --
-- Redistribution and use in source and binary forms, with or without       --
-- modification, are permitted provided that the following conditions       --
-- are met:                                                                 --
--                                                                          --
--  * Redistributions of source code must retain the above copyright        --
--    notice, this list of conditions and the following disclaimer.         --
--                                                                          --
--  * Redistributions in binary form must reproduce the above copyright     --
--    notice, this list of conditions and the following disclaimer in the   --
--    documentation and/or other materials provided with the distribution.  --
--                                                                          --
--  * Neither the name of the Alexander Basov, IE nor the names of its      --
--    contributors may be used to endorse or promote products derived from  --
--    this software without specific prior written permission.              --
--                                                                          --
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS      --
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT        --
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR    --
-- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT     --
-- HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,   --
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED --
-- TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR   --
-- PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF   --
-- LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING     --
-- NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS       --
-- SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.             --
--                                                                          --
------------------------------------------------------------------------------
--  $Revision$ $Date$
------------------------------------------------------------------------------
with Ada.Containers.Vectors;

with League.Strings;

with XML.SAX.Pretty_Writers;

with XMPP.IQS;
with XMPP.Services_Features;
with XMPP.Services_Identities;

package XMPP.Services is

   Query_Element : constant League.Strings.Universal_String
     := League.Strings.To_Universal_String ("query");

   type Service_Item is record
      JID  : League.Strings.Universal_String;
      Name : League.Strings.Universal_String;
      Node : League.Strings.Universal_String;
   end record;

   package Service_Items_Package is
      new Ada.Containers.Vectors (Natural, Service_Item);

   type XMPP_Service is new XMPP.IQS.XMPP_IQ with private;

   type XMPP_Service_Access is access all XMPP_Service'Class;

   overriding function Get_Kind (Self : XMPP_Service) return Object_Kind;

   overriding procedure Serialize
    (Self   : XMPP_Service;
     Writer : in out XML.SAX.Pretty_Writers.XML_Pretty_Writer'Class);

   overriding
   procedure Set_Content (Self      : in out XMPP_Service;
                          Parameter : League.Strings.Universal_String;
                          Value     : League.Strings.Universal_String);

   function Create return not null XMPP_Service_Access;

   function Get_Type (Self : XMPP_Service) return XMPP.Feature;

   procedure Set_Type (Self : in out XMPP_Service;
                       Val  : XMPP.Feature);

   function Get_Identities (Self : XMPP_Service)
      return XMPP.Services_Identities.Identities_Vector;

   function Get_Features (Self : XMPP_Service)
      return XMPP.Services_Features.Features_Vector;

   procedure Add_Identity (Self : in out XMPP_Service;
                           Val  : XMPP.Services_Identities.Identity);

   procedure Add_Feature (Self : in out XMPP_Service;
                          Val  : XMPP.Feature);

   procedure Add_Feature (Self : in out XMPP_Service;
                          Val  : League.Strings.Universal_String);

   function Get_Items (Self : XMPP_Service)
      return Service_Items_Package.Vector;

   procedure Add_Item (Self : in out XMPP_Service; Item : Service_Item);

private

   type XMPP_Service is new XMPP.IQS.XMPP_IQ with
   record
      Type_Of_Service : XMPP.Feature;
      Identities      : XMPP.Services_Identities.Identities_Vector;
      Features        : XMPP.Services_Features.Features_Vector;
      Items           : Service_Items_Package.Vector;
   end record;

end XMPP.Services;
