--------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+owm@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
--------------------------------------------------------------------------------
pragma License (Unrestricted);

with Ada.Characters.Latin_1;
with Ada.Text_IO;
with AWS.Client;
with AWS.Messages;
with AWS.Response;
with GNATCOLL.JSON;

package body Open_Weather_Map.API.Service is

   My_Debug : constant not null GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create (Unit_Name => "OWM.API.SERVICE");

   use type Ada.Real_Time.Time;

   -----------------------------------------------------------------------------
   --  Initialize
   -----------------------------------------------------------------------------
   procedure Initialize
     (Self               :    out   T;
      Configuration      : in       GNATCOLL.JSON.JSON_Value;
      Connection         : not null Client.T_Access;
      Max_Cache_Interval : in       Ada.Real_Time.Time_Span := Default_Cache_Interval;
      For_API_Service    : in       API_Services) is
   begin
      My_Debug.all.Trace (Message => "Initialize");

      --  inherited initialization
      Query.T (Self).Initialize;

      --  initialization of added fields
      Self.Server_Connection := Connection;
      Self.Cache_Interval    := Max_Cache_Interval;

      --  initialization of added fields.
      Get_API_Key :
      declare
         pragma Assertion_Policy (Dynamic_Predicate => Check);
         --  Force type predicate check on API key type.
      begin
         My_Debug.all.Trace (Message => "Initialize: Loading API key...");
         Self.Key := Configuration.Get (Field => Config_Names.Field_API_Key);
         My_Debug.all.Trace (Message => "Initialize: API key loaded.");
      exception
         when E : others =>
            My_Debug.all.Trace
              (E => E,
               Msg => "Initialize: Error parsing configuration data: ");
            Ada.Text_IO.Put_Line
              (File => Ada.Text_IO.Standard_Error,
               Item =>
                  "Warning: Missing or invalid JSON data, " &
                  "API key configuration is invalid.");
            Self.Key := Invalid_API_Key;
            --  Force API key to invalid, yet satisfy the type predicate.
      end Get_API_Key;

      Self.Service := For_API_Service;
   end Initialize;

   -----------------------------------------------------------------------------
   --  Perform_Query
   -----------------------------------------------------------------------------
   overriding procedure Perform_Query (Self    : in out T;
                                       Current : in out Data_Set)
   is
      Now : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;
      URI : constant String := T'Class (Self).Service_URI;
   begin
      My_Debug.all.Trace (Message => "Query");

      if
        T'Class (Self).Last_Query + T'Class (Self).Cache_Interval < Now
      then
         My_Debug.all.Trace
           (Message => "Query: Firing query: """ & API_Host & URI & """");

         Do_HTTP_Query :
         declare
            Response : AWS.Response.Data;
         begin
            AWS.Client.Get
              (Connection => Self.Server_Connection.all.Connection.all,
               URI        => URI & "&appid=" & T'Class (Self).Key,
               Result     => Response);

            declare
               Status_Code : constant AWS.Messages.Status_Code :=
                 AWS.Response.Status_Code (D => Response);
            begin
               if Status_Code in AWS.Messages.Success then
                  My_Debug.all.Trace
                    (Message =>
                       "Query: Succeeded (" &
                       AWS.Messages.Image (S => Status_Code) & "/" &
                       AWS.Messages.Reason_Phrase (S => Status_Code) & ")");
                  T'Class (Self).Set_Last_Query (Value => Now);
                  --  Mark retrieval of data.

                  --  Decode retrieved JSON data.
                  declare
                     Content : constant String :=
                       AWS.Response.Message_Body (D => Response);
                  begin
                     My_Debug.all.Trace
                       (Message =>
                          "Query: Received response: " &
                          Ada.Characters.Latin_1.LF &
                          """" & Content & """");

                     declare
                        Read_Result : constant GNATCOLL.JSON.Read_Result :=
                          GNATCOLL.JSON.Read (Strm => Content);
                     begin
                        if Read_Result.Success then
                           Current :=
                             T'Class (Self).Decode_Response
                               (Root => Read_Result.Value);
                        else
                           Report_Error :
                           declare
                              Error_Msg : constant String :=
                                GNATCOLL.JSON.Format_Parsing_Error
                                  (Error => Read_Result.Error);
                           begin
                              My_Debug.all.Trace
                                (Message => "Query: " & Error_Msg);
                              Ada.Text_IO.Put_Line
                                (File => Ada.Text_IO.Standard_Error,
                                 Item =>
                                   "Error parsing response: " & Error_Msg);
                           end Report_Error;
                        end if;
                     end;
                  end;
               else
                  --  Error retrieving network data.
                  My_Debug.all.Trace
                    (Message =>
                       "Query: Failed (" & AWS.Messages.Image (Status_Code) &
                       "/" & AWS.Messages.Reason_Phrase (Status_Code) & ")");
                  Ada.Text_IO.Put_Line
                    (File => Ada.Text_IO.Standard_Error,
                     Item =>
                       "API query failed: " &
                       AWS.Messages.Image (Status_Code) & "/" &
                       AWS.Messages.Reason_Phrase (S => Status_Code) & ".");
               end if;
            end;
         end Do_HTTP_Query;
      else
         My_Debug.all.Trace
           (Message => "Query: Within cache interval, nothing to do.");
      end if;
   end Perform_Query;

   -----------------------------------------------------------------------------
   --  Service_URI
   -----------------------------------------------------------------------------
   function Service_URI (This : in T) return String
   is
      Result : constant String :=
        API_Path & To_Service_Name (This.Service) & This.Parameters.URI_Format;
   begin
      My_Debug.all.Trace (Message => "Service_URI: " & Result);

      return Result;
   end Service_URI;

   -----------------------------------------------------------------------------
   --  Set_Cache_Interval
   -----------------------------------------------------------------------------
   procedure Set_Cache_Interval
     (Self               : in out T;
      Max_Cache_Interval : in     Ada.Real_Time.Time_Span) is
   begin
      My_Debug.all.Trace (Message => "Set_Cache_Interval");

      Self.Cache_Interval := Max_Cache_Interval;
   end Set_Cache_Interval;

end Open_Weather_Map.API.Service;
