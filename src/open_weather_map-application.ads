--------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+owm@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
--------------------------------------------------------------------------------
pragma License (Unrestricted);

with Standard_Application;

private with Ada.Containers.Indefinite_Vectors;
private with Open_Weather_Map.API;
private with Open_Weather_Map.Client;
private with Open_Weather_Map.Configuration;

--------------------------------------------------------------------------------
--% @summary
--% A sample application showing the use of the openweathermap API.
--------------------------------------------------------------------------------
package Open_Weather_Map.Application is

   type T is new Standard_Application.T with private;

   -----------------------------------------------------------------------------
   --  Initialize
   -----------------------------------------------------------------------------
   not overriding procedure Initialize (Self : in out T);
   --% Initializes the application Self.
   --
   --% @param Self
   --% Instance of application to initialize.

   -----------------------------------------------------------------------------
   --  Shutdown
   -----------------------------------------------------------------------------
   overriding procedure Shutdown (Self : in out T);
   --% Shuts down the application Self.
   --
   --% @param Self
   --% Instance of application to shut down.

   -----------------------------------------------------------------------------
   --  Work
   -----------------------------------------------------------------------------
   overriding procedure Work (Self : in out T);
   --% The work procedure (i.e. main loop) of the application.
   --
   --% @param Self
   --% Instance of application to run.

private

   --% Query data set stored within the application.
   --% Contains the new data received from a query and the previous data set.
   --
   --% @field Query
   --% Access discriminant pointing to the query updating the data set.
   type Query_Data_Set (Query : API.API_Class_Access) is
      record
         Data          : Data_Set;
         --% @field Data
         --% The current data set.
         Previous_Data : Data_Set;
         --% @field Previous_Data
         --% The previous data set.
      end record;

   package Queries is new
     Ada.Containers.Indefinite_Vectors (Index_Type   => Positive,
                                        Element_Type => Query_Data_Set);

   type T is new Standard_Application.T with
      record
         Configuration : Open_Weather_Map.Configuration.T;
         --% @field Configuration
         --% Configuration data set.
         Connection    : Client.T_Access;
         --% @field Connection
         --% The HTTP server connection used for queries.
         API_Calls     : Queries.Vector;
         --% @field API_Calls
         --% The API calls implemented within the application.
      end record;

   -----------------------------------------------------------------------------
   --  Update_And_Report
   -----------------------------------------------------------------------------
   not overriding procedure Update_And_Report (Self : in     T;
                                               Q    : in out Query_Data_Set);
   --% Updates the stored data set via HTTP requests and reports the new data
   --% to the console.
   --
   --% @param Self
   --% Instance of the application running.
   --
   --% @param Q
   --% The query data set to be updated.

   -----------------------------------------------------------------------------
   --  Print
   -----------------------------------------------------------------------------
   procedure Print (City_Name : in String);
   --% Prints the city name to standard output.
   --
   --% @param City_Name
   --% The name of the city to print.

   -----------------------------------------------------------------------------
   --  Print
   -----------------------------------------------------------------------------
   procedure Print (H : in Types.Humidity);
   --% Prints a humidity value to standard output.
   --
   --% @param H
   --% The humidity value to print.

   -----------------------------------------------------------------------------
   --  Print
   -----------------------------------------------------------------------------
   procedure Print (P : in Types.Pressure);
   --% Prints an atmospheric pressure value to standard output.
   --
   --% @param P
   --% The pressure value to print.

   -----------------------------------------------------------------------------
   --  Print
   -----------------------------------------------------------------------------
   procedure Print (T : in Types.Celsius);
   --% Prints a temperature value to standard output.
   --
   --% @param T
   --% The temperature value to print.

   -----------------------------------------------------------------------------
   --  Print_Last_Update_On_Server
   -----------------------------------------------------------------------------
   procedure Print_Last_Update_On_Server
     (Value     : in Ada.Calendar.Time;
      Time_Zone : in Ada.Calendar.Time_Zones.Time_Offset);
   --% Prints the time of the last server update to standard output.
   --
   --% @param Value
   --% The time of the update.
   --
   --% @param Time_Zone
   --% The local time zone for Value.

   -----------------------------------------------------------------------------
   --  Print_Location
   -----------------------------------------------------------------------------
   procedure Print_Location (Loc : in Open_Weather_Map.Geo_Coordinates);
   --% Prints the geographical coordinates to standard output.
   --
   --% @param Loc
   --% The location to print.

   -----------------------------------------------------------------------------
   --  Print_Seconds_Since_Last_Query
   -----------------------------------------------------------------------------
   procedure Print_Seconds_Since_Last_Query
     (Elapsed_Time : in Ada.Real_Time.Time_Span);
   --% Prints the time elapsed since the last HTTP query done to standard
   --% output.
   --
   --% @param Elapsed_Time
   --% The time span since the last query.

   -----------------------------------------------------------------------------
   --  Print_Time
   -----------------------------------------------------------------------------
   procedure Print_Time (Value     : in Ada.Calendar.Time;
                         Time_Zone : in Ada.Calendar.Time_Zones.Time_Offset);
   --% Prints the given time to standard output.
   --
   --% @param Value
   --% The time to print.
   --
   --% @param Time_Zone
   --% The local time zone for Value.

   -----------------------------------------------------------------------------
   --  Print_Timestamp
   -----------------------------------------------------------------------------
   procedure Print_Timestamp (Value : in Ada.Real_Time.Time_Span);
   --% Prints a time stamp to standard output.
   --
   --% @param Value
   --% The time the timestamp shall be created from and printed.

end Open_Weather_Map.Application;
