--------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+owm@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
--------------------------------------------------------------------------------
pragma License (Unrestricted);

limited with Ada.Calendar.Time_Zones;
limited with Ada.Containers.Vectors;
limited with Ada.Real_Time;
limited with Ada.Strings.Unbounded;
with Types;

private with Ada.Characters.Handling;
private with GNATCOLL.Traces;

--------------------------------------------------------------------------------
--% @summary
--% Top-level package of the implementation to access openweathermap.org's API.
--
--% @description
--% Provides basic types, restrictions, and information about the API, its
--% implementation, and limitations.
--------------------------------------------------------------------------------
package Open_Weather_Map is

   -----------------------------------------------------------------------------
   --  API restrictions as documented on their web page
   -----------------------------------------------------------------------------

   Default_Cache_Interval : constant Ada.Real_Time.Time_Span :=
     Ada.Real_Time.Seconds (10);
   --% Default value for the time the values of a query shall be retained before
   --% we refresh the data from the server.
   --  According to openweathermap.org/price the limit on API calls per minute
   --  per account (regardless of API keys) is 60 calls even for the free model,
   --  so we can in theory run at least one query each second. To be on the safe
   --  side, we restrict it to a tenth of that, i.e. one query every ten
   --  seconds. This leaves open the possibility to run a couple of queries in
   --  parallel.

   Default_Rate_Limit : constant Ada.Real_Time.Time_Span :=
     Ada.Real_Time.Milliseconds (500);
   --% Default value for the minimum temporal separation of queries sent to the
   --% server over the same connection.
   --  We limit the number of consecutive requests per HTTP connection to be at
   --  least that apart.

   subtype Max_Group_Size is Positive range 1 .. 20;
   --% Maximum number of ids in a group query.
   --  API limitation.

   -----------------------------------------------------------------------------
   --  API types
   -----------------------------------------------------------------------------

   subtype API_Key is String (1 .. 32) with
     Dynamic_Predicate =>
       (for all C of API_Key =>
          Ada.Characters.Handling.Is_Hexadecimal_Digit (C)),
       Predicate_Failure =>
         (raise Constraint_Error with
            """" & API_Key & """ is not a 32 character hexadecimal string");
   --% API key is a hexadecimal representation of a 128 bit value.

   Invalid_API_Key : constant API_Key;
   --% Denotes the all '0' key.
   --  We're presuming that this key will be invalid for anyone.

   type City_Id is range 1 .. 99_999_999;
   --% City ids.
   --  Not sure about the actual range, though.

   -----------------------------------------------------------------------------
   --  Support types
   -----------------------------------------------------------------------------

   type Group_List is array (Max_Group_Size range <>) of City_Id;
   --% Type representing a list of ids (for a group query).

   type Geo_Coordinates is
      record
         Latitude  : Types.Latitude;
         Longitude : Types.Longitude;
      end record;
   --% Type representing geographical 2-D coordinates.
   --% @field Latitude
   --% The latitude part of the coordinates.
   --% @field Longitude
   --% The longitude part of the coordinates.

   type City_Data is
      record
         Location    : Geo_Coordinates;
         Temperature : Types.Kelvin;
         Humidity    : Types.Humidity;
         Pressure    : Types.Pressure;
         Name        : Ada.Strings.Unbounded.Unbounded_String;
         Sunrise     : Ada.Calendar.Time;
         Sunset      : Ada.Calendar.Time;
         Time_Zone   : Ada.Calendar.Time_Zones.Time_Offset;
         Last_Update : Ada.Calendar.Time;
      end record;
   --% Type representing the set of data returned as per city/location.
   --  May depend on the query issued, but that's the general idea.
   --% @field Location
   --% The geographical location of the city/location.
   --% @field Temperature
   --% The temperature at the location.
   --% @field Humidity
   --% The relative humidity at the location.
   --% @field Pressure
   --% The atmospheric pressure at the location.
   --% @field Name
   --% Name of the city (may be empty, if no city is associated with the
   --% coordinates).
   --% @field Sunrise
   --% UTC of sunrise at that location.
   --% @field Sunset
   --% UTC of sunset at that location.
   --% @field Time_Zone
   --% Local time zone of the location.
   --% @field Last_Update
   --% UTC of when the data was last updated on the server.

   package City_Lists is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                     Element_Type => City_Data);

   --% @field Valid
   --% Indicates if the object actually contains data.
   --% @field Cities
   --% List of cities in the data set.
   type Data_Set (Valid : Boolean := False) is
      record
         case Valid is
            when False =>
               null;
            when True =>
               Cities : City_Lists.Vector;
         end case;
      end record;
   --  TODO: This should become either a tagged type, or part of the query
   --        objects with suitable accessor functions to cater for the dynamic
   --        nature of the data being returned.

   Invalid_Data_Set : constant Data_Set;
   --% An empty (invalid) data set.

   -----------------------------------------------------------------------------
   --  Enumeration of (implemented) API services.
   -----------------------------------------------------------------------------

   type API_Services is
     (Current_By_Id,          --  "weather?id={city_id}"
      Current_By_Coordinates, --  "weather?lat={latitude}&lon={longitude}"
      Current_By_Group);      --  "group?id={city_id}[,{city_id} ...]
   --% List of implemented API services.
   --% @value Current_By_Id
   --% Current weather data for a city Id.
   --% @value Current_By_Coordinates
   --% Current weather data for a location.
   --% @value Current_By_Group
   --% Current weather data for a list of city ids.

   --
   --  Utility functions
   --

   -----------------------------------------------------------------------------
   --  Application_Directory
   -----------------------------------------------------------------------------
   function Application_Directory return String with
     Global => null;
   --% Returns the (system-dependent) local directory where the application
   --% expects the configuration and log files and such.
   --% @return The directory where configuration and log files should be stored.
   --  The underlying implementation uses a constant evaluated at elaboration
   --  time, hence the Global aspect of null.

private

   OWM_Debug : constant not null GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create (Unit_Name => "Open_Weather_Map");
   --% Debug trace for top level package.

   API_Host : constant String := "http://api.openweathermap.org";
   --% The actual server we're supposed to talk to.

   API_Path : constant String := "/data/2.5/";
   --% Query path to the currently implemented version of the API.

   Invalid_API_Key : constant API_Key := API_Key'(others => '0');

   Invalid_Data_Set : constant Data_Set := Data_Set'(Valid => False);

   -----------------------------------------------------------------------------
   --  Config_Names
   --
   --  Expected names of fields in the configuration file.
   --  Nested package to improve readability when using these constants.
   -----------------------------------------------------------------------------
   package Config_Names is

      --  Proxy configuration fields.
      Env_Network_Address   : constant String := "http_proxy";
      Field_Network_Address : constant String := "proxy.url";
      Field_User            : constant String := "proxy.user";
      Field_Password        : constant String := "proxy.password";

      --  Account configuration fields.
      Field_API_Key : constant String := "api.key";

   end Config_Names;

   -----------------------------------------------------------------------------
   --  To_Service_Name
   -----------------------------------------------------------------------------
   --% @param Service The service to be translated into an API URL.
   --% @return The part of the API URL denoting the name of the service being
   --%         called.
   function To_Service_Name (Service : in API_Services) return String with
     Global => null;

   -----------------------------------------------------------------------------
   --  To_Service_Name
   -----------------------------------------------------------------------------
   function To_Service_Name (Service : in API_Services) return String is
     (case Service is
         when Current_By_Id
           | Current_By_Coordinates => "weather",
         when Current_By_Group      => "group");

end Open_Weather_Map;
