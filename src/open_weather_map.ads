--------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+owm@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
--------------------------------------------------------------------------------
pragma License (Unrestricted);

--------------------------------------------------------------------------------
--  Open_Weather_Map
--
--  Top-level package of the implementation to the openweathermap.org API.
--
--  Provides basic types, restrictions, and information about the API and its
--  implementation.
--------------------------------------------------------------------------------

limited with Ada.Calendar.Time_Zones;
limited with Ada.Containers.Vectors;
limited with Ada.Real_Time;
limited with Ada.Strings.Unbounded;
with Types;

private with Ada.Characters.Handling;
private with GNATCOLL.Traces;

package Open_Weather_Map is

   -----------------------------------------------------------------------------
   --  API restrictions as documented on their web page
   -----------------------------------------------------------------------------

   Default_Cache_Interval : constant Ada.Real_Time.Time_Span :=
     Ada.Real_Time.Seconds (10);
   --  According to openweathermap.org/price the limit on API calls per minute
   --  per account (regardless of API keys) is 60 calls even for the free model,
   --  so we can in theory run at least one query each second. To be on the safe
   --  side, we restrict it to a tenth of that, i.e. one query every ten
   --  seconds. This leaves open the possibility to run a couple of queries in
   --  parallel.

   Default_Rate_Limit : constant Ada.Real_Time.Time_Span :=
     Ada.Real_Time.Milliseconds (500);
   --  We limit the number of consecutive requests per HTTP connection to be at
   --  least that apart.

   subtype Max_Group_Size is Positive range 1 .. 20;
   --  Maximum number of ids in a group query.

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
   --  API key seems to be the hexadecimal representation of a 128 bit value.

   Invalid_API_Key : constant API_Key;
   --  Denotes the all '0' key.  We're presuming that this key will be invalid
   --  for anyone.

   type City_Id is range 1 .. 99_999_999;
   --  City ids (not sure about the actual range).

   -----------------------------------------------------------------------------
   --  Support types
   -----------------------------------------------------------------------------

   type Group_List is array (Max_Group_Size range <>) of City_Id;

   type Geo_Coordinates is
      record
         Latitude  : Types.Latitude;
         Longitude : Types.Longitude;
      end record;

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
   --  Data returned as per city (may depend on query issued, but that's the
   --  general idea).

   package City_Lists is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                     Element_Type => City_Data);

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

   -----------------------------------------------------------------------------
   --  Enumeration of (implemented) API services.
   -----------------------------------------------------------------------------

   type API_Services is
     (Current_By_Id,          --  "weather?id={city_id}"
      Current_By_Coordinates, --  "weather?lat={latitude}&lon={longitude}"
      Current_By_Group);      --  "group?id={city_id}[,{city_id} ...]

   -----------------------------------------------------------------------------
   --  Utility functions
   -----------------------------------------------------------------------------
   function Application_Directory return String with
     Global => null;
   --  Returns the (system-dependent) local directory where the application
   --  expects the configuration and log files and such.
   --
   --  The underlying implementation uses a constant evaluated at elaboration
   --  time, hence the Global aspect of null.

private

   OWM_Debug : constant not null GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create (Unit_Name => "Open_Weather_Map");

   API_Host : constant String := "http://api.openweathermap.org";
   --  The actual server we're supposed to talk to.

   API_Path : constant String := "/data/2.5/";
   --  Query path to the currently implemented version of the API.

   Invalid_API_Key : constant API_Key := API_Key'(others => '0');
   --  Use the all-zero key for an invalid key value.

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

   function To_Service_Name (Service : in API_Services) return String with
     Global => null;
   --  Returns the part of the API URL denoting the name of the service being
   --  called.

   function To_Service_Name (Service : in API_Services) return String is
     (case Service is
         when Current_By_Id
           | Current_By_Coordinates => "weather",
         when Current_By_Group      => "group");

end Open_Weather_Map;
