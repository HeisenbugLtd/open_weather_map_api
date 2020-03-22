--------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+owm@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
--------------------------------------------------------------------------------
pragma License (Unrestricted);

--------------------------------------------------------------------------------
--% @summary
--% Open_Weather_Map.API.Service.Location
--
--% @description
--% Provides the query object implementing a location based query (i.e. a
--% geographical coordinate given as latitude and longitude).
--------------------------------------------------------------------------------
package Open_Weather_Map.API.Service.Location is

   -----------------------------------------------------------------------------
   --  API: Current weather data (by location)
   -----------------------------------------------------------------------------
   type T is new Service.T with private;

   -----------------------------------------------------------------------------
   --  Initialize
   -----------------------------------------------------------------------------
   procedure Initialize
     (Self               :    out   T;
      Configuration      : in       GNATCOLL.JSON.JSON_Value;
      Connection         : not null Client.T_Access;
      Max_Cache_Interval : in       Ada.Real_Time.Time_Span := Default_Cache_Interval;
      Coordinates        : in       Geo_Coordinates);
   --% Initializes the location query object.
   --
   --% @param Self
   --% Instance of the location query to initialize.
   --
   --% @param Configuration
   --% Connection specific configuration data (e.g. proxy server, API key).
   --
   --% @param Connection
   --% The connection to be used for client server communication.
   --
   --% @param Max_Cache_Interval
   --% Denotes the maximum frequency at which actual HTTP queries are being sent
   --% to the server.
   --
   --% @param Coordinates
   --% The geographical coordinates of the place to be queried.

private

   type T is new Service.T with null record;

   -----------------------------------------------------------------------------
   --  Decode_Response
   -----------------------------------------------------------------------------
   overriding function Decode_Response
     (Self : in T;
      Root : in GNATCOLL.JSON.JSON_Value) return Data_Set;
   --% Decodes a location query.
   --
   --% @param Self
   --% Instance of the location query object.
   --
   --% @param Root
   --% Root of the JSON data sent back by the server.
   --
   --% @return
   --% The data set decoded from the server's response.

end Open_Weather_Map.API.Service.Location;
