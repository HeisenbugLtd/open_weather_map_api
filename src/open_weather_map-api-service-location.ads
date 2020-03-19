--------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+owm@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
--------------------------------------------------------------------------------
pragma License (Unrestricted);

--------------------------------------------------------------------------------
--  Open_Weather_Map.API.Service.Location
--
--  Provides the query object implementing a location based query (i.e. a
--  geographical coordinate given as latitude and longitude).
--------------------------------------------------------------------------------

package Open_Weather_Map.API.Service.Location is

   -----------------------------------------------------------------------------
   --  API: Current weather data (by location)
   -----------------------------------------------------------------------------
   type T is new Service.T with private;

   procedure Initialize
     (Context            :    out   T;
      Configuration      : in       GNATCOLL.JSON.JSON_Value;
      Connection         : not null Client.T_Access;
      Max_Cache_Interval : in       Ada.Real_Time.Time_Span := Default_Cache_Interval;
      Coordinates        : in       Geo_Coordinates);
   --  Initializes Context according to the given Configuration object, using
   --  the given Connection for server queries that will be fired at least
   --  Max_Cache_Interval apart.
   --  Coordinates are the geographical location of the place to be queried.

private

   type T is new Service.T with null record;

   overriding function Decode_Response
     (Context : in T;
      Root    : in GNATCOLL.JSON.JSON_Value) return Data_Set;

end Open_Weather_Map.API.Service.Location;
