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
--% Open_Weather_Map.API.Service.Weather
--
--% @description
--% Provides the query object implementing a single id based query.
--------------------------------------------------------------------------------
package Open_Weather_Map.API.Service.Weather is

   -----------------------------------------------------------------------------
   --  API: Current weather data (by city ID)
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
      Id                 : in       City_Id);
   --% Initializes an instance of a single query.
   --
   --% @param Self
   --% Instance of the single query to initialize.
   --
   --% @param Configuration
   --% Configuration data object containing connection relevant data (proxy
   --% server, API key, etc.).
   --
   --% @param Connection
   --% The connection to be used for client server communication.
   --
   --% @param Max_Cache_Interval
   --% Denotes the maximum frequency at which actual queries are being sent to
   --% the server.
   --
   --% @param Id
   --% Location id of the place to be queried.

private

   type T is new API. Service.T with null record;

   -----------------------------------------------------------------------------
   --  Decode_Response
   -----------------------------------------------------------------------------
   overriding function Decode_Response
     (Self : in T;
      Root : in GNATCOLL.JSON.JSON_Value) return Data_Set;
   --% Decodes a single query response from the server.
   --
   --% @param Self
   --% The single query instance.
   --
   --% @param Root
   --% Root of the JSON data sent back by the server.
   --
   --% @return
   --% The data set decoded from the response in Root.

end Open_Weather_Map.API.Service.Weather;
