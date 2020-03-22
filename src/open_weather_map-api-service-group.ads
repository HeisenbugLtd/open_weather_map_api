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
--% Open_Weather_Map.API.Service.Group
--
--% @description
--% Provides the query object implementing a group query (i.e. a list of Ids for
--% which weather information shall be retrieved).
--------------------------------------------------------------------------------
package Open_Weather_Map.API.Service.Group is

   -----------------------------------------------------------------------------
   --  API: Current weather data (by list of city IDs)
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
      Ids                : in       Group_List);
   --% Initializes the group query instance.
   --
   --% @param Self
   --% The instance of the group query to initialize.
   --
   --% @param Configuration
   --% Used to configure necessary internals (proxy server etc.).
   --
   --% @param Connection
   --% Access to the connection object to be used for client server
   --% communication.
   --
   --% @param Max_Cache_Interval
   --% Denotes the maximum frequency of actual queries sent to the server.
   --
   --% @param Ids
   --% The list of location ids to be queried.

private

   type T is new Service.T with null record;

   -----------------------------------------------------------------------------
   --  Decode_Response
   -----------------------------------------------------------------------------
   overriding function Decode_Response
     (Self : in T;
      Root : in GNATCOLL.JSON.JSON_Value) return Data_Set;
   --% Decodes a group query response.
   --
   --% @param Self
   --% The group query instance.
   --
   --% @param Root
   --% The root of the JSON response sent by the server.
   --
   --% @return
   --% The data set decoded from the JSON data in Root.

end Open_Weather_Map.API.Service.Group;
