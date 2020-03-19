--------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+owm@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
--------------------------------------------------------------------------------
pragma License (Unrestricted);

--------------------------------------------------------------------------------
--  Open_Weather_Map.API.Service.Group
--
--  Provides the query object implementing a group query (i.e. a list of Ids for
--  which weather information shall be retrieved).
--------------------------------------------------------------------------------

package Open_Weather_Map.API.Service.Group is

   -----------------------------------------------------------------------------
   --  API: Current weather data (by list of city IDs)
   -----------------------------------------------------------------------------
   type T is new Service.T with private;

   procedure Initialize
     (Context            :    out   T;
      Configuration      : in       GNATCOLL.JSON.JSON_Value;
      Connection         : not null Client.T_Access;
      Max_Cache_Interval : in       Ada.Real_Time.Time_Span := Default_Cache_Interval;
      Ids                : in       Group_List);
   --  Initializes the object.
   --  Configuration is used to configure necessary internals (proxy server
   --  etc.).
   --  Connection is an access to the connection object to be used.
   --  Max_Cache_Interval denotes the frequency of actual queries sent to the
   --  server.
   --  Ids is the list of location ids to be queried.

private

   type T is new Service.T with null record;

   overriding function Decode_Response
     (Context : in T;
      Root    : in GNATCOLL.JSON.JSON_Value) return Data_Set;

end Open_Weather_Map.API.Service.Group;
