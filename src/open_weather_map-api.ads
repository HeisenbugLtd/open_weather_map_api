--------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+owm@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
--------------------------------------------------------------------------------
pragma License (Unrestricted);

limited with GNATCOLL.JSON;
with Open_Weather_Map.Client;

--------------------------------------------------------------------------------
--% @summary
--% Open_Weather_Map.API
--% Publicly visible interface to the openweathermap.org API.
--
--% @description
--% Provides the abstract tagged type (i.e. interface) of what any derived API
--% query implementation will provide, and "constructor" subprograms to create
--% such query objects.
--
--  Query objects are immutable in the sense that, once constructed, they are
--  responsible for exactly one specific API call.
--
--    For instance, Create_Current_By_Id will create a query object that can
--    retrieve weather information for a single location/city with the Id given
--    at construction time.  If you need weather information for a location with
--    a different id, you either need to construct another query object with the
--    different id or derive a new tagged type where the id can be changed
--    during the life time of the object of that type.
--------------------------------------------------------------------------------
package Open_Weather_Map.API is

   -----------------------------------------------------------------------------
   --  The abstract base class for the API.  This is the official interface, all
   --  derived types shall be considered private.
   -----------------------------------------------------------------------------

   type T is abstract tagged limited private;
   subtype API_Class is T'Class;

   type API_Class_Access is not null access API_Class;

   -----------------------------------------------------------------------------
   --  Perform_Query
   -----------------------------------------------------------------------------
   procedure Perform_Query (Self    : in out T;
                            Current : in out Data_Set) is abstract;
   --% Operation to fire and evaluate a query as defined by the actual type of
   --% client context Self.
   --
   --% @param Self
   --% The client instance to perform the query.
   --
   --% @param Current
   --% The data set being updated during the query.

   -----------------------------------------------------------------------------
   --  Last_Query
   -----------------------------------------------------------------------------
   function Last_Query (Self : in T) return Ada.Real_Time.Time is abstract;
   --% @param Self
   --% The client instance.
   --
   --% @return
   --% The time of the last query done with the client context This.

   -----------------------------------------------------------------------------
   --  Query object constructing subprograms
   --
   --  All these subprograms follow the same approach.  First, the API specific
   --  parameters (ids, locations, ...), the remaining parameters should be the
   --  same for all constructors.
   --
   --  Configuration : The JSON object holding the configuration data (usually
   --                  read from some external file).
   --  Connection    : If Connection is not null, then the given object will be
   --                  re-used to establish communication with the API server.
   --                  It is the responsibility of the caller to ensure that the
   --                  life time of the connection object is at least the life
   --                  time of the returned query object.
   --                  If Connection is null, then a new connection object will
   --                  be dynamically created.
   --                    FIXME: As of now, there is no automatic storage
   --                           reclamation for these, so if you keep creating
   --                           new query objects with no shared connection, you
   --                           will leak memory.
   --  Cache_Interval: The time interval at which new data is requested from the
   --                  server.  You can call Perform_Query as often as you want
   --                  on the returned object, but as long as the duration
   --                  between such calls is shorter than Cache_Interval, no new
   --                  data will be requested and we simply return the previous
   --                  data set.
   --  Rate_Limit    : Limits the amount of calls to the API server using the
   --                  connection.  Calls to the server must be at least the
   --                  given duration apart, otherwise the call will be delayed
   --                  until at least the given interval has passed. This is
   --                  only really useful in conjunction with shared
   --                  connections, if you have a connection per query object,
   --                  Cache_Interval should take care of any rate limits
   --                  (unless, of course, it's smaller than Rate_Limit).
   --                  Please note that, unlike with Cache_Interval, the caller
   --                  will actually be suspended for a time if the Rate_Limit
   --                  is exceeded.
   --                  Also, keep in mind that the rate limit only applies when
   --                  a new connection object is being created. If you share
   --                  connections, the Rate_Limit in use will be the one from
   --                  the connection given, and this parameter will be ignored.
   -----------------------------------------------------------------------------

   -----------------------------------------------------------------------------
   --  Create_Current_By_Coordinates
   -----------------------------------------------------------------------------
   function Create_Current_By_Coordinates
     (Coordinates    : in Geo_Coordinates;
      Configuration  : in GNATCOLL.JSON.JSON_Value;
      Connection     : in Client.T_Access         := null;
      Cache_Interval : in Ada.Real_Time.Time_Span := Default_Cache_Interval;
      Rate_Limit     : in Ada.Real_Time.Time_Span := Default_Rate_Limit)
      return API.API_Class;
   --% Creates a current weather query context by coordinates.
   --  Translates to:
   --    api.openweathermap.org/data/2.5/weather?lat={Latitude}&lon={Longitude}
   --
   --% @param Coordinates
   --% The geographical coordinates the API query shall be created for.
   --
   --% @param Configuration
   --% Configuration object with connection configuration data.
   --
   --% @param Connection
   --% The server connection to be re-used with this query if not null. If this
   --% parameter is null, a new server connection will be created.
   --
   --% @param Cache_Interval
   --% Amount of time previous queries are cached before a new query is sent to
   --% the server.
   --
   --% @param Rate_Limit
   --% Minimum time interval between queries. As this is specific to the
   --% connection object, this only applies if the Connection parameter is null.
   --
   --% @return
   --% An API query object for current weather data at given coordinates.

   -----------------------------------------------------------------------------
   --  Create_Current_By_Group
   -----------------------------------------------------------------------------
   function Create_Current_By_Group
     (Ids            : in Group_List;
      Configuration  : in GNATCOLL.JSON.JSON_Value;
      Connection     : in Client.T_Access         := null;
      Cache_Interval : in Ada.Real_Time.Time_Span := Default_Cache_Interval;
      Rate_Limit     : in Ada.Real_Time.Time_Span := Default_Rate_Limit)
      return API.API_Class;
   ---% Creates a current weather query context by city id.
   --
   --  Translates to:
   --    api.openweathermap.org/data/2.5/group?id={Ids}
   --
   --  Please be aware that while this only fires a single query, each member of
   --  the group still counts as one API call as far as rate limits are
   --  concerned.
   --
   --% @param Ids
   --% The list of city ids to be queried.
   --
   --% @param Configuration
   --% Configuration object with connection configuration data.
   --
   --% @param Connection
   --% The server connection to be re-used with this query if not null. If this
   --% parameter is null, a new server connection will be created.
   --
   --% @param Cache_Interval
   --% Amount of time previous queries are cached before a new query is sent to
   --% the server.
   --
   --% @param Rate_Limit
   --% Minimum time interval between queries. As this is specific to the
   --% connection object, this only applies if the Connection parameter is null.
   --
   --% @return
   --% An API query object for current weather data for the given list of city
   --% ids.

   -----------------------------------------------------------------------------
   --  Create_Current_By_Id
   -----------------------------------------------------------------------------
   function Create_Current_By_Id
     (Id             : in City_Id;
      Configuration  : in GNATCOLL.JSON.JSON_Value;
      Connection     : in Client.T_Access         := null;
      Cache_Interval : in Ada.Real_Time.Time_Span := Default_Cache_Interval;
      Rate_Limit     : in Ada.Real_Time.Time_Span := Default_Rate_Limit)
      return API.API_Class;
   --% Creates a current weather query context by city id.
   --
   --  Translates to:
   --    api.openweathermap.org/data/2.5/weather?id={City_Id}
   --
   --% @param Id
   --% The city id to be queried.
   --
   --% @param Configuration
   --% Configuration object with connection configuration data.
   --
   --% @param Connection
   --% The server connection to be re-used with this query if not null. If this
   --% parameter is null, a new server connection will be created.
   --
   --% @param Cache_Interval
   --% Amount of time previous queries are cached before a new query is sent to
   --% the server.
   --
   --% @param Rate_Limit
   --% Minimum time interval between queries. As this is specific to the
   --% connection object, this only applies if the Connection parameter is null.
   --
   --% @return
   --% An API query object for current weather data for the given city id.

private

   type T is abstract tagged limited null record;

end Open_Weather_Map.API;
