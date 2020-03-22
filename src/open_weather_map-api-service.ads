--------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+owm@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
--------------------------------------------------------------------------------
pragma License (Unrestricted);

with Open_Weather_Map.API.Query;

private with AWS.Parameters;

--------------------------------------------------------------------------------
--% @summary
--% Open_Weather_Map.API.Query
--
--% @description
--% Provides the abstract tagged type from which every concrete implementation
--% of a specific API query object shall be derived.
--------------------------------------------------------------------------------
private package Open_Weather_Map.API.Service is

   -----------------------------------------------------------------------------
   --  Extended context type for API queries, and its primitive operations.
   --
   --  This type actually "knows" something about the API, and is the type all
   --  API query types shall derive from.
   -----------------------------------------------------------------------------
   type T is abstract new Query.T with private;

   -----------------------------------------------------------------------------
   --  Initialize
   -----------------------------------------------------------------------------
   procedure Initialize
     (Self               :    out   T;
      Configuration      : in       GNATCOLL.JSON.JSON_Value;
      Connection         : not null Client.T_Access;
      Max_Cache_Interval : in       Ada.Real_Time.Time_Span := Default_Cache_Interval;
      For_API_Service    : in       API_Services);
   --% Sets up the context (and namely the proper URL for API calls) given the
   --% service name for the actual API.
   --
   --% @param Self
   --% The object to be initialized.
   --
   --% @param Configuration
   --% JSON value containing configuration data.
   --
   --% @param Connection
   --% The HTTP connection to be used for the query object. If this value is
   --% null, a new connection will be created.
   --
   --% @param Max_Cache_Interval
   --% Span of time a previous value will be stored before a new value will be
   --% requested from the server.
   --
   --% @param For_API_Service
   --% The API service for which this query instance shall be initialized for.

   -----------------------------------------------------------------------------
   --  Cache_Interval
   -----------------------------------------------------------------------------
   function Cache_Interval (Self : in T) return Ada.Real_Time.Time_Span with
     Inline => True;
   --% Returns a previously set Cache_Interval for this Context.
   --
   --% @param Self
   --% The object the cache interval shall be returned from.
   --
   --% @return
   --% The preset cache interval for object Self.

   -----------------------------------------------------------------------------
   --  Set_Cache_Interval
   -----------------------------------------------------------------------------
   procedure Set_Cache_Interval
     (Self               : in out T;
      Max_Cache_Interval : in     Ada.Real_Time.Time_Span);
   --% Set a new cache interval for the object.
   --
   --% @param Self
   --% The object for which a new cache interval shall be set.
   --
   --% @param Max_Cache_Interval
   --% The new cache interval to be set.
   --
   --  Once a value has been retrieved from the server, a new request will not
   --  be sent if the new request happens within Cache_Interval. This supports
   --  both rate limiting on queries as well as speed optimization to not
   --  retrieve new values that seldom change over time, anyway.
   --
   --  The update interval on openweathermap.org seems to be about 10 minutes,
   --  but that doesn't even mean, there's a new measurement (in fact, this can
   --  take hours).

   -----------------------------------------------------------------------------
   --  Service_URI
   -----------------------------------------------------------------------------
   function Service_URI (This : in T) return String;
   --% Returns the URI for the context specific API (including parameters etc.).
   --
   --% @param This
   --% The object the Service URI shall be retrieved from.
   --
   --% @return
   --% The query specific service URI for the object.
   --  For security reasons, the API key parameter is not included here, only
   --  the query specific parameters.

   -----------------------------------------------------------------------------
   --  Decode_Response
   -----------------------------------------------------------------------------
   function Decode_Response
     (Self : in T;
      Root : in GNATCOLL.JSON.JSON_Value) return Data_Set is abstract;
   --% Decodes the Response and returns the context specific Data_Set.
   --  Called from within Query() with the received JSON data.
   --  This subroutine must be overridden by the concrete API context
   --  implementations to cater for the differences of the services' responses.
   --
   --% @param Self
   --% The concerned object.
   --
   --% @param Root
   --% The root object of the JSON tree received from the server.
   --
   --% @return
   --% The Data_Set extracted from the JSON value given in Root.

   -----------------------------------------------------------------------------
   --  Perform_Query
   -----------------------------------------------------------------------------
   overriding procedure Perform_Query (Self    : in out T;
                                       Current : in out Data_Set);
   --% Operation to fire and evaluate a query as defined by the actual type of
   --% Self.
   --
   --% @param Self
   --% The query object for which a query shall be sent.
   --
   --% @param Current
   --% The data set to be updated.

private

   type T is abstract new Query.T with
      record
         Server_Connection : Client.T_Access;
         --% @field Server_Connection
         --% The HTTP client connection used to communicate with the server.
         Cache_Interval    : Ada.Real_Time.Time_Span;
         --% @field Cache_Interval
         --% The span of time previously acquired values will be cached before a
         --% new query is sent to the server.
         Key               : API_Key;
         --% @field Key
         --% The (secret) API key used to authenticate queries.
         Service           : API_Services;
         --% @field Service
         --% The API service for this query.
         Parameters        : AWS.Parameters.List;
         --% @field Parameters
         --% API specific query parameters.
      end record;

   -----------------------------------------------------------------------------
   --  Cache_Interval
   -----------------------------------------------------------------------------
   function Cache_Interval (Self : in T) return Ada.Real_Time.Time_Span is
     (Self.Cache_Interval);

end Open_Weather_Map.API.Service;
