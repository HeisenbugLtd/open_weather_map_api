--------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+owm@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
--------------------------------------------------------------------------------
pragma License (Unrestricted);

--------------------------------------------------------------------------------
--  Open_Weather_Map.API.Query
--
--  Provides the abstract tagged type from which every concrete implementation
--  of a specific API query object shall be derived.
--------------------------------------------------------------------------------

with Open_Weather_Map.API.Query;

private with AWS.Parameters;

private package Open_Weather_Map.API.Service is

   -----------------------------------------------------------------------------
   --  Extended context type for API queries, and its primitive operations.
   --
   --  This type actually "knows" something about the API, and is the type all
   --  API query types shall derive from.
   -----------------------------------------------------------------------------
   type T is abstract new Query.T with private;

   procedure Initialize
     (Context            :    out   T;
      Configuration      : in       GNATCOLL.JSON.JSON_Value;
      Connection         : not null Client.T_Access;
      Max_Cache_Interval : in       Ada.Real_Time.Time_Span := Default_Cache_Interval;
      For_API_Service    : in       API_Services);
   --  Sets up the context (and namely the proper URL for API calls) given the
   --  service name for the actual API.

   function Cache_Interval (Context : in T) return Ada.Real_Time.Time_Span with
     Inline => True;
   --  Returns a previously set Cache_Interval for this Context.

   procedure Set_Cache_Interval
     (Context            : in out T;
      Max_Cache_Interval : in     Ada.Real_Time.Time_Span);
   --  Once a value has been retrieved from the server, a new request will not
   --  be sent if the new request happens within Cache_Interval. This supports
   --  both rate limiting on queries as well as speed optimization to not
   --  retrieve new values that seldom change over time, anyway.
   --
   --  The update interval on openweathermap.org seems to be about 10 minutes,
   --  but that doesn't even mean, there's a new measurement (in fact, this can
   --  take hours).

   function Service_URI (Context : in T) return String;
   --  Returns the URI for the context specific API (including parameters etc.).
   --  For security reasons, the API key parameter is not included here, only
   --  the query specific parameters.

   function Decode_Response
     (Context : in T;
      Root    : in GNATCOLL.JSON.JSON_Value) return Data_Set is abstract;
   --  Decodes the Response and returns the context specific Data_Set.
   --  Called from within Query() with the received JSON data.
   --  This subroutine must be overridden by the concrete API context
   --  implementations to cater for the differences of the services' responses.

   overriding procedure Perform_Query (Context : in out T;
                                       Current : in out Data_Set);
   --  Operation to fire and evaluate a query as defined by the actual type of
   --  Context.

private

   type T is abstract new Query.T with
      record
         Server_Connection : Client.T_Access;
         Cache_Interval    : Ada.Real_Time.Time_Span;
         Key               : API_Key;
         Service           : API_Services;
         Parameters        : AWS.Parameters.List;
      end record;

   function Cache_Interval (Context : in T) return Ada.Real_Time.Time_Span is
     (Context.Cache_Interval);

end Open_Weather_Map.API.Service;
