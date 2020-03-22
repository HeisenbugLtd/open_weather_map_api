--------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+owm@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
--------------------------------------------------------------------------------
pragma License (Unrestricted);

with AWS.Client;
limited with GNATCOLL.JSON;

--------------------------------------------------------------------------------
--% @summary
--  Open_Weather_Map.Client
--
--% @description
--% Provides a wrapper for an HTTP connection object including the
--% implementation of a rate limit.
--------------------------------------------------------------------------------
package Open_Weather_Map.Client is

   type T is tagged limited private;
   type T_Access is access all T'Class;

   -----------------------------------------------------------------------------
   --  Create
   -----------------------------------------------------------------------------
   function Create
     (Configuration : in GNATCOLL.JSON.JSON_Value;
      Rate_Limit    : in Ada.Real_Time.Time_Span := Default_Rate_Limit) return T_Access;
   --% Creates (dynamically allocates) and initializes a Client object.
   --
   --% @param Configuration
   --% A JSON value with configuration values for the client stored in it.
   --
   --% @param Rate_Limit
   --% Interval at which API requests should be made at most with this client.

   -----------------------------------------------------------------------------
   --  Destroy
   -----------------------------------------------------------------------------
   procedure Destroy (Self : in out T_Access);
   --% Destroys a Client object and frees the storage associated with it.
   --
   --% @param Self
   --% The object to be freed.

   -----------------------------------------------------------------------------
   --  Initialize
   -----------------------------------------------------------------------------
   procedure Initialize
     (Self          :    out T;
      Configuration : in     GNATCOLL.JSON.JSON_Value;
      Rate_Limit    : in     Ada.Real_Time.Time_Span := Default_Rate_Limit);
   --% Initializes a Client object according to the given configuration.
   --
   --% @param Self
   --% The object to be initialized.
   --
   --% @param Configuration
   --% A JSON value with configuration values for the client stored in it.
   --
   --% @param Rate_Limit
   --% Indicates the minimum time interval at which a call to Connection (see
   --% below) can be invoked without suspending the caller.

   -----------------------------------------------------------------------------
   --  Connection
   -----------------------------------------------------------------------------
   function Connection
     (Self : in out T) return not null AWS.Client.HTTP_Connection_Access;
   --% Implements rate limiting by suspending a caller if calls to this function
   --% are not at least the time interval given in Rate_Limit apart.
   --
   --% @param Self
   --% The client object.
   --
   --% @return
   --% The HTTP connection associated with the client.
   --
   --  NOTE: The implementation assumes that the returned connection object will
   --        not be stored by the caller, but rather requested each time a
   --        connection is being made.

private

   type T is new Ada.Finalization.Limited_Controlled with
      record
         Rate_Limit      : Ada.Real_Time.Time_Span;
         Last_Access     : Ada.Real_Time.Time;
         HTTP_Connection : AWS.Client.HTTP_Connection_Access;
      end record;
   --% The type representing a (HTTP) client.
   --
   --% @field Rate_Limit
   --% The minimum separation interval between HTTP API requests made by this
   --% client.
   --
   --% @field Last_Access
   --% The time of the last HTTP query made by the client.
   --
   --% @field HTTP_Connection
   --% The client's HTTP connection to the server.

   -----------------------------------------------------------------------------
   --  Finalize
   -----------------------------------------------------------------------------
   overriding procedure Finalize (Self : in out T);
   --% Finalizes the client object.
   --
   --% @param Self
   --% The client object being finalized.

end Open_Weather_Map.Client;
