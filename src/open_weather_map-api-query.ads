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
--% Open_Weather_Map.API.Query (private)
--
--% @description
--% Provides the abstract tagged type implementing a context where query timings
--% are stored.
--------------------------------------------------------------------------------
private package Open_Weather_Map.API.Query is

   -----------------------------------------------------------------------------
   --  Abstract context base type, and its primitive operations.
   -----------------------------------------------------------------------------

   type T is abstract new Open_Weather_Map.API.T with private;

   -----------------------------------------------------------------------------
   --  Initialize
   -----------------------------------------------------------------------------
   procedure Initialize (Self : out T);
   --% Initializes the object.
   --
   --% @param Self
   --% The object to be initialized.

   -----------------------------------------------------------------------------
   --  Last_Query
   -----------------------------------------------------------------------------
   overriding function Last_Query (Self : in T) return Ada.Real_Time.Time with
     Inline => True;
   --% Returns the time of the last query done with that context.
   --
   --% @param Self
   --% The object to retrive its last update time from.
   --
   --% @return
   --% Time of the last server update for This.

   -----------------------------------------------------------------------------
   --  Set_Last_Query
   -----------------------------------------------------------------------------
   procedure Set_Last_Query (Self  : in out T;
                             Value : in     Ada.Real_Time.Time);
   --% Sets a new last time a query has been executed. Should be called by
   --% derived types whenever a query is about to be performed.
   --
   --% @param Self
   --% The object to be updated.
   --
   --% @param Value
   --% The new time to be set for the last update being performed.

private

   type T is abstract new Open_Weather_Map.API.T with
      record
         Last_Query : Ada.Real_Time.Time;
         --% @field Last_Query
         --% Stores the last time the object has been updated from the server.
         --  Used to implement and caching and rate limiting.
      end record;

   -----------------------------------------------------------------------------
   --  Last_Query
   -----------------------------------------------------------------------------
   overriding function Last_Query (Self : in T) return Ada.Real_Time.Time is
     (Self.Last_Query);

end Open_Weather_Map.API.Query;
