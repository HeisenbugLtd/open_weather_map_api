--------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+owm@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
--------------------------------------------------------------------------------
pragma License (Unrestricted);

--------------------------------------------------------------------------------
--  Open_Weather_Map.API.Query (private)
--
--  Provides the abstract tagged type implementing a context where query timings
--  are stored.
--------------------------------------------------------------------------------

private package Open_Weather_Map.API.Query is

   -----------------------------------------------------------------------------
   --  Abstract context base type, and its primitive operations.
   -----------------------------------------------------------------------------

   type T is abstract new Open_Weather_Map.API.T with private;

   procedure Initialize (Context : out T);
   --  Initializes the object.

   overriding function Last_Query (Context : in T) return Ada.Real_Time.Time with
     Inline => True;
   --  Returns the time of the last query done with that context.

   procedure Set_Last_Query (Context : in out T;
                             Value   : in     Ada.Real_Time.Time);
   --  Sets a new last time a query has been executed. Should be called by
   --  derived types whenever a query is about to be performed.

private

   type T is abstract new Open_Weather_Map.API.T with
      record
         Last_Query : Ada.Real_Time.Time;
      end record;

   overriding function Last_Query (Context : in T) return Ada.Real_Time.Time is
     (Context.Last_Query);

end Open_Weather_Map.API.Query;
