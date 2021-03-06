--------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+owm@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
--------------------------------------------------------------------------------
pragma License (Unrestricted);

package body Open_Weather_Map.API.Query is

   My_Debug : constant not null GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create (Unit_Name => "OWM.API.QUERY");

   -----------------------------------------------------------------------------
   --  Initialize
   -----------------------------------------------------------------------------
   procedure Initialize (Self : out T) is
   begin
      My_Debug.all.Trace (Message => "Initialize");

      Self.Last_Query := Ada.Real_Time.Time_First;
   end Initialize;

   -----------------------------------------------------------------------------
   --  Set_Last_Query
   -----------------------------------------------------------------------------
   procedure Set_Last_Query (Self  : in out T;
                             Value : in     Ada.Real_Time.Time) is
   begin
      My_Debug.all.Trace (Message => "Set_Last_Query");

      Self.Last_Query := Value;
   end Set_Last_Query;

end Open_Weather_Map.API.Query;
