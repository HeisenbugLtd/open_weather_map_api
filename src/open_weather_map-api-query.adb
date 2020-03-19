package body Open_Weather_Map.API.Query with
  SPARK_Mode => Off
is

   My_Debug : constant not null GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create (Unit_Name => "OWM.API.QUERY");

   procedure Initialize (Context : out T) is
   begin
      My_Debug.all.Trace (Message => "Initialize");

      Context.Last_Query := Ada.Real_Time.Time_First;
   end Initialize;

   procedure Set_Last_Query (Context : in out T;
                             Value   : in     Ada.Real_Time.Time) is
   begin
      My_Debug.all.Trace (Message => "Set_Last_Query");

      Context.Last_Query := Value;
   end Set_Last_Query;

end Open_Weather_Map.API.Query;
