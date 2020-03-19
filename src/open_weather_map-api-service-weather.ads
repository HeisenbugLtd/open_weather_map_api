--------------------------------------------------------------------------------
--  Open_Weather_Map.API.Service.Weather
--
--  Provides the query object implementing a specific id based based query.
--------------------------------------------------------------------------------

package Open_Weather_Map.API.Service.Weather with
  SPARK_Mode => Off
is

   -----------------------------------------------------------------------------
   --  API: Current weather data (by city ID)
   -----------------------------------------------------------------------------
   type T is new Service.T with private;

   procedure Initialize
     (Context            :    out   T;
      Configuration      : in       GNATCOLL.JSON.JSON_Value;
      Connection         : not null Client.T_Access;
      Max_Cache_Interval : in       Ada.Real_Time.Time_Span := Default_Cache_Interval;
      Id                 : in       City_Id);
   --  Initializes Context according to the given Configuration object, using
   --  the given Connection for server queries that will be fired at least
   --  Max_Cache_Interval apart.
   --  Id is the location id of the place to be queried.

private

   type T is new API. Service.T with null record;

   overriding function Decode_Response
     (Context : in T;
      Root    : in GNATCOLL.JSON.JSON_Value) return Data_Set;

end Open_Weather_Map.API.Service.Weather;
