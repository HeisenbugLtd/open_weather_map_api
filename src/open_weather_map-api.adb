with Open_Weather_Map.API.Service.Group;
with Open_Weather_Map.API.Service.Location;
with Open_Weather_Map.API.Service.Weather;

package body Open_Weather_Map.API with
  SPARK_Mode => Off
is

   use type Client.T_Access;

   My_Debug : constant not null GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create (Unit_Name => "OWM.API");

   function Create_Current_By_Coordinates
     (Coordinates    : in Geo_Coordinates;
      Configuration  : in GNATCOLL.JSON.JSON_Value;
      Connection     : in Client.T_Access         := null;
      Cache_Interval : in Ada.Real_Time.Time_Span := Default_Cache_Interval;
      Rate_Limit     : in Ada.Real_Time.Time_Span := Default_Rate_Limit)
      return API.API_Class
   is
      My_Connection : Open_Weather_Map.Client.T_Access;
   begin
      My_Debug.all.Trace (Message => "Create_Current_By_Coordinates");

      if Connection = null then
         My_Debug.all.Trace
           (Message =>
              "Create_Current_By_Coordinates: " &
              "No HTTP connection specified, creating new one...");

         My_Connection := Client.Create (Configuration => Configuration,
                                         Rate_Limit    => Rate_Limit);
      else
         My_Connection := Connection;
      end if;

      return Result : Service.Location.T do
         Result.Initialize (Configuration      => Configuration,
                            Connection         => My_Connection,
                            Max_Cache_Interval => Cache_Interval,
                            Coordinates        => Coordinates);
      end return;
   end Create_Current_By_Coordinates;

   function Create_Current_By_Group
     (Ids            : in Group_List;
      Configuration  : in GNATCOLL.JSON.JSON_Value;
      Connection     : in Client.T_Access         := null;
      Cache_Interval : in Ada.Real_Time.Time_Span := Default_Cache_Interval;
      Rate_Limit     : in Ada.Real_Time.Time_Span := Default_Rate_Limit)
      return API.API_Class
   is
      My_Connection : Open_Weather_Map.Client.T_Access;
   begin
      My_Debug.all.Trace (Message => "Create_Current_By_Group");

      if Connection = null then
         My_Debug.all.Trace
           (Message =>
              "Create_Current_By_Group: " &
              "No HTTP connection specified, creating new one...");

         My_Connection := Client.Create (Configuration => Configuration,
                                         Rate_Limit    => Rate_Limit);
      else
         My_Connection := Connection;
      end if;

      return Result : Service.Group.T do
         Result.Initialize (Configuration      => Configuration,
                            Connection         => My_Connection,
                            Max_Cache_Interval => Cache_Interval,
                            Ids                => Ids);
      end return;
   end Create_Current_By_Group;

   function Create_Current_By_Id
     (Id             : in City_Id;
      Configuration  : in GNATCOLL.JSON.JSON_Value;
      Connection     : in Client.T_Access         := null;
      Cache_Interval : in Ada.Real_Time.Time_Span := Default_Cache_Interval;
      Rate_Limit     : in Ada.Real_Time.Time_Span := Default_Rate_Limit)
      return API.API_Class
   is
      My_Connection : Open_Weather_Map.Client.T_Access;
   begin
      My_Debug.all.Trace (Message => "Create_Current_By_Id");

      if Connection = null then
         My_Debug.all.Trace
           (Message =>
              "Create_Current_By_Id: " &
              "No HTTP connection specified, creating new one...");

         My_Connection := Client.Create (Configuration => Configuration,
                                         Rate_Limit    => Rate_Limit);
      else
         My_Connection := Connection;
      end if;

      return Result : Service.Weather.T do
         Result.Initialize (Configuration      => Configuration,
                            Connection         => My_Connection,
                            Max_Cache_Interval => Cache_Interval,
                            Id                 => Id);
      end return;
   end Create_Current_By_Id;

end Open_Weather_Map.API;
