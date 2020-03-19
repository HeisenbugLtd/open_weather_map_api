with Standard_Application;

private with Ada.Containers.Indefinite_Vectors;
private with Open_Weather_Map.API;
private with Open_Weather_Map.Client;
private with Open_Weather_Map.Configuration;

package Open_Weather_Map.Application with
  SPARK_Mode => Off
is

   type T is new Standard_Application.T with private;

   not overriding procedure Initialize (Self : in out T);
   overriding procedure Shutdown (Self : in out T);
   overriding procedure Work (Self : in out T);

private

   type Query_Data_Set (Query : API.API_Class_Access) is
      record
         Data          : Data_Set;
         Previous_Data : Data_Set;
      end record;

   package Queries is new
     Ada.Containers.Indefinite_Vectors (Index_Type   => Positive,
                                        Element_Type => Query_Data_Set);

   type T is new Standard_Application.T with
      record
         Configuration : Open_Weather_Map.Configuration.T;
         Connection    : Client.T_Access;
         API_Calls     : Queries.Vector;
      end record;

   not overriding procedure Update_And_Report (Self : in     T;
                                               Q    : in out Query_Data_Set);

   procedure Print (City_Name : in String);
   procedure Print (H : in Types.Humidity);
   procedure Print (P : in Types.Pressure);
   procedure Print (T : in Types.Celsius);
   procedure Print_Last_Update_On_Server
     (Value     : in Ada.Calendar.Time;
      Time_Zone : in Ada.Calendar.Time_Zones.Time_Offset);
   procedure Print_Location (Loc : in Open_Weather_Map.Geo_Coordinates);
   procedure Print_Seconds_Since_Last_Query
     (Elapsed_Time : in Ada.Real_Time.Time_Span);
   procedure Print_Time (Value     : in Ada.Calendar.Time;
                         Time_Zone : in Ada.Calendar.Time_Zones.Time_Offset);
   procedure Print_Timestamp (Value : in Ada.Real_Time.Time_Span);

end Open_Weather_Map.Application;
