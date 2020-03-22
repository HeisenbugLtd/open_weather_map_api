--------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+owm@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
--------------------------------------------------------------------------------
pragma License (Unrestricted);

with Ada.Calendar.Formatting;
with Ada.Text_IO;
with Open_Weather_Map.City_Ids;
with SI_Units.Metric.Scaling;
with SI_Units.Names;
with SI_Units.Sexagesimal;

package body Open_Weather_Map.Application is

   My_Debug : constant not null GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create (Unit_Name => "OWM.APP");

   No_Break_Space : constant String :=
     Character'Val (16#C2#) & Character'Val (16#A0#);

   Standard_Output : constant Ada.Text_IO.File_Type :=
     Ada.Text_IO.Standard_Output;

   -----------------------------------------------------------------------------
   --  Image
   -----------------------------------------------------------------------------
   function Image is new
     SI_Units.Metric.Fixed_Image (Item        => Types.Humidity,
                                  Default_Aft => 1,
                                  Unit        => SI_Units.Names.Percent);

   -----------------------------------------------------------------------------
   --  Image
   -----------------------------------------------------------------------------
   function Image is new
     SI_Units.Metric.Fixed_Image (Item        => Types.Pressure'Base,
                                  Default_Aft => 1,
                                  Unit        => SI_Units.Names.Pascal);

   -----------------------------------------------------------------------------
   --  Image
   -----------------------------------------------------------------------------
   function Image is new
     SI_Units.Metric.Fixed_Image
       (Item        => Types.Celsius,
        Default_Aft => 1,
        Unit        => SI_Units.Names.Degree_Celsius);

   package Degrees is new SI_Units.Sexagesimal (Degree      => Types.Degree,
                                                Default_Aft => 3);

   -----------------------------------------------------------------------------
   --  Initialize
   -----------------------------------------------------------------------------
   procedure Initialize (Self : in out T) is
   begin
      Standard_Application.T (Self).Initialize
        (Cycle_Time            => Ada.Real_Time.Seconds (1),
         Application_Directory => Application_Directory,
         Log_Name              => "owm");

      My_Debug.all.Trace (Message => "Initialize");

      Self.Configuration.Initialize;
      Self.Connection :=
        Client.Create (Configuration => Self.Configuration.Values);

      Self.API_Calls.Append
        (New_Item =>
           Query_Data_Set'
             (new API.API_Class'
                (API.Create_Current_By_Id
                   (Id            => City_Ids.Mexico.Xaltianguis,
                    Configuration => Self.Configuration.Values,
                    Connection    => Self.Connection)),
              Data          => (Valid => False),
              Previous_Data => (Valid => False)));
      Self.API_Calls.Append
        (New_Item =>
           Query_Data_Set'
             (new API.API_Class'
                (API.Create_Current_By_Coordinates
                   (Coordinates   =>
                      Geo_Coordinates'
                        (Latitude  => Types.Latitude'Value ("67.292"),
                         Longitude => Types.Longitude'Value ("28.150")),
                    Configuration => Self.Configuration.Values,
                    Connection    => Self.Connection)),
              Data          => (Valid => False),
              Previous_Data => (Valid => False)));
      Self.API_Calls.Append
        (New_Item =>
           Query_Data_Set'
             (new API.API_Class'
                (API.Create_Current_By_Group
                   (Ids           => (1 => City_Ids.India.Giddalur,
                                      2 => City_Ids.India.Nandyal),
                    Configuration => Self.Configuration.Values,
                    Connection    => Self.Connection)),
              Data          => (Valid => False),
              Previous_Data => (Valid => False)));
   end Initialize;

   -----------------------------------------------------------------------------
   --  Print
   -----------------------------------------------------------------------------
   procedure Print (City_Name : in String) is
   begin
      Ada.Text_IO.Put (File => Standard_Output,
                       Item => ", data for """ & City_Name & """");
   end Print;

   -----------------------------------------------------------------------------
   --  Print
   -----------------------------------------------------------------------------
   procedure Print (H : in Types.Humidity) is
   begin
      Ada.Text_IO.Put (File => Standard_Output,
                       Item => ", humidity: " & Image (Value => H));
   end Print;

   -----------------------------------------------------------------------------
   --  Print
   -----------------------------------------------------------------------------
   procedure Print (P : in Types.Pressure) is
      -----------------------------------------------------------------------------
      --  Scale
      -----------------------------------------------------------------------------
      function Scale is
        new SI_Units.Metric.Scaling.Fixed_Scale (Item => Types.Pressure'Base);
      use all type SI_Units.Metric.Scaling.Prefixes;
   begin
      Ada.Text_IO.Put (File => Standard_Output,
                       Item =>
                         ", pressure: " &
                         Image (Value => Scale (Value       => P,
                                                From_Prefix => Hecto)));
   end Print;

   -----------------------------------------------------------------------------
   --  Print
   -----------------------------------------------------------------------------
   procedure Print (T : in Types.Celsius) is
   begin
      Ada.Text_IO.Put (File => Standard_Output,
                       Item => ", temperature: " & Image (Value => T));
   end Print;

   -----------------------------------------------------------------------------
   --  Print_Last_Update_On_Server
   -----------------------------------------------------------------------------
   procedure Print_Last_Update_On_Server
     (Value     : in Ada.Calendar.Time;
      Time_Zone : in Ada.Calendar.Time_Zones.Time_Offset) is
   begin
      Ada.Text_IO.Put (File => Standard_Output,
                       Item => ", data updated at ");
      Print_Time (Value     => Value,
                  Time_Zone => Time_Zone);
   end Print_Last_Update_On_Server;

   -----------------------------------------------------------------------------
   --  Print_Location
   -----------------------------------------------------------------------------
   procedure Print_Location (Loc : in Geo_Coordinates) is
   begin
      Ada.Text_IO.Put
        (File => Standard_Output,
         Item =>
           " (" & Degrees.Latitude.Image (L => Loc.Latitude) & No_Break_Space &
           Degrees.Longitude.Image (L => Loc.Longitude) & ")");
   end Print_Location;

   -----------------------------------------------------------------------------
   --  Print_Seconds_Since_Last_Query
   -----------------------------------------------------------------------------
   procedure Print_Seconds_Since_Last_Query
     (Elapsed_Time : in Ada.Real_Time.Time_Span)
   is
      Time_String : constant String :=
        Ada.Calendar.Formatting.Image
          (Elapsed_Time          => Ada.Real_Time.To_Duration (Elapsed_Time),
           Include_Time_Fraction => True);
   begin
      Ada.Text_IO.Put (File => Standard_Output,
                       Item => "Last query: ");
      Ada.Text_IO.Put
        (File => Standard_Output,
         Item => Time_String (Time_String'Last - 4 .. Time_String'Last));
      Ada.Text_IO.Put (File => Standard_Output,
                       Item => " seconds ago");
   end Print_Seconds_Since_Last_Query;

   -----------------------------------------------------------------------------
   --  Print_Time
   -----------------------------------------------------------------------------
   procedure Print_Time (Value     : in Ada.Calendar.Time;
                         Time_Zone : in Ada.Calendar.Time_Zones.Time_Offset) is
   begin
      Ada.Text_IO.Put
        (Ada.Calendar.Formatting.Image (Date => Value) &
           " (" & Ada.Calendar.Formatting.Image (Date      => Value,
                                                 Time_Zone => Time_Zone) &
           " local time)");
   end Print_Time;

   -----------------------------------------------------------------------------
   --  Print_Timestamp
   -----------------------------------------------------------------------------
   procedure Print_Timestamp (Value : in Ada.Real_Time.Time_Span) is
   begin
      Ada.Text_IO.Put (File => Standard_Output,
                       Item => "[");

      declare
         use type Ada.Real_Time.Time_Span;
         One_Day    : constant Ada.Real_Time.Time_Span :=
           Ada.Real_Time.Seconds (24 * 60 * 60);
         Temp_Value : Ada.Real_Time.Time_Span := Value;
      begin
         --  This is a bug in GNAT, according to ARM 9.6.1/85f Image should be
         --  able to support values up to 100 hours.  The GNAT runtime chokes on
         --  a call to Split which only supports Day_Duration, so it raises
         --  Constraint_Error if the # of seconds exceeds 86400.
         while Temp_Value >= One_Day loop
            Temp_Value := Temp_Value - One_Day;
         end loop;

         Ada.Text_IO.Put
           (File => Standard_Output,
            Item =>
              Ada.Calendar.Formatting.Image
                (Elapsed_Time          => Ada.Real_Time.To_Duration (Temp_Value),
                 Include_Time_Fraction => True));
      end;

      Ada.Text_IO.Put (File => Standard_Output,
                       Item => "] ");
   end Print_Timestamp;

   -----------------------------------------------------------------------------
   --  Shutdown
   -----------------------------------------------------------------------------
   overriding procedure Shutdown (Self : in out T) is
   begin
      My_Debug.all.Trace (Message => "Shutdown");

      Client.Destroy (Self.Connection);
      Standard_Application.T (Self).Shutdown;
   end Shutdown;

   -----------------------------------------------------------------------------
   --  Update_And_Report
   -----------------------------------------------------------------------------
   procedure Update_And_Report (Self : in     T;
                                Q    : in out Query_Data_Set) is
      use type Ada.Real_Time.Time;
   begin
      My_Debug.all.Trace (Message => "Update_And_Report");

      Q.Previous_Data := Q.Data;
      Q.Query.all.Perform_Query (Current => Q.Data);

      if Q.Data.Valid and then Q.Data /= Q.Previous_Data then
         declare
            Now       : constant Ada.Real_Time.Time      := Ada.Real_Time.Clock;
            TS_Global : constant Ada.Real_Time.Time_Span := Now - Self.Epoch;
            TS_Query  : constant Ada.Real_Time.Time_Span := Now - Q.Query.all.Last_Query;
         begin
            for I in 1 .. Natural (Q.Data.Cities.Length) loop
               declare
                  City : constant City_Data :=
                    Q.Data.Cities.Element (Index => I);
               begin
                  Print_Timestamp (Value => TS_Global);
                  Print_Seconds_Since_Last_Query (Elapsed_Time => TS_Query);
                  Print
                    (City_Name => Ada.Strings.Unbounded.To_String (City.Name));
                  Print_Location (Loc => City.Location);
                  Print (T => Types.To_Celsius (City.Temperature));
                  Print (H => City.Humidity);
                  Print (P => City.Pressure);
                  Ada.Text_IO.Put (File => Standard_Output,
                                   Item => ", sunrise at ");
                  Print_Time (Value     => City.Sunrise,
                              Time_Zone => City.Time_Zone);
                  Ada.Text_IO.Put (File => Standard_Output,
                                   Item => ", sunset at ");
                  Print_Time (Value     => City.Sunset,
                              Time_Zone => City.Time_Zone);
                  Print_Last_Update_On_Server (Value     => City.Last_Update,
                                               Time_Zone => City.Time_Zone);
                  Ada.Text_IO.Put_Line (File => Standard_Output,
                                        Item => ".");
               end;
            end loop;
         end;
      end if;
   end Update_And_Report;

   -----------------------------------------------------------------------------
   --  Work
   -----------------------------------------------------------------------------
   overriding procedure Work (Self : in out T) is
   begin
      My_Debug.all.Trace (Message => "Work");

      Queries_Loop :
      for Call of Self.API_Calls loop
         Self.Update_And_Report (Q => Call);
      end loop Queries_Loop;
   end Work;

end Open_Weather_Map.Application;
