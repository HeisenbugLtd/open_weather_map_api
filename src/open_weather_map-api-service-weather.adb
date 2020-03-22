--------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+owm@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
--------------------------------------------------------------------------------
pragma License (Unrestricted);

with Ada.Strings.Fixed;
with GNATCOLL.JSON.Conversions;
with Open_Weather_Map.API.Service.Utilities;

package body Open_Weather_Map.API.Service.Weather is

   My_Debug : constant not null GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create (Unit_Name => "OWM.API.WEATHER");

   package Field_Names is
      pragma Warnings (Off, "declaration hides");

      package Main is
         Humidity    : constant String := "humidity";
         Pressure    : constant String := "pressure";
         Temperature : constant String := "temp";
      end Main;

      package Root is
         Coordinates : constant String := "coord";
         Date_Time   : constant String := "dt";
         Main        : constant String := "main";
         Name        : constant String := "name";
         Sys         : constant String := "sys";
         Time_Zone   : constant String := "timezone";
      end Root;

      package Sys is
         Sunrise : constant String := "sunrise";
         Sunset  : constant String := "sunset";
      end Sys;

      pragma Warnings (On, "declaration hides");
   end Field_Names;

   -----------------------------------------------------------------------------
   --  Decode_Response
   -----------------------------------------------------------------------------
   overriding function Decode_Response
     (Self : in T;
      Root : in GNATCOLL.JSON.JSON_Value) return Data_Set
   is
      pragma Unreferenced (Self);

      --------------------------------------------------------------------------
      --  Decode_City_Data
      --------------------------------------------------------------------------
      function Decode_City_Data
        (Element : in GNATCOLL.JSON.JSON_Value;
         Coord   : in GNATCOLL.JSON.JSON_Value;
         Main    : in GNATCOLL.JSON.JSON_Value;
         Sys     : in GNATCOLL.JSON.JSON_Value) return City_Data;

      --------------------------------------------------------------------------
      --  Decode_City_Data
      --------------------------------------------------------------------------
      function Decode_City_Data
        (Element : in GNATCOLL.JSON.JSON_Value;
         Coord   : in GNATCOLL.JSON.JSON_Value;
         Main    : in GNATCOLL.JSON.JSON_Value;
         Sys     : in GNATCOLL.JSON.JSON_Value) return City_Data
      is
         package Conversions renames GNATCOLL.JSON.Conversions;
         package Field_Names_Element renames Field_Names.Root;
      begin
         My_Debug.all.Trace (Message => "Decode_Response.Decode_City_Data");

         return
           City_Data'(Location     =>
                        Utilities.Decode_Coordinates (Coordinates => Coord),
                      Temperature =>
                        Conversions.To_Temperature
                          (Value => Main,
                           Field => Field_Names.Main.Temperature),
                      Humidity    =>
                        Conversions.To_Humidity
                          (Value => Main,
                           Field => Field_Names.Main.Humidity),
                      Pressure    =>
                        Conversions.To_Pressure
                          (Value => Main,
                           Field => Field_Names.Main.Pressure),
                      Sunrise     =>
                        Conversions.To_Time (Value => Sys,
                                             Field => Field_Names.Sys.Sunrise),
                      Sunset      =>
                        Conversions.To_Time (Value => Sys,
                                             Field => Field_Names.Sys.Sunset),
                      Name        =>
                        Element.Get (Field => Field_Names_Element.Name),
                      Time_Zone   =>
                        Conversions.To_Time_Offset
                          (Value => Element,
                           Field => Field_Names_Element.Time_Zone),
                      Last_Update =>
                        Conversions.To_Time
                          (Value => Element,
                           Field => Field_Names_Element.Date_Time));
      end Decode_City_Data;

      Cities : City_Lists.Vector;
   begin
      My_Debug.all.Trace (Message => "Decode_Response");

      if
        Root.Has_Field (Field => Field_Names.Root.Coordinates) and then
        Root.Has_Field (Field => Field_Names.Root.Date_Time)   and then
        Root.Has_Field (Field => Field_Names.Root.Main)        and then
        Root.Has_Field (Field => Field_Names.Root.Name)        and then
        Root.Has_Field (Field => Field_Names.Root.Sys)         and then
        Root.Has_Field (Field => Field_Names.Root.Time_Zone)
      then
         Check_Data_Fields :
         declare
            Coord : constant GNATCOLL.JSON.JSON_Value :=
              Root.Get (Field => Field_Names.Root.Coordinates);
            Main  : constant GNATCOLL.JSON.JSON_Value :=
              Root.Get (Field => Field_Names.Root.Main);
            Sys   : constant GNATCOLL.JSON.JSON_Value :=
              Root.Get (Field => Field_Names.Root.Sys);
         begin
            if
              Utilities.Has_Coord_Fields (Coordinates => Coord)      and then
              Main.Has_Field (Field => Field_Names.Main.Humidity)    and then
              Main.Has_Field (Field => Field_Names.Main.Pressure)    and then
              Main.Has_Field (Field => Field_Names.Main.Temperature) and then
              Sys.Has_Field (Field => Field_Names.Sys.Sunrise)       and then
              Sys.Has_Field (Field => Field_Names.Sys.Sunset)
            then
               Cities.Append (New_Item => Decode_City_Data (Element => Root,
                                                            Coord   => Coord,
                                                            Main    => Main,
                                                            Sys     => Sys));
            end if;
         end Check_Data_Fields;
      end if;

      if not Cities.Is_Empty then
         return Data_Set'(Valid  => True,
                          Cities => Cities);
      end if;

      return Data_Set'(Valid => False);
   end Decode_Response;

   -----------------------------------------------------------------------------
   --  Initialize
   -----------------------------------------------------------------------------
   procedure Initialize
     (Self               :    out   T;
      Configuration      : in       GNATCOLL.JSON.JSON_Value;
      Connection         : not null Client.T_Access;
      Max_Cache_Interval : in       Ada.Real_Time.Time_Span := Default_Cache_Interval;
      Id                 : in       City_Id) is
   begin
      My_Debug.all.Trace (Message => "Initialize");

      --  inherited initialization
      Service.T (Self).Initialize (Configuration      => Configuration,
                                   Connection         => Connection,
                                   Max_Cache_Interval => Max_Cache_Interval,
                                   For_API_Service    => Current_By_Id);

      --  initialization of added fields.
      Self.Parameters.Add
        (Name  => "id",
         Value => Ada.Strings.Fixed.Trim (Source => Id'Image,
                                          Side   => Ada.Strings.Left));
   end Initialize;

end Open_Weather_Map.API.Service.Weather;
