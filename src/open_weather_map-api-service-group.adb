--------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+owm@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
--------------------------------------------------------------------------------
pragma License (Unrestricted);

with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with GNATCOLL.JSON.Conversions;
with Open_Weather_Map.API.Service.Utilities;

package body Open_Weather_Map.API.Service.Group is

   My_Debug : constant not null GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create (Unit_Name => "OWM.API.GROUP");

   package Field_Names is
      pragma Warnings (Off, "declaration hides");

      package Main is
         Humidity    : constant String := "humidity";
         Pressure    : constant String := "pressure";
         Temperature : constant String := "temp";
      end Main;

      package Element is
         Coordinates : constant String := "coord";
         Date_Time   : constant String := "dt";
         Main        : constant String := "main";
         Name        : constant String := "name";
         Sys         : constant String := "sys";
      end Element;

      package Root is
         List : constant String := "list";
      end Root;

      package Sys is
         Sunrise   : constant String := "sunrise";
         Sunset    : constant String := "sunset";
         Time_Zone : constant String := "timezone";
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
      begin
         My_Debug.all.Trace (Message => "Decode_Response.Decode_City_Data");

         return
           City_Data'(Location    =>
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
                        Element.Get (Field => Field_Names.Element.Name),
                      Time_Zone   =>
                        Conversions.To_Time_Offset
                          (Value => Sys,
                           Field => Field_Names.Sys.Time_Zone),
                      Last_Update =>
                        Conversions.To_Time
                          (Value => Element,
                           Field => Field_Names.Element.Date_Time));
      end Decode_City_Data;

      Cities : City_Lists.Vector;
   begin
      My_Debug.all.Trace (Message => "Decode_Response");

      if not Root.Has_Field (Field => Field_Names.Root.List) then
         return Invalid_Data_Set;
      end if;

      Get_City_List :
      declare
         List   : constant GNATCOLL.JSON.JSON_Array :=
           Root.Get (Field => Field_Names.Root.List);
         Length : constant Natural := GNATCOLL.JSON.Length (Arr => List);
      begin
         if Length = 0 then
            return Invalid_Data_Set;
         end if;

         City_Loop :
         for I in 1 .. Length loop
            Decode_City :
            declare
               Element : constant GNATCOLL.JSON.JSON_Value :=
                 GNATCOLL.JSON.Get (Arr   => List,
                                    Index => I);
            begin
               if
                 Element.Has_Field (Field => Field_Names.Element.Coordinates) and then
                 Element.Has_Field (Field => Field_Names.Element.Date_Time)   and then
                 Element.Has_Field (Field => Field_Names.Element.Main)        and then
                 Element.Has_Field (Field => Field_Names.Element.Name)        and then
                 Element.Has_Field (Field => Field_Names.Element.Sys)
               then
                  Check_Data_Fields :
                  declare
                     Coord : constant GNATCOLL.JSON.JSON_Value :=
                       Element.Get (Field => Field_Names.Element.Coordinates);
                     Main : constant GNATCOLL.JSON.JSON_Value :=
                       Element.Get (Field => Field_Names.Element.Main);
                     Sys  : constant GNATCOLL.JSON.JSON_Value :=
                       Element.Get (Field => Field_Names.Element.Sys);
                  begin
                     if
                       Utilities.Has_Coord_Fields (Coordinates => Coord)      and then
                       Main.Has_Field (Field => Field_Names.Main.Humidity)    and then
                       Main.Has_Field (Field => Field_Names.Main.Pressure)    and then
                       Main.Has_Field (Field => Field_Names.Main.Temperature) and then
                       Sys.Has_Field (Field => Field_Names.Sys.Sunrise)       and then
                       Sys.Has_Field (Field => Field_Names.Sys.Sunset)        and then
                       Sys.Has_Field (Field => Field_Names.Sys.Time_Zone)
                     then
                        Cities.Append
                          (New_Item => Decode_City_Data (Element => Element,
                                                         Coord   => Coord,
                                                         Main    => Main,
                                                         Sys     => Sys));
                     end if;
                  end Check_Data_Fields;
               end if;
            end Decode_City;
         end loop City_Loop;
      end Get_City_List;

      if not Cities.Is_Empty then
         return Data_Set'(Valid  => True,
                          Cities => Cities);
      end if;

      return Invalid_Data_Set;
   end Decode_Response;

   -----------------------------------------------------------------------------
   --  Initialize
   -----------------------------------------------------------------------------
   procedure Initialize
     (Self               :    out   T;
      Configuration      : in       GNATCOLL.JSON.JSON_Value;
      Connection         : not null Client.T_Access;
      Max_Cache_Interval : in       Ada.Real_Time.Time_Span := Default_Cache_Interval;
      Ids                : in       Group_List)
   is
      Id_List : Ada.Strings.Unbounded.Unbounded_String;
   begin
      My_Debug.all.Trace (Message => "Initialize");

      Service.T (Self).Initialize (Configuration      => Configuration,
                                   Connection         => Connection,
                                   Max_Cache_Interval => Max_Cache_Interval,
                                   For_API_Service    => Current_By_Group);

      for Id of Ids loop
         Ada.Strings.Unbounded.Append
           (Source   => Id_List,
            New_Item => Ada.Strings.Fixed.Trim (Source => Id'Image,
                                                Side   => Ada.Strings.Left));
         Ada.Strings.Unbounded.Append (Source   => Id_List,
                                       New_Item => ',');
      end loop;

      Ada.Strings.Unbounded.Trim (Source => Id_List,
                                  Left   => Ada.Strings.Maps.Null_Set,
                                  Right  => Ada.Strings.Maps.To_Set (","));
      Self.Parameters.Add (Name  => "id",
                           Value => Ada.Strings.Unbounded.To_String (Id_List));
   end Initialize;

end Open_Weather_Map.API.Service.Group;
