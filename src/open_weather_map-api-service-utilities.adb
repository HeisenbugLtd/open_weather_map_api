with GNATCOLL.JSON.Conversions;

package body Open_Weather_Map.API.Service.Utilities with
  SPARK_Mode => Off
is

   My_Debug : constant not null GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create (Unit_Name => "OWM.API.SERVICE.UTILITIES");

   package Field_Names is

      package Coordinates is
         Latitude  : constant String := "lat";
         Longitude : constant String := "lon";
      end Coordinates;

   end Field_Names;

   function Decode_Coordinates
     (Coordinates : in GNATCOLL.JSON.JSON_Value) return Geo_Coordinates
   is
      package Conversions renames GNATCOLL.JSON.Conversions;
   begin
      My_Debug.all.Trace (Message => "Decode_Coordinates");

      return
        Geo_Coordinates'(Latitude  =>
                           Conversions.To_Latitude
                             (Value => Coordinates,
                              Field => Field_Names.Coordinates.Latitude),
                         Longitude =>
                           Conversions.To_Longitude
                             (Value => Coordinates,
                              Field => Field_Names.Coordinates.Longitude));
   end Decode_Coordinates;

   function Has_Coord_Fields
     (Coordinates : in GNATCOLL.JSON.JSON_Value) return Boolean is
   begin
      My_Debug.all.Trace (Message => "Has_Coord_Fields");

      return
        Coordinates.Has_Field (Field => Field_Names.Coordinates.Latitude) and then
        Coordinates.Has_Field (Field => Field_Names.Coordinates.Longitude);
   end Has_Coord_Fields;

end Open_Weather_Map.API.Service.Utilities;
