--------------------------------------------------------------------------------
--  Open_Weather_Map.API.Service.Utilities
--
--  Provides several utility functions.
--------------------------------------------------------------------------------

private package Open_Weather_Map.API.Service.Utilities with
  SPARK_Mode => Off
is

   function Has_Coord_Fields
     (Coordinates : in GNATCOLL.JSON.JSON_Value) return Boolean;
   --  Returns True if the given JSON value object has the field names expected
   --  for geographical coordinates.

   function Decode_Coordinates
     (Coordinates : in GNATCOLL.JSON.JSON_Value) return Geo_Coordinates with
     Global => null,
     Pre    => Has_Coord_Fields (Coordinates);
   --  Returns the geogrpahical coordinates associated with the given JSON value
   --  object.  Before it should be checked that the expected fields actually
   --  exist, hence making this requirement more prominent in the  precondition.

end Open_Weather_Map.API.Service.Utilities;
