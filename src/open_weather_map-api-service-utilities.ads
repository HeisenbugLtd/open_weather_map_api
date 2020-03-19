--------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+owm@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
--------------------------------------------------------------------------------
pragma License (Unrestricted);

--------------------------------------------------------------------------------
--  Open_Weather_Map.API.Service.Utilities
--
--  Provides several utility functions.
--------------------------------------------------------------------------------

private package Open_Weather_Map.API.Service.Utilities is

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
