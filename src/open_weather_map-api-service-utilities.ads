--------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+owm@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
--------------------------------------------------------------------------------
pragma License (Unrestricted);

--------------------------------------------------------------------------------
--% @summary
--% Open_Weather_Map.API.Service.Utilities
--
--% @description
--% Provides several utility functions.
--------------------------------------------------------------------------------
private package Open_Weather_Map.API.Service.Utilities is

   -----------------------------------------------------------------------------
   --  Has_Coord_Fields
   -----------------------------------------------------------------------------
   function Has_Coord_Fields
     (Coordinates : in GNATCOLL.JSON.JSON_Value) return Boolean;
   --% Checks if the given JSON value object has the field names expected for
   --% geographical coordinates.
   --
   --% @param Coordinates
   --% The JSON object to be checked for expected coordinate fields.
   --
   --% @return
   --% True if the given JSON value object has the field names expected for
   --% geographical coordinates.

   -----------------------------------------------------------------------------
   --  Decode_Coordinates
   -----------------------------------------------------------------------------
   function Decode_Coordinates
     (Coordinates : in GNATCOLL.JSON.JSON_Value) return Geo_Coordinates with
     Global => null,
     Pre    => Has_Coord_Fields (Coordinates);
   --% Retrieves geographical coordinates from the JSON value given in
   --% Coordinates.
   --  Before calling this subprogram it should be checked that the expected
   --  fields actually exist, hence making this requirement more prominent in
   --  the precondition.
   --
   --% @param Coordinates
   --% The JSON object where the coordinates shall be extracted from.
   --
   --% @return
   --% The geographical coordinates extracted from Coordinates.

end Open_Weather_Map.API.Service.Utilities;
