--------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+owm@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
--------------------------------------------------------------------------------
pragma License (Unrestricted);

--------------------------------------------------------------------------------
--  Open_Weather_Map.City_Ids
--
--  Provides the Ada language version of city Ids.
--
--  Please note that this list may change, the actual list can be downloaded
--  from <http://bulk.openwathermap.org/sample/city.list.json.gz>).
--------------------------------------------------------------------------------

package Open_Weather_Map.City_Ids is

   package Mexico is
      --  Mexico/Guerrero (B. smithii)
      Acapulco_de_Juarez : constant City_Id := 3533462;
      Chacalapa_Acapulco : constant City_Id := 3531263;
      Coyuca_de_Benitez  : constant City_Id := 4012608;
      Las_Guacamayas     : constant City_Id := 4026075;
      Tierra_Colorada    : constant City_Id := 3515643;
      Xaltianguis        : constant City_Id := 3514583;
   end Mexico;

   package India is
      --  India (P. Metallica)
      Giddalur : constant City_Id := 1271213;
      Nandyal  : constant City_Id := 1261927;
   end India;

end Open_Weather_Map.City_Ids;
