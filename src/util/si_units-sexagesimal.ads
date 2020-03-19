--------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+owm@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
--------------------------------------------------------------------------------
pragma License (Unrestricted);

limited with Ada.Text_IO;

generic
   type Degree is delta <>;
   Default_Aft : in Ada.Text_IO.Field;
package SI_Units.Sexagesimal is

   package Latitude is

      subtype T is Degree range
        Degree'Max (Degree'First, -90.0) .. Degree'Min (Degree'Last, 90.0);

      function Image
        (L   : in T;
         Aft : in Ada.Text_IO.Field := Default_Aft) return String;

   end Latitude;

   package Longitude is

      subtype T is Degree range
        Degree'Max (Degree'First, -180.0) .. Degree'Min (Degree'Last, 180.0);

      function Image
        (L   : in T;
         Aft : in Ada.Text_IO.Field := Default_Aft) return String;

   end Longitude;

end SI_Units.Sexagesimal;
