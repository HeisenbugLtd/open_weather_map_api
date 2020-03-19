limited with Ada.Text_IO;

generic
   type Degree is delta <>;
   Default_Aft : in Ada.Text_IO.Field;
package SI_Units.Sexagesimal with
  SPARK_Mode => Off
is

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
