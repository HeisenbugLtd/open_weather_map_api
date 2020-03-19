package body SI_Units.Sexagesimal with
  SPARK_Mode => Off
is

   package Fixed_IO is new Ada.Text_IO.Fixed_IO (Num => Degree);

   package body Latitude is

      function Image (L   : in T;
                      Aft : in Ada.Text_IO.Field := Default_Aft) return String
      is
         Result : String (1 .. 3 + Ada.Text_IO.Field'Max (1, Aft)); -- "90.[...]";
      begin
         Fixed_IO.Put (To   => Result,
                       Item => abs L,
                       Aft  => Aft,
                       Exp  => 0);

         return Trim (Result & Degree_Sign & No_Break_Space &
                      (if L < 0.0
                         then "S"
                         else "N"));
      end Image;

   end Latitude;

   package body Longitude is

      function Image (L   : in T;
                      Aft : in Ada.Text_IO.Field := Default_Aft) return String
      is
         Result : String (1 .. 4 + Ada.Text_IO.Field'Max (1, Aft)); -- "180.[...]";
      begin
         Fixed_IO.Put (To   => Result,
                       Item => abs L,
                       Aft  => Aft,
                       Exp  => 0);

         return Trim (Result & Degree_Sign & No_Break_Space &
                      (if L < 0.0
                         then "W"
                         else "E"));
      end Image;

   end Longitude;

end SI_Units.Sexagesimal;
