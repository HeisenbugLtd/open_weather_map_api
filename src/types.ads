package Types with
  SPARK_Mode   => On,
  Pure         => True,
  Remote_Types => True
is

   type Scalar is delta 1.0 / 2 ** 32 range -2.0 ** 15 .. 2.0 ** 15
     with Small => 1.0 / 2 ** 32;

   type Degree is new Scalar range -180.0 ..  180.0; -- unit: °
   subtype Latitude  is Types.Degree range  -90.0 ..  90.0; --  90°S ..  90°N
   subtype Longitude is Types.Degree range -180.0 .. 180.0; -- 180°W .. 180°E

   type Humidity  is new Scalar range 0.0 ..  100.0; --  unit: %
   type Intensity is new Scalar range 0.0 ..    1.0 + Scalar'Small;
   type Pressure  is new Scalar range 0.0 .. 2048.0; --  unit: hPa

   --
   --  Temperature requires a bit of attention due to the different scales used.
   --

   Zero_Celsius : constant Scalar :=
     273.149_999_999_906_867_742_538_452_148_437_500;
   --  Physical constant denoting the different scales of Kelvin and Celsius
   --  (fixed point value closest to 273.15).

   Celsius_First : constant Scalar := -150.0;
   Celsius_Last  : constant Scalar :=  150.0;
   --  Supported range of temperature measurement, based on their respective
   --  lowest points.
   --  The temperature can be used in different contexts, so we need a rather
   --  large range.  For weather data we should at least support the
   --  temperature range found on Earth, this is roughly -90.0 °C to 56.0 °C for
   --  the air temperature, but ground temperatures of 94 °C have been recorded.
   --  Hence, to be on the safe side, let's support -150.0 to +150.0 °C.

   type Celsius is new Scalar range Celsius_First .. Celsius_Last;
   --  Scale from -150 °C to 150 °C.

   Kelvin_First : constant Scalar := Zero_Celsius + Celsius_First;
   Kelvin_Last  : constant Scalar := Zero_Celsius + Celsius_Last;

   type Kelvin is new Scalar range Kelvin_First .. Kelvin_Last;
   --  Same scale as Celsius, but in Kelvin, i.e. 123.15 .. 423.15.

   function To_Celsius (K : in Kelvin) return Celsius with
     Inline => True;

   function To_Kelvin (C : in Celsius) return Kelvin with
     Inline => True;

private

   function To_Celsius (K : in Kelvin) return Celsius is
     (Celsius (Scalar (K) - Zero_Celsius));

   function To_Kelvin (C : in Celsius) return Kelvin is
     (Kelvin (Scalar (C) + Zero_Celsius));

end Types;
