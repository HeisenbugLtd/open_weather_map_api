--------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+owm@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
--------------------------------------------------------------------------------
pragma License (Unrestricted);

package Types with
  Pure         => True,
  Remote_Types => True
is

   type Scalar is delta 1.0 / 2 ** 32 range -2.0 ** 15 .. 2.0 ** 15
     with Small => 1.0 / 2 ** 32;
   --% Fixed point type all other types are derived from.

   type Degree is new Scalar range -180.0 ..  180.0;
   --% Type representing degrees.
   --% unit: °

   subtype Latitude  is Types.Degree range  -90.0 ..  90.0;
   --% Type representing a latitude.
   --% 90°S ..  90°N

   subtype Longitude is Types.Degree range -180.0 .. 180.0;
   --% Type representing a longitude.
   --% 180°W .. 180°E

   type Humidity is new Scalar range 0.0 ..  100.0;
   --% Type representing humidity.
   --% unit: %

   type Pressure is new Scalar range 0.0 .. 2048.0;
   --% Type representing (atmospheric) pressure.
   --% unit: hPa

   --
   --  Temperature requires a bit of attention due to the different scales used.
   --

   Zero_Celsius : constant Scalar :=
     273.149_999_999_906_867_742_538_452_148_437_500;
   --% Physical constant denoting the different scales of Kelvin and Celsius.
   --  (Fixed point value closest to 273.15).

   --  Supported range of temperature measurement, based on their respective
   --  lowest points.
   --  The temperature can be used in different contexts, so we need a rather
   --  large range.  For weather data we should at least support the
   --  temperature range found on Earth, this is roughly -90.0 °C to 56.0 °C for
   --  the air temperature, but ground temperatures of 94 °C have been recorded.
   --  Hence, to be on the safe side, let's support -150.0 to +150.0 °C.

   Celsius_First : constant Scalar := -150.0;
   --% Minimum temperature measurement supported (in degree Celsius).

   Celsius_Last  : constant Scalar :=  150.0;
   --% Maximum temperature measurement supported (in degree Celsius).

   type Celsius is new Scalar range Celsius_First .. Celsius_Last;
   --% Supported temperature measurement range in degree Celsius (-150 °C to
   --% 150 °C).

   Kelvin_First : constant Scalar := Zero_Celsius + Celsius_First;
   --% Minimum temperature measurement supported (in Kelvin).

   Kelvin_Last  : constant Scalar := Zero_Celsius + Celsius_Last;
   --% Maximum temperature measurement supported (in Kelvin).

   type Kelvin is new Scalar range Kelvin_First .. Kelvin_Last;
   --% Supported temperature measurement range in Kelvin.
   --% Same scale as Celsius, but in Kelvin, i.e. 123.15 .. 423.15.

   -----------------------------------------------------------------------------
   --  To_Celsius
   -----------------------------------------------------------------------------
   function To_Celsius (K : in Kelvin) return Celsius with
     Inline => True;
   --% Converts a temperature from Kelvin to degree Celsius.
   --
   --% @param K
   --% The value to be converted from Kelvin to degree Celsius.

   -----------------------------------------------------------------------------
   --  To_Kelvin
   -----------------------------------------------------------------------------
   function To_Kelvin (C : in Celsius) return Kelvin with
     Inline => True;
   --% Converts a temperature from degree Celsius to Kelvin.
   --
   --% @param C
   --% The value to be converted from degree Celsius to Kelvin.

private

   -----------------------------------------------------------------------------
   --  To_Celsius
   -----------------------------------------------------------------------------
   function To_Celsius (K : in Kelvin) return Celsius is
     (Celsius (Scalar (K) - Zero_Celsius));

   -----------------------------------------------------------------------------
   --  To_Kelvin
   -----------------------------------------------------------------------------
   function To_Kelvin (C : in Celsius) return Kelvin is
     (Kelvin (Scalar (C) + Zero_Celsius));

end Types;
