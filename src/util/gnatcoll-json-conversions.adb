--------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+owm@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
--------------------------------------------------------------------------------
pragma License (Unrestricted);

package body GNATCOLL.JSON.Conversions is

   Unix_Epoch : constant Ada.Calendar.Time :=
     Ada.Calendar.Time_Of (Year  => 1970,
                           Month => 1,
                           Day   => 1);

   function To_Humidity (Value : in JSON_Value;
                         Field : in UTF8_String) return Types.Humidity is
   begin
      return Types.Humidity (To_Scalar (Value, Field));
   end To_Humidity;

   function To_Latitude (Value : in JSON_Value;
                         Field : in UTF8_String) return Types.Latitude is
   begin
      return Types.Latitude (To_Scalar (Value, Field));
   end To_Latitude;

   function To_Longitude (Value : in JSON_Value;
                          Field : in UTF8_String) return Types.Longitude is
   begin
      return Types.Longitude (To_Scalar (Value, Field));
   end To_Longitude;

   function To_Pressure (Value : in JSON_Value;
                         Field : in UTF8_String) return Types.Pressure is
   begin
      return Types.Pressure (To_Scalar (Value, Field));
   end To_Pressure;

   function To_Scalar (Value : in JSON_Value;
                       Field : in UTF8_String) return Types.Scalar is
      Field_Value : constant JSON_Value := Value.Get (Field);
   begin
      case Field_Value.Kind is
         when JSON_Int_Type =>
            return Types.Scalar (Long_Long_Integer'(Field_Value.Get));
         when JSON_Float_Type =>
            return Types.Scalar (Field_Value.Get_Long_Float);
         when others =>
            raise Program_Error;
      end case;
   end To_Scalar;

   function To_Temperature (Value : in JSON_Value;
                            Field : in UTF8_String) return Types.Kelvin is
   begin
      return Types.Kelvin (To_Scalar (Value, Field));
   end To_Temperature;

   function To_Time (Value : in JSON_Value;
                     Field : in UTF8_String) return Ada.Calendar.Time is
      use type Ada.Calendar.Time;
   begin
      return Unix_Epoch + Duration (Long_Integer'(Value.Get (Field)));
   end To_Time;

   function To_Time_Offset
     (Value : in JSON_Value;
      Field : in UTF8_String) return Ada.Calendar.Time_Zones.Time_Offset is
   begin
      return Ada.Calendar.Time_Zones.Time_Offset (Long_Integer'(Value.Get (Field)) / 60);
   end To_Time_Offset;

end GNATCOLL.JSON.Conversions;
