--------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+owm@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
--------------------------------------------------------------------------------
pragma License (Unrestricted);

limited with Ada.Calendar.Time_Zones;
with Types;

package GNATCOLL.JSON.Conversions is

   -----------------------------------------------------------------------------
   --  To_Humidity
   -----------------------------------------------------------------------------
   function To_Humidity (Value : in JSON_Value;
                         Field : in UTF8_String) return Types.Humidity with
     Inline => True,
     Global => null,
     Pre    => (Value.Kind = JSON_Object_Type and then
                  Value.Get (Field).Kind in JSON_Int_Type | JSON_Float_Type);
   --% Converts a numeric (either float or integer) JSON field value into a
   --% Humidity.
   --
   --% @param Value
   --% The JSON value containing the humidity data.
   --
   --% @param Field
   --% The name of the field containing the humidity value.
   --
   --% @return
   --% The humidity value extracted from the given JSON value.

   -----------------------------------------------------------------------------
   --  To_Latitude
   -----------------------------------------------------------------------------
   function To_Latitude (Value : in JSON_Value;
                         Field : in UTF8_String) return Types.Latitude with
     Inline => True,
     Global => null,
     Pre    => (Value.Kind = JSON_Object_Type and then
                  Value.Get (Field).Kind in JSON_Int_Type | JSON_Float_Type);
   --% Converts a numeric (either float or integer) JSON field value into a
   --% Latitude.
   --
   --% @param Value
   --% The JSON value containing the latitude data.
   --
   --% @param Field
   --% The name of the field containing the latitude value.
   --
   --% @return
   --% The latitude value extracted from the given JSON value.

   -----------------------------------------------------------------------------
   --  To_Longitude
   -----------------------------------------------------------------------------
   function To_Longitude (Value : in JSON_Value;
                          Field : in UTF8_String) return Types.Longitude with
     Inline => True,
     Global => null,
     Pre    => (Value.Kind = JSON_Object_Type and then
                  Value.Get (Field).Kind in JSON_Int_Type | JSON_Float_Type);
   --% Converts a numeric (either float or integer) JSON field value into a
   --% Longitude.
   --
   --% @param Value
   --% The JSON value containing the longitude data.
   --
   --% @param Field
   --% The name of the field containing the longitude value.
   --
   --% @return
   --% The longitude value extracted from the given JSON value.

   -----------------------------------------------------------------------------
   --  To_Pressure
   -----------------------------------------------------------------------------
   function To_Pressure (Value : in JSON_Value;
                         Field : in UTF8_String) return Types.Pressure with
     Inline => True,
     Global => null,
     Pre    => (Value.Kind = JSON_Object_Type and then
                  Value.Get (Field).Kind in JSON_Int_Type | JSON_Float_Type);
   --% Converts a numeric (either float or integer) JSON field value into a
   --% Pressure.
   --
   --% @param Value
   --% The JSON value containing the pressure data.
   --
   --% @param Field
   --% The name of the field containing the pressure value.
   --
   --% @return
   --% The pressure value extracted from the given JSON value.

   -----------------------------------------------------------------------------
   --  To_Scalar
   -----------------------------------------------------------------------------
   function To_Scalar (Value : in JSON_Value;
                       Field : in UTF8_String) return Types.Scalar with
     Global => null,
     Pre    => (Value.Kind = JSON_Object_Type and then
                  Value.Get (Field).Kind in JSON_Int_Type | JSON_Float_Type);
   --% Converts a numeric (either float or integer) JSON field value into a
   --% Scalar.
   --
   --% @param Value
   --% The JSON value containing the numeric data.
   --
   --% @param Field
   --% The name of the field containing the numeric value.
   --
   --% @return
   --% The numeric value extracted from the given JSON value.

   -----------------------------------------------------------------------------
   --  To_Temperature
   -----------------------------------------------------------------------------
   function To_Temperature
     (Value : in JSON_Value;
      Field : in UTF8_String) return Types.Kelvin with
     Global => null,
     Pre    => (Value.Kind = JSON_Object_Type and then
                  Value.Get (Field).Kind in JSON_Int_Type | JSON_Float_Type);
   --% Converts a numeric (either float or integer) JSON field value, expected
   --% to be a value given in Kelvin.
   --
   --% @param Value
   --% The JSON value containing the temperature data.
   --
   --% @param Field
   --% The name of the field containing the temperature value.
   --
   --% @return
   --% The temperature value extracted from the given JSON value.

   -----------------------------------------------------------------------------
   --  To_Time
   -----------------------------------------------------------------------------
   function To_Time (Value : in JSON_Value;
                     Field : in UTF8_String) return Ada.Calendar.Time with
     Global => null,
     Pre    => (Value.Kind = JSON_Object_Type and then
                  Value.Get (Field).Kind = JSON_Int_Type);
   --% Converts an integer JSON field which is expected to contain a Unix time
   --% (i.e. number of seconds since 1970-01-01) into a proper Ada Time.
   --
   --% @param Value
   --% The JSON value containing the time data.
   --
   --% @param Field
   --% The name of the field containing the time value.
   --
   --% @return
   --% The time extracted from the given JSON value.

   -----------------------------------------------------------------------------
   --  To_Time_Offset
   -----------------------------------------------------------------------------
   function To_Time_Offset
     (Value : in JSON_Value;
      Field : in UTF8_String) return Ada.Calendar.Time_Zones.Time_Offset with
     Global => null,
     Pre    => (Value.Kind = JSON_Object_Type and then
                  Value.Get (Field).Kind = JSON_Int_Type);
   --% Converts an integer JSON field which is expected to contain a time zone
   --% offset in seconds into Time_Offset.
   --
   --% @param Value
   --% The JSON value containing the time zone data.
   --
   --% @param Field
   --% The name of the field containing the time zone value.
   --
   --% @return
   --% The time zone offset extracted from the given JSON value.

end GNATCOLL.JSON.Conversions;
