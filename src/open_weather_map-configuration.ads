--------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+owm@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
--------------------------------------------------------------------------------
pragma License (Unrestricted);

--------------------------------------------------------------------------------
--  Open_Weather_Map.Configuration
--
--  Provides a configuration object to be used throughout the API to configure
--  certain connection and API related parameters.
--------------------------------------------------------------------------------

with GNATCOLL.JSON;

package Open_Weather_Map.Configuration is

   -----------------------------------------------------------------------------
   --  Configuration object.
   --
   --  Reads a JSON file from the application specific directory and stores the
   --  data.  Data can be retrieved by a call to the Configuration_Data method.
   -----------------------------------------------------------------------------
   type T is tagged limited private;

   subtype Configuration_Values is GNATCOLL.JSON.JSON_Value;

   procedure Initialize (Self : out T);
   --  Initializes the configuration object.
   --  This is a rather heavy operation as it tries to read a file in JSON
   --  format and stores its contents internally.

   function Values (Self : in T) return Configuration_Values with
     Inline => True;
   --  Returns the JSON object associated with the configuration file read when
   --  the object was initialized.

private

   type T is tagged limited
      record
         Config : Configuration_Values;
      end record;

   procedure Read_Config (Self      : in out T;
                          From_File : in     String);

   function Values (Self : in T) return Configuration_Values is
     (Self.Config);

end Open_Weather_Map.Configuration;
