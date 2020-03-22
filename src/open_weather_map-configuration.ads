--------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+owm@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
--------------------------------------------------------------------------------
pragma License (Unrestricted);

with GNATCOLL.JSON;

--------------------------------------------------------------------------------
--% @summary
--% Open_Weather_Map.Configuration
--
--% @description
--% Provides a configuration object to be used throughout the API to configure
--% certain connection and API related parameters.
--------------------------------------------------------------------------------
package Open_Weather_Map.Configuration is

   type T is tagged limited private;
   --% Configuration object.
   --
   --% Reads a JSON file from the application specific directory and stores the
   --% data.  Data can be retrieved by a call to the Configuration_Data method.

   subtype Configuration_Values is GNATCOLL.JSON.JSON_Value;
   --% Convenience rename for the returned configuration object.

   ---------------------------------------------------------------------------
   --  Initialize
   ---------------------------------------------------------------------------
   procedure Initialize (Self : out T);
   --% Initializes the configuration object.
   --  This is a rather heavy operation as it tries to read a file in JSON
   --  format and stores its contents internally.
   --
   --% @param Self
   --% The instance of the configriation object to initialize.

   ---------------------------------------------------------------------------
   --  Values
   ---------------------------------------------------------------------------
   function Values (Self : in T) return Configuration_Values with
     Inline => True;
   --% Returns the configuration data associated with the configuration file
   --% read when the object was initialized.
   --
   --% @param Self
   --% The instance of the configuration object.
   --
   --% @return
   --% The configuration data stored in the configuration object.

private

   type T is tagged limited
      record
         Config : Configuration_Values;
         --% @field Config
         --% The configuration data (JSON value).
      end record;

   ---------------------------------------------------------------------------
   --  Read_Config
   --
   --% Reads configuration from given JSON file.
   --
   --% @param Self
   --% Instance of the object the configuration shall be read into.
   --
   --% @param From_File
   --% Name of the file the configuration data shall be read from.
   ---------------------------------------------------------------------------
   procedure Read_Config (Self      : in out T;
                          From_File : in     String);

   ---------------------------------------------------------------------------
   --  Values
   ---------------------------------------------------------------------------
   function Values (Self : in T) return Configuration_Values is
     (Self.Config);

end Open_Weather_Map.Configuration;
