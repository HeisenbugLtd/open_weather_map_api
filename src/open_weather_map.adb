--------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+owm@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
--------------------------------------------------------------------------------
pragma License (Unrestricted);

with Ada.Directories;
with Ada.Environment_Variables;

package body Open_Weather_Map is

   --  Application files are expected to reside in the home directory under an
   --  application specific subdirectory. Under Unixoids the home directory is
   --  denoted by the environment variable HOME, under Windows this is
   --  USERPROFILE.
   --  So we try HOME first, if that fails, we try USERPROFILE.
   Home_Unix    : constant String := "HOME";
   Home_Windows : constant String := "USERPROFILE";
   App_Dir_Name : constant String := ".openweathermap";

   Full_Application_Directory : constant String :=
     Ada.Directories.Compose
       (Containing_Directory =>
          (if Ada.Environment_Variables.Exists (Name => Home_Unix) then
                Ada.Environment_Variables.Value (Name => Home_Unix)
           elsif Ada.Environment_Variables.Exists (Name => Home_Windows) then
                Ada.Environment_Variables.Value (Name => Home_Windows)
           else Ada.Directories.Current_Directory),
        Name                 => App_Dir_Name);

   -----------------------------------------------------------------------------
   --  Application_Directory
   -----------------------------------------------------------------------------
   function Application_Directory return String is
   begin
      return Full_Application_Directory;
   end Application_Directory;

end Open_Weather_Map;
