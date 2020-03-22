--------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+owm@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
--------------------------------------------------------------------------------
pragma License (Unrestricted);

limited with Ada.Real_Time;
private with GNATCOLL.Memory;

package Standard_Application is

   type T is abstract tagged limited private;

   -----------------------------------------------------------------------------
   --  Initialize
   -----------------------------------------------------------------------------
   procedure Initialize (Self                  : in out T;
                         Cycle_Time            : in     Ada.Real_Time.Time_Span;
                         Application_Directory : in     String;
                         Log_Name              : in     String);
   --% Initializes the application.
   --
   --% @param Self
   --% Instance of the application to initialize.
   --
   --% @param Cycle_Time
   --% The interval at which the Worker subprogram shall be called.
   --
   --% @param Application_Directory
   --% Application specific directory containing configuration data and such.
   --
   --% @param Log_Name
   --% Base name of the application log file.

   -----------------------------------------------------------------------------
   --  Shutdown
   -----------------------------------------------------------------------------
   procedure Shutdown (Self : in out T);
   --% Shuts down the application.
   --
   --% @param Self
   --% Instance of the application to shut down.

   -----------------------------------------------------------------------------
   --  Epoch
   -----------------------------------------------------------------------------
   function Epoch (Self : in T) return Ada.Real_Time.Time;
   --% Returns the epoch (i.e. startup time) of the application. Used to
   --% generate relative time stamps.
   --
   --% @param Self
   --% Instance of the application.
   --
   --% @return
   --% The start time of the application.

   -----------------------------------------------------------------------------
   --  Run
   -----------------------------------------------------------------------------
   procedure Run (Self : in out T);
   --% Starts the application main loop.
   --
   --% @param Self
   --% Instance of the application to start.

   -----------------------------------------------------------------------------
   --  Work
   -----------------------------------------------------------------------------
   procedure Work (Self : in out T) is abstract;
   --% The work to be done on each iteration of the main loop.
   --
   --% @param Self
   --% Instance of the application.

private

   type T is abstract tagged limited
      record
         Start_Time      : Ada.Real_Time.Time;
         --% @field Start_Time
         --% Time the application has been initialized.
         Interval        : Ada.Real_Time.Time_Span;
         --% @field Interval
         --% The interval at which the Work subprogram shall be called from
         --% within the Run subprogram.
         Previous_Memory : GNATCOLL.Memory.Watermark_Info;
         --% @field Previous_Memory
         --% Holds the last value of the application's memory consumption as
         --% reported by the operating system.
      end record;

   -----------------------------------------------------------------------------
   --  Report_Memory_Usage
   -----------------------------------------------------------------------------
   procedure Report_Memory_Usage (Self : in out T);
   --% Prints memory usage to the log file.
   --
   --% @param Self
   --% Instance of the application to report memory usage for.

end Standard_Application;
