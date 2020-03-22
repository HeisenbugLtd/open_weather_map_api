--------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+owm@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
--------------------------------------------------------------------------------
pragma License (Unrestricted);

with Ada.Characters.Latin_1;
with Ada.Directories;
with Ada.Strings.Fixed;
with GNATCOLL.Traces;
with Signal_Handlers;
with SI_Units.Binary;

package body Standard_Application is

   My_Debug : constant not null GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create (Unit_Name => "STDAPP");

   LF : Character renames Ada.Characters.Latin_1.LF;
   Underline : constant String := Ada.Strings.Fixed."*" (79, '=') & LF;

   -----------------------------------------------------------------------------
   --  Binary_Image
   -----------------------------------------------------------------------------
   function Binary_Image is
     new SI_Units.Binary.Image (Item        => GNATCOLL.Memory.Byte_Count,
                                Default_Aft => 3,
                                Unit        => "B");

   -----------------------------------------------------------------------------
   --  Epoch
   -----------------------------------------------------------------------------
   function Epoch (Self : in T) return Ada.Real_Time.Time is
     (Self.Start_Time);

   -----------------------------------------------------------------------------
   --  Initialize
   -----------------------------------------------------------------------------
   procedure Initialize (Self                  : in out T;
                         Cycle_Time            : in     Ada.Real_Time.Time_Span;
                         Application_Directory : in     String;
                         Log_Name              : in     String)
   is
      Log_File_Name : constant String :=
        Ada.Directories.Compose
          (Containing_Directory => Application_Directory,
           Name                 => Log_Name,
           Extension            => "log");
   begin
      Self.Interval   := Cycle_Time;
      Self.Start_Time := Ada.Real_Time.Clock;

      Self.Previous_Memory := GNATCOLL.Memory.Watermark_Info'(High    => 0,
                                                              Current => 0);

      GNATCOLL.Traces.Parse_Config
        (Config =>
           "+"  &                  LF & --  Activate all logging by default.
           ">>" & Log_File_Name  & LF & --  Redirect to "owm.log"
           "DEBUG.ABSOLUTE_TIME" & LF); --  Show timestamps.
      GNATCOLL.Traces.Parse_Config_File; --  User overrides via config file.

      My_Debug.all.Trace (Message => "Initialize");
      My_Debug.all.Trace
        (Message => Underline & "Application started." & LF & Underline);
      Self.Report_Memory_Usage;
   end Initialize;

   -----------------------------------------------------------------------------
   --  Report_Memory_Usage
   -----------------------------------------------------------------------------
   procedure Report_Memory_Usage (Self : in out T)
   is
      subtype Byte_Count is GNATCOLL.Memory.Byte_Count;
      use type Byte_Count;

      --------------------------------------------------------------------------
      --  Trim
      --------------------------------------------------------------------------
      function Trim
        (Source : in String;
         Side   : in Ada.Strings.Trim_End := Ada.Strings.Left) return String
         renames Ada.Strings.Fixed.Trim;

      --------------------------------------------------------------------------
      --  Full_Image
      --------------------------------------------------------------------------
      function Full_Image (Value : in Byte_Count) return String;

      --------------------------------------------------------------------------
      --  Full_Image
      --------------------------------------------------------------------------
      function Full_Image (Value : in Byte_Count) return String is
        (Trim (Value'Image) & " [" & Binary_Image (Value) & "]");

      Mem_Info : constant GNATCOLL.Memory.Watermark_Info :=
        GNATCOLL.Memory.Get_Allocations;
      use type GNATCOLL.Memory.Watermark_Info;
   begin
      My_Debug.all.Trace (Message => "Report_Memory_Usage");

      if Mem_Info /= Self.Previous_Memory then
         Report_Changes :
         declare
            Diff_Current : constant Byte_Count :=
              Mem_Info.Current - Self.Previous_Memory.Current;
            Diff_Peak    : constant Byte_Count :=
              Mem_Info.High - Self.Previous_Memory.High;
         begin
            Self.Previous_Memory := Mem_Info;

            My_Debug.all.Trace
              (Message =>
                 "Memory usage: Current: " & Full_Image (Mem_Info.Current) &
                 " (diff: "                & Full_Image (Diff_Current)     &
                 "), Peak: "               & Full_Image (Mem_Info.High)    &
                 " (diff: "                & Full_Image (Diff_Peak)        &
                 ").");
         end Report_Changes;
      end if;
   end Report_Memory_Usage;

   -----------------------------------------------------------------------------
   --  Run
   -----------------------------------------------------------------------------
   procedure Run (Self : in out T)
   is
      use type Ada.Real_Time.Time;
      Next_Update : Ada.Real_Time.Time := Self.Epoch;
   begin
      My_Debug.all.Trace (Message => "Run");

      Main_Loop :
      loop
         T'Class (Self).Work; --  Do the actual work.

         Next_Update := Next_Update + Self.Interval;

         select
            Signal_Handlers.Sigint.Wait;
            exit Main_Loop;
         or
            delay until Next_Update;
         end select;
      end loop Main_Loop;
   end Run;

   -----------------------------------------------------------------------------
   --  Shutdown
   -----------------------------------------------------------------------------
   procedure Shutdown (Self : in out T) is
   begin
      My_Debug.all.Trace (Message => "Shutdown");

      Self.Report_Memory_Usage;
      My_Debug.all.Trace
        (Message => Underline & "Application stopped." & LF & Underline);
   end Shutdown;

end Standard_Application;
