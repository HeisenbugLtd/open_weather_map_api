--------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+owm@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
--------------------------------------------------------------------------------
pragma License (Unrestricted);

with Ada.Directories;
with Ada.Text_IO;

package body Open_Weather_Map.Configuration is

   My_Debug : constant not null GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create (Unit_Name => "OWM.CONFIGURATION");

   Config_Name : constant String := "config";
   Config_Ext  : constant String := "json";
   --  Full configuration file name: "$HOME/.openweathermap/config.json".

   procedure Initialize (Self : out T) is
   begin
      My_Debug.all.Trace (Message => "Initialize");

      Self.Read_Config
        (From_File =>
           Ada.Directories.Compose
             (Containing_Directory => Application_Directory,
              Name                 => Config_Name,
              Extension            => Config_Ext));
   end Initialize;

   procedure Read_Config (Self      : in out T;
                          From_File : in     String) is
      Content : Ada.Strings.Unbounded.Unbounded_String;
   begin
      My_Debug.all.Trace (Message => "Read_Config");
      My_Debug.all.Trace
        (Message =>
           "Read_Config: Reading configuration file """ & From_File & """.");

      declare
         JSON_File : Ada.Text_IO.File_Type;
      begin
         Ada.Text_IO.Open (File => JSON_File,
                           Mode => Ada.Text_IO.In_File,
                           Name => From_File);

         begin
            while not Ada.Text_IO.End_Of_File (File => JSON_File) loop
               Ada.Strings.Unbounded.Append
                 (Source   => Content,
                  New_Item => Ada.Text_IO.Get_Line (JSON_File));
            end loop;
         exception
            when others =>
               Ada.Text_IO.Close (File => JSON_File);
               raise;
         end;

         Ada.Text_IO.Close (File => JSON_File);
      exception
         when E : others =>
            My_Debug.all.Trace
              (E   => E,
               Msg => "Read_Config: Error reading """ & From_File & """: ");
            Ada.Text_IO.Put_Line
              (File => Ada.Text_IO.Standard_Error,
               Item =>
                  "Error reading from file """ & From_File & """, " &
                  "no configuration loaded!");
      end;

      begin
         Self.Config := GNATCOLL.JSON.Read (Strm     => Content,
                                            Filename => From_File);
         My_Debug.all.Trace (Message => "Read_Config: Configuration loaded.");
      exception
         when E : GNATCOLL.JSON.Invalid_JSON_Stream =>
            My_Debug.all.Trace
              (E   => E,
               Msg => "Read_Config: Invalid data in JSON stream: ");
            --  Error reporting already done by callee.
      end;
   end Read_Config;

end Open_Weather_Map.Configuration;
