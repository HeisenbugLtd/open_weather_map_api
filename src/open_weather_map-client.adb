--------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+owm@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
--------------------------------------------------------------------------------
pragma License (Unrestricted);

with Ada.Environment_Variables;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with GNATCOLL.JSON;

package body Open_Weather_Map.Client is

   My_Debug : constant not null GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create (Unit_Name => "OWM.CLIENT");

   procedure Free is
     new Ada.Unchecked_Deallocation
       (Object => AWS.Client.HTTP_Connection,
        Name   => AWS.Client.HTTP_Connection_Access);

   type T_Local_Access is access all T;

   procedure Free is new Ada.Unchecked_Deallocation (Object => T,
                                                     Name   => T_Local_Access);

   function Connection
     (Self : in out T) return not null AWS.Client.HTTP_Connection_Access
   is
      use type Ada.Real_Time.Time;

      Next_Allowed : constant Ada.Real_Time.Time := Self.Last_Access + Self.Rate_Limit;
      Now          : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;
   begin
      My_Debug.all.Trace (Message => "Connection");

      if Next_Allowed > Now then
         My_Debug.all.Trace
           (Message => "Connection: Rate limit exceeded, throttling...");
         delay until Next_Allowed;
         My_Debug.all.Trace (Message => "Connection: Rate limited.");

         Self.Last_Access := Next_Allowed;
      else
         Self.Last_Access := Now;
      end if;

      return Self.HTTP_Connection;
   end Connection;

   function Create
     (Configuration : in GNATCOLL.JSON.JSON_Value;
      Rate_Limit    : in Ada.Real_Time.Time_Span := Default_Rate_Limit) return T_Access
   is
      Result : T_Access;
   begin
      My_Debug.all.Trace (Message => "Create");

      begin
         Result := new T;
         Result.all.Initialize (Configuration => Configuration,
                                Rate_Limit    => Rate_Limit);

         return Result;
      exception
         when others =>
            Destroy (Result);
            raise;
      end;
   end Create;

   procedure Destroy (Self : in out T_Access) is
   begin
      if Self /= null then
         Self.all.Finalize;
         Free (T_Local_Access (Self));
      end if;
   end Destroy;

   overriding procedure Finalize (Self : in out T) is
   begin
      My_Debug.all.Trace (Message => "Finalize");

      Free (Self.HTTP_Connection);
   end Finalize;

   procedure Initialize
     (Self          :    out T;
      Configuration : in     GNATCOLL.JSON.JSON_Value;
      Rate_Limit    : in     Ada.Real_Time.Time_Span := Default_Rate_Limit)
   is
      --  Proxy information.
      Network_Address : Ada.Strings.Unbounded.Unbounded_String;
      User            : Ada.Strings.Unbounded.Unbounded_String;
      Password        : Ada.Strings.Unbounded.Unbounded_String;

      function "+"
        (Source : in Ada.Strings.Unbounded.Unbounded_String) return String
         renames Ada.Strings.Unbounded.To_String;
   begin
      My_Debug.all.Trace (Message => "Initialize");

      Self.Rate_Limit  := Rate_Limit;
      Self.Last_Access := Ada.Real_Time.Time_First;

      Get_Proxy_Information :
      declare
         use type Ada.Strings.Unbounded.Unbounded_String;
      begin
         --  TODO: Better error handling.
         My_Debug.all.Trace
           (Message => "Initialize: Loading proxy configuration...");

         --  Try to get proxy URL from configuration file.
         if
           Configuration.Has_Field (Field => Config_Names.Field_Network_Address)
         then
            Network_Address :=
              Configuration.Get (Field => Config_Names.Field_Network_Address);
         end if;

         --  If there's still no proxy URL, try to get it from the environment
         --  variables.
         if
           Network_Address = AWS.Client.No_Data and then
           Ada.Environment_Variables.Exists
             (Name => Config_Names.Env_Network_Address)
         then
            Network_Address :=
              Ada.Strings.Unbounded.To_Unbounded_String
                (Source =>
                   Ada.Environment_Variables.Value
                     (Name => Config_Names.Env_Network_Address));
         end if;

         --  Proxy user and password are only useful if there actually is a
         --  proxy.
         if Network_Address /= AWS.Client.No_Data then
            if Configuration.Has_Field (Field => Config_Names.Field_User) then
               User := Configuration.Get (Field => Config_Names.Field_User);

               --  Passwords only make sense if there's also a username.
               if
                 Configuration.Has_Field (Field => Config_Names.Field_Password)
               then
                  Password :=
                    Configuration.Get (Field => Config_Names.Field_Password);
               end if;
            end if;
         end if;

         My_Debug.all.Trace
           (Message => "Initialize: Proxy configuration loaded.");
      exception
         when E : others =>
            My_Debug.all.Trace
              (E   => E,
               Msg => "Initialize: Error parsing configuration data: ");
            Ada.Text_IO.Put_Line
              (File => Ada.Text_IO.Standard_Error,
               Item =>
                  "Warning: Missing or invalid JSON data, " &
                  "proxy configuration may only be partial.");
      end Get_Proxy_Information;

      My_Debug.all.Trace
        (Message =>
           "Initialize: Proxy """ & (+Network_Address) & """" &
           ", user: """ & (+User) & """, password not shown.");

      Create_Network_Connection :
      begin
         My_Debug.all.Trace
           (Message => "Initialize: Creating network connection...");

         Self.HTTP_Connection := new AWS.Client.HTTP_Connection'
           (AWS.Client.Create
              (Host       => API_Host,
               Proxy      => +Network_Address,
               Proxy_User => +User,
               Proxy_Pwd  => +Password,
               Timeouts   => AWS.Client.Timeouts (Each => 10.0)));

         My_Debug.all.Trace
           (Message => "Initialize: Network connection created.");
      exception
         when E : others =>
            My_Debug.all.Trace
              (E   => E,
               Msg => "Initialize: Error creating network connection: ");
            Ada.Text_IO.Put_Line
              (File => Ada.Text_IO.Standard_Error,
               Item => "Error: HTTP connection failed!");
      end Create_Network_Connection;
   end Initialize;

end Open_Weather_Map.Client;
