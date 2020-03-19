--------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+owm@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
--------------------------------------------------------------------------------
pragma License (Unrestricted);

with Open_Weather_Map.Application;

procedure Main_OWM_Test is
   App : Open_Weather_Map.Application.T;
begin
   App.Initialize;
   App.Run;
   App.Shutdown;
   pragma Unreferenced (App);
end Main_OWM_Test;
