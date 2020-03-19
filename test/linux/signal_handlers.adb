--------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+owm@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
--------------------------------------------------------------------------------
pragma License (Unrestricted);

package body Signal_Handlers is

   protected body Sigint is

      entry Wait when Call_Count > 0 is
      begin
         Call_Count := Call_Count - 1;
      end Wait;

      procedure Handle is
      begin
         Call_Count := Call_Count + 1;
      end Handle;

   end Sigint;

end Signal_Handlers;
