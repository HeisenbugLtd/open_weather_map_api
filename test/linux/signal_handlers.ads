--------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+owm@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
--------------------------------------------------------------------------------
pragma License (Unrestricted);

limited with Ada.Interrupts.Names;
limited with System;

package Signal_Handlers is

   pragma Unreserve_All_Interrupts;

   protected Sigint with
     Interrupt_Priority => System.Interrupt_Priority'First
   is

      --------------------------------------------------------------------------
      --  Wait
      --------------------------------------------------------------------------
      entry Wait;

      --------------------------------------------------------------------------
      --  Handle
      --------------------------------------------------------------------------
      procedure Handle with
        Interrupt_Handler => True,
        Attach_Handler    => Ada.Interrupts.Names.SIGINT;

   private

      Call_Count : Natural := 0;

   end Sigint;

end Signal_Handlers;
