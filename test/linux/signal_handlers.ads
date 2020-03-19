limited with Ada.Interrupts.Names;
limited with System;

package Signal_Handlers with
  SPARK_Mode => Off
is

   pragma Unreserve_All_Interrupts;

   protected Sigint with
     Interrupt_Priority => System.Interrupt_Priority'First
   is

      entry Wait;

      procedure Handle with
        Interrupt_Handler => True,
        Attach_Handler    => Ada.Interrupts.Names.SIGINT;

   private

      Call_Count : Natural := 0;

   end Sigint;

end Signal_Handlers;
