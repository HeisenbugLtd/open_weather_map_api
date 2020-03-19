package body Signal_Handlers with
  SPARK_Mode => Off
is

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
