with Open_Weather_Map.Application;

procedure Main_OWM_Test with
  SPARK_Mode => Off
is
   App : Open_Weather_Map.Application.T;
begin
   App.Initialize;
   App.Run;
   App.Shutdown;
   pragma Unreferenced (App);
end Main_OWM_Test;
