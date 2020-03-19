limited with Ada.Real_Time;
private with GNATCOLL.Memory;

package Standard_Application with
  SPARK_Mode => Off
is

   type T is abstract tagged limited private;

   procedure Initialize (Self                  : in out T;
                         Cycle_Time            : in     Ada.Real_Time.Time_Span;
                         Application_Directory : in     String;
                         Log_Name              : in     String);

   procedure Shutdown (Self : in out T);

   function Epoch (Self : in T) return Ada.Real_Time.Time;

   procedure Run (Self : in out T);

   procedure Work (Self : in out T) is abstract;

private

   type T is abstract tagged limited
      record
         Start_Time      : Ada.Real_Time.Time;
         Interval        : Ada.Real_Time.Time_Span;
         Previous_Memory : GNATCOLL.Memory.Watermark_Info;
      end record;

   procedure Report_Memory_Usage (Self : in out T);

end Standard_Application;
