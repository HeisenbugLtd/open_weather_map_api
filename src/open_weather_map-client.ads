--------------------------------------------------------------------------------
--  Open_Weather_Map.Client
--
--  Provides a wrapper for an HTTP connection object including the
--  implementation of a rate limit.
--------------------------------------------------------------------------------

with AWS.Client;
limited with GNATCOLL.JSON;

package Open_Weather_Map.Client with
  SPARK_Mode => Off
is

   type T is tagged limited private;
   type T_Access is access all T'Class;

   function Create
     (Configuration : in GNATCOLL.JSON.JSON_Value;
      Rate_Limit    : in Ada.Real_Time.Time_Span := Default_Rate_Limit) return T_Access;
   --  Creates (dynamically allocates) and initializes a Client object.

   procedure Destroy (Self : in out T_Access);
   --  Destroys a Client object and frees the storage associated with it.

   procedure Initialize
     (Self          :    out T;
      Configuration : in     GNATCOLL.JSON.JSON_Value;
      Rate_Limit    : in     Ada.Real_Time.Time_Span := Default_Rate_Limit);
   --  Initializes a Client object according to the given configuration.
   --  Rate_Limit indicates the minimum time interval at which a call to
   --  Connection (see below) can be invoked without suspending the caller.

   function Connection
     (Self : in out T) return not null AWS.Client.HTTP_Connection_Access;
   --  Implements rate limiting by suspending a caller if calls to this function
   --  are not at least the time interval given in Rate_Limit apart.
   --
   --  NOTE: The implementation assumes that the returned connection object will
   --        not be stored by the caller, but rather requested each time a
   --        connection is being made.

private

   type T is new Ada.Finalization.Limited_Controlled with
      record
         Rate_Limit      : Ada.Real_Time.Time_Span;
         Last_Access     : Ada.Real_Time.Time;
         HTTP_Connection : AWS.Client.HTTP_Connection_Access;
      end record;

   overriding procedure Finalize (Self : in out T);

end Open_Weather_Map.Client;
