with Ada.Unchecked_Deallocation;
with Ada.Finalization;
with LuaJit;

use LuaJit;

package Script is
	type script_t is new Ada.Finalization.Limited_Controlled with
	record
		L: lua_state_t;
	end record;
		
	procedure Init_Thread;
	procedure Deinit_Thread;
end;

