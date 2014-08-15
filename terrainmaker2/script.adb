with LuaJit;

use LuaJit;

package body Script is
	procedure Init_Thread
	is
		L: lua_state_t := LuaJit.NewState;
	begin
		LuaJit.Set_Thread_State(L);
	end;

	procedure Deinit_Thread
	is
		L: lua_state_t := LuaJit.Get_Thread_State;
	begin
		LuaJit.Close(L);
	end;
end;
		
