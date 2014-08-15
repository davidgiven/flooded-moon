with Config;
with Interfaces.C.Strings;

use Config;
use Interfaces.C.Strings;

package body LuaJit is
	package C is
		procedure PushString(L: lua_state_t; value: chars_ptr);
		pragma import(C, PushString, "lua_pushstring");

		function LoadFile(L: lua_state_t; filename: chars_ptr) return integer;
		pragma import(C, LoadFile, "luaL_loadfile");
	end;

	procedure PushString(L: lua_state_t; value: string)
	is
		valuew: wrapped_string_t;
	begin
		Wrap(valuew, value);
		C.PushString(L, valuew.c);
	end;

	function LoadFile(L: lua_state_t; filename: string) return integer
	is
		filenamew: wrapped_string_t;
	begin
		Wrap(filenamew, filename);
		return C.LoadFile(L, filenamew.c);
	end;

	function DoFile(L: lua_state_t; filename: string) return boolean
	is
	begin
		return (LoadFile(L, filename) = 0) and then (PCall(L, 0, -1, 0) = 0);
	end;

	current_L: lua_state_t;
	pragma Thread_Local_Storage(current_L);

	function Get_Thread_State return lua_state_t
	is
	begin
		return current_L;
	end;

	procedure Set_Thread_State(L: lua_state_t)
	is
	begin
		current_L := L;
	end;
end;

