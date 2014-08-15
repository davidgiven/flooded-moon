with System;

package LuaJit is
	subtype lua_state_t is System.Address;

	function NewState return lua_state_t;
	pragma import(C, NewState, "luaL_newstate");

	procedure Close(L: lua_state_t);
	pragma import(C, Close, "lua_close");

	procedure OpenLibs(L: lua_state_t);
	pragma import(C, OpenLibs, "luaL_openlibs");

	function GetTop(L: lua_state_t) return integer;
	pragma import(C, GetTop, "lua_gettop");

	procedure Insert(L: lua_state_t);
	pragma import(C, Insert, "lua_insert");

	function PCall(L: lua_state_t; inarg, outarg, errfunc: integer) return integer;
	pragma import(C, PCall, "lua_pcall");

	procedure Remove(L: lua_state_t; n: integer);
	pragma import(C, Remove, "lua_remove");

	procedure PushString(L: lua_state_t; value: string);
	function LoadFile(L: lua_state_t; filename: string) return integer;
	function DoFile(L: lua_state_t; filename: string) return boolean;

	function Get_Thread_State return lua_state_t;
	procedure Set_Thread_State(L: lua_state_t);
end;

