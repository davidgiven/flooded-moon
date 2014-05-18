with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Ada.Finalization;
with Interfaces.C;
with Interfaces.C.Strings;
with Config;

use Ada.Text_IO;
use Interfaces.C;
use Interfaces.C.Strings;
use Config;

package body ConfigFiles is
	configerror: exception;

	package C is
		function Init return ConfigFileCRef;
		pragma Import(C, Init, "ada_config_init");

		procedure Destroy(cimpl: ConfigFileCRef);
		pragma Import(C, Destroy, "ada_config_destroy");

		function ErrorText(cimpl: ConfigFileCRef) return chars_ptr;
		pragma Import(C, ErrorText, "ada_config_error_text");

		function ReadFile(cimpl: ConfigFileCRef; filename: chars_ptr) return integer;
		pragma Import(C, ReadFile, "ada_config_read_file");

		function RootSetting(cimpl: ConfigFileCRef) return ConfigSettingCRef;
		pragma Import(C, RootSetting, "ada_config_root_setting");

		function GetMember(s: ConfigSettingCRef; element: chars_ptr) return ConfigSettingCRef;
		pragma Import(C, GetMember, "ada_config_setting_get_member");

		function GetDouble(s: ConfigSettingCRef) return Interfaces.C.double;
		pragma Import(C, GetDouble, "ada_config_setting_get_float");

		function GetString(s: ConfigSettingCRef) return chars_ptr;
		pragma Import(C, GetString, "ada_config_setting_get_string");

	end;

	type WrappedString is new Ada.Finalization.Limited_Controlled with
	record
		c: chars_ptr;
	end record;

	procedure Finalize(ws: in out WrappedString) is
	begin
		if (ws.c /= Null_Ptr) then
			Free(ws.c);
		end if;
	end;

	procedure WrapString(ws: in out WrappedString; s: string) is
	begin
		ws.c := New_String(s);
	end;

	procedure Adjust(cf: in out ConfigFile) is
	begin
		if (cf.impl /= null) then
			cf.impl.refcount := cf.impl.refcount + 1;
		end if;
	end;

	procedure Finalize(cf: in out ConfigFile) is
		procedure Free is
			new Ada.Unchecked_Deallocation(ConfigFileImpl, ConfigFileImplRef);
	begin
		if (cf.impl /= null) then
			cf.impl.refcount := cf.impl.refcount - 1;
			if (cf.impl.refcount = 0) then
				C.Destroy(cf.impl.c);
				Free(cf.impl);
			end if;
		end if;
	end;

	function Load(filename: string) return ConfigFile is
		cf: ConfigFile;
		filenamep: WrappedString;
	begin
		cf.impl := new ConfigFileImpl;
		cf.impl.c := C.Init;
		if (cf.impl.c = null) then
			raise configerror with "memory allocation error";
		end if;

		WrapString(filenamep, filename);
		if (C.ReadFile(cf.impl.c, filenamep.c) = 0) then
			raise configerror with Value(C.ErrorText(cf.impl.c));
		end if;
				
		cf.s := C.RootSetting(cf.impl.c);
		return cf;
	end;

	function Get(cf: ConfigFile; element: string) return ConfigFile is
		child: ConfigFile := cf;
		elementp: WrappedString;
	begin
		WrapString(elementp, element);
		child.s := C.GetMember(cf.s, elementp.c);
		if (child.s = null) then
			raise configerror with "value '" & element & "' not present";
		end if;
		return child;
	end;

	function Get(cf: ConfigFile) return Number is
	begin
		return Number(C.GetDouble(cf.s));
	end;

	function Get(cf: ConfigFile) return string is
		s: chars_ptr := C.GetString(cf.s);
	begin
		if (s = Null_Ptr) then
			raise configerror with "value missing or not a string";
		end if;
		return Value(s);
	end;
end;
