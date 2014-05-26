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
	package C is
		function Init return ConfigFileCRef;
		pragma Import(C, Init, "ada_config_init");

		procedure Destroy(cimpl: ConfigFileCRef);
		pragma Import(C, Destroy, "ada_config_destroy");

		function ErrorText(cimpl: ConfigFileCRef) return chars_ptr;
		pragma Import(C, ErrorText, "ada_config_error_text");

		function ErrorFile(cimpl: ConfigFileCRef) return chars_ptr;
		pragma Import(C, ErrorFile, "ada_config_error_file");

		function ErrorLine(cimpl: ConfigFileCRef) return integer;
		pragma Import(C, ErrorLine, "ada_config_error_line");

		function ReadFile(cimpl: ConfigFileCRef; filename: chars_ptr) return integer;
		pragma Import(C, ReadFile, "ada_config_read_file");

		function RootSetting(cimpl: ConfigFileCRef) return ConfigSettingCRef;
		pragma Import(C, RootSetting, "ada_config_root_setting");

		function GetMember(s: ConfigSettingCRef; element: chars_ptr) return ConfigSettingCRef;
		pragma Import(C, GetMember, "ada_config_setting_get_member");

		function GetElement(s: ConfigSettingCRef; element: integer) return ConfigSettingCRef;
		pragma Import(C, GetElement, "ada_config_setting_get_elem");

		function Length(s: ConfigSettingCRef) return integer;
		pragma Import(C, Length, "ada_config_setting_length");

		function GetDouble(s: ConfigSettingCRef) return Interfaces.C.double;
		pragma Import(C, GetDouble, "ada_config_setting_get_float");

		function GetString(s: ConfigSettingCRef) return chars_ptr;
		pragma Import(C, GetString, "ada_config_setting_get_string");

		function Name(s: ConfigSettingCRef) return chars_ptr;
		pragma Import(C, Name, "ada_config_setting_name");

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

	function Create return ConfigFile is
		cf: ConfigFile;
	begin
		cf.impl := new ConfigFileImpl;
		cf.impl.c := C.Init;
		if (cf.impl.c = null) then
			raise ConfigError with "memory allocation error";
		end if;

		cf.s := C.RootSetting(cf.impl.c);
		return cf;
	end;

	procedure Load(cf: in out ConfigFile; filename: string) is
		filenamep: WrappedString;
	begin
		WrapString(filenamep, filename);
		if (C.ReadFile(cf.impl.c, filenamep.c) = 0) then
			raise ConfigError with
				Value(C.ErrorText(cf.impl.c)) & " at " &
				Value(C.ErrorFile(cf.impl.c)) & ":" &
				C.ErrorLine(cf.impl.c)'img;
		end if;

		cf.s := C.RootSetting(cf.impl.c);
	end;

	function Exists(cf: ConfigFile; element: string) return boolean is
		elementp: WrappedString;
	begin
		WrapString(elementp, element);
		return (C.GetMember(cf.s, elementp.c) /= null);
	end;

	function Get(cf: ConfigFile; element: string) return ConfigFile is
		child: ConfigFile := cf;
		elementp: WrappedString;
	begin
		WrapString(elementp, element);
		child.s := C.GetMember(cf.s, elementp.c);
		if (child.s = null) then
			raise ConfigError with "value '" & element & "' not present";
		end if;
		return child;
	end;

	function Get(cf: ConfigFile; element: integer) return ConfigFile is
		child: ConfigFile := cf;
	begin
		child.s := C.GetElement(cf.s, element);
		if (child.s = null) then
			raise ConfigError with "value [" & element'img & "] not present";
		end if;
		return child;
	end;

	function Length(cf: ConfigFile) return integer is
	begin
		return C.Length(cf.s);
	end;

	function Name(cf: ConfigFile) return string is
		s: chars_ptr := C.Name(cf.s);
	begin
		if (s = Null_Ptr) then
			raise ConfigError with "value missing or does not have a name";
		end if;
		return Value(s);
	end;

	function Value(cf: ConfigFile) return Number is
	begin
		return Number(C.GetDouble(cf.s));
	end;

	function Value(cf: ConfigFile) return string is
		s: chars_ptr := C.GetString(cf.s);
	begin
		if (s = Null_Ptr) then
			raise ConfigError with "value missing or not a string";
		end if;
		return Value(s);
	end;
end;
