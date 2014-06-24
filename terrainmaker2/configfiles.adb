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
		function Init return node_c_ref;
		pragma Import(C, Init, "ada_config_init");

		procedure Destroy(cimpl: node_c_ref);
		pragma Import(C, Destroy, "ada_config_destroy");

		function ErrorText(cimpl: node_c_ref) return chars_ptr;
		pragma Import(C, ErrorText, "ada_config_error_text");

		function ErrorFile(cimpl: node_c_ref) return chars_ptr;
		pragma Import(C, ErrorFile, "ada_config_error_file");

		function ErrorLine(cimpl: node_c_ref) return integer;
		pragma Import(C, ErrorLine, "ada_config_error_line");

		function ReadFile(cimpl: node_c_ref; filename: chars_ptr) return integer;
		pragma Import(C, ReadFile, "ada_config_read_file");

		function RootSetting(cimpl: node_c_ref) return setting_c_ref;
		pragma Import(C, RootSetting, "ada_config_root_setting");

		function GetMember(s: setting_c_ref; element: chars_ptr) return setting_c_ref;
		pragma Import(C, GetMember, "ada_config_setting_get_member");

		function GetElement(s: setting_c_ref; element: integer) return setting_c_ref;
		pragma Import(C, GetElement, "ada_config_setting_get_elem");

		function Length(s: setting_c_ref) return integer;
		pragma Import(C, Length, "ada_config_setting_length");

		function GetDouble(s: setting_c_ref) return Interfaces.C.double;
		pragma Import(C, GetDouble, "ada_config_setting_get_float");

		function GetString(s: setting_c_ref) return chars_ptr;
		pragma Import(C, GetString, "ada_config_setting_get_string");

		function Name(s: setting_c_ref) return chars_ptr;
		pragma Import(C, Name, "ada_config_setting_name");

	end;

	procedure Adjust(cf: in out node_t) is
	begin
		if (cf.impl /= null) then
			cf.impl.refcount := cf.impl.refcount + 1;
		end if;
	end;

	procedure Finalize(cf: in out node_t) is
		procedure Free is
			new Ada.Unchecked_Deallocation(node_impl_t, node_impl_ref);
	begin
		if (cf.impl /= null) then
			cf.impl.refcount := cf.impl.refcount - 1;
			if (cf.impl.refcount = 0) then
				C.Destroy(cf.impl.c);
				Free(cf.impl);
			end if;
		end if;
	end;

	function Create return node_t is
		cf: node_t;
	begin
		cf.impl := new node_impl_t;
		cf.impl.c := C.Init;
		if (cf.impl.c = null) then
			raise config_exception with "memory allocation error";
		end if;

		cf.s := C.RootSetting(cf.impl.c);
		return cf;
	end;

	procedure Load(cf: in out node_t; filename: string) is
		filenamep: wrapped_string_t;
	begin
		Wrap(filenamep, filename);
		if (C.ReadFile(cf.impl.c, filenamep.c) = 0) then
			raise config_exception with
				Value(C.ErrorText(cf.impl.c)) & " at " &
				Value(C.ErrorFile(cf.impl.c)) & ":" &
				C.ErrorLine(cf.impl.c)'img;
		end if;

		cf.s := C.RootSetting(cf.impl.c);
	end;

	function Exists(cf: node_t; element: string) return boolean is
		elementp: wrapped_string_t;
	begin
		Wrap(elementp, element);
		return (C.GetMember(cf.s, elementp.c) /= null);
	end;

	function Get(cf: node_t; element: string) return node_t is
		child: node_t := cf;
		elementp: wrapped_string_t;
	begin
		Wrap(elementp, element);
		child.s := C.GetMember(cf.s, elementp.c);
		if (child.s = null) then
			raise config_exception with "value '" & element & "' not present";
		end if;
		return child;
	end;

	function Get(cf: node_t; element: integer) return node_t is
		child: node_t := cf;
	begin
		child.s := C.GetElement(cf.s, element);
		if (child.s = null) then
			raise config_exception with "value [" & element'img & "] not present";
		end if;
		return child;
	end;

	function Length(cf: node_t) return integer is
	begin
		return C.Length(cf.s);
	end;

	function Name(cf: node_t) return string is
		s: chars_ptr := C.Name(cf.s);
	begin
		if (s = Null_Ptr) then
			raise config_exception with "value missing or does not have a name";
		end if;
		return Value(s);
	end;

	function Value(cf: node_t) return number is
	begin
		return number(C.GetDouble(cf.s));
	end;

	function Value(cf: node_t) return string is
		s: chars_ptr := C.GetString(cf.s);
	begin
		if (s = Null_Ptr) then
			raise config_exception with "value missing or not a string";
		end if;
		return Value(s);
	end;
end;
