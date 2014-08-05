with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Interfaces.C;
with Interfaces.C.Strings;
with Bigfiles;
with Config;

use Ada.Text_IO;
use Interfaces.C.Strings;
use Bigfiles;
use Config;

package body Calculon is
	package C is
		procedure Register_Callback(name, signature: chars_ptr;
				callback: System.Address);
		pragma import(C, Register_Callback, "ada_calculon_register_callback");
	end;

	package body Script is
		package C is
			function LastError return chars_ptr;
			pragma Import(C, LastError, "ada_calculon_last_error");

			function Create(code, signature: chars_ptr) return FuncCRef;
			pragma Import(C, Create, "ada_calculon_create");

			procedure Destroy(cimpl: FuncCRef);
			pragma Import(C, Destroy, "ada_calculon_destroy");

			procedure Dump(cimpl: FuncCRef);
			pragma Import(C, Dump, "ada_calculon_dump");

			function GetPointer(cimpl: FuncCRef) return T;
			pragma Import(C, GetPointer, "ada_calculon_get_pointer");
		end;

		procedure Initialise(cf: in out Func; code, signature: String) is
			codew, signaturew: wrapped_string_t;
		begin
			if (cf.impl /= null) then
				C.Destroy(cf.impl);
			end if;
			Wrap(codew, code);
			Wrap(signaturew, signature);
			cf.impl := C.Create(codew.c, signaturew.c);

			if (cf.impl = null) then
				raise compilation_exception with Value(C.LastError);
			end if;
		end;

		procedure Initialise_From_File(cf: in out Func;
				filename: string; signature: string) is
			bf: bigfile_t;
		begin
			bf.Open(filename);
			declare
				s: string(1..integer(bf.size));
				for s'address use bf.address;
			begin
				Initialise(cf, s, signature);
			end;
		end;

		procedure Finalize(cf: in out Func) is
		begin
			if (cf.impl /= null) then
				C.Destroy(cf.impl);
			end if;
		end;

		function Call(cf: Func) return T is
		begin
			return C.GetPointer(cf.impl);
		end;

		procedure Dump(cf: Func) is
		begin
			C.Dump(cf.impl);
		end;
	end;

	procedure Register_Callback(name, signature: string;
			callback: System.Address) is
		namew, signaturew: wrapped_string_t;
	begin
		Wrap(namew, name);
		Wrap(signaturew, signature);
		C.Register_Callback(namew.c, signaturew.c, callback);
	end;
end;

