with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Interfaces.C;
with Interfaces.C.Strings;
with Bigfiles;

use Ada.Text_IO;
use Interfaces.C.Strings;
use Bigfiles;

package body Calculon is
	package C is
		function LastError return chars_ptr;
		pragma Import(C, LastError, "ada_calculon_last_error");

		function Create(code, signature: chars_ptr) return FuncCRef;
		pragma Import(C, Create, "ada_calculon_create");

		procedure Destroy(cimpl: FuncCRef);
		pragma Import(C, Destroy, "ada_calculon_destroy");

		function GetPointer(cimpl: FuncCRef) return T;
		pragma Import(C, GetPointer, "ada_calculon_get_pointer");
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

	procedure Initialise(cf: in out Func; code, signature: String) is
		codew, signaturew: WrappedString;
	begin
		if (cf.impl /= null) then
			C.Destroy(cf.impl);
		end if;
		WrapString(codew, code);
		WrapString(signaturew, signature);
		cf.impl := C.Create(codew.c, signaturew.c);

		if (cf.impl = null) then
			raise CompilationException with Value(C.LastError);
		end if;
	end;

	procedure InitialiseFromFile(cf: in out Func;
			filename: string; signature: string) is
		bf: BigFile;
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
end;

