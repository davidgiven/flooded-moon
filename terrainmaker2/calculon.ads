with Ada.Finalization;
with System;

package Calculon is
	type FuncC is null record;
	type FuncCRef is access FuncC;

	generic
		type T is private;
	package Script is
		compilation_exception: exception;

		type Func is new Ada.Finalization.Limited_Controlled with
		record
			impl: FuncCRef;
		end record;

		procedure Initialise(cf: in out Func; code, signature: string);
		procedure Initialise_From_File(cf: in out Func;
				filename: string; signature: string);
		procedure Finalize(cf: in out Func);

		procedure Dump(cf: Func);
		function Call(cf: Func) return T;
	end;

	procedure Register_Callback(name, signature: string;
			callback: System.Address);
end;

