with Ada.Finalization;

generic
	type T is private;
package Calculon is
	CompilationException: exception;

	type FuncC is null record;
	type FuncCRef is access FuncC;

	type Func is new Ada.Finalization.Limited_Controlled with
	record
		impl: FuncCRef;
	end record;

	procedure Initialise(cf: in out Func; code, signature: string);
	procedure InitialiseFromFile(cf: in out Func;
			filename: string; signature: string);
	procedure Finalize(cf: in out Func);

	function Call(cf: Func) return T;
end;

