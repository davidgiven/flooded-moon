with Interfaces.C;
with Interfaces.C.Strings;
with Ada.Numerics;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Finalization;

use Ada.Numerics;
use Interfaces.C.Strings;

package Config is
	MaxObjects: constant Natural := 10;
	ProgressBarSize: constant natural := 75;

	type Number is new Interfaces.C.double;

	type WrappedString is new Ada.Finalization.Limited_Controlled with
	record
		c: chars_ptr;
	end record;

	procedure Finalize(ws: in out WrappedString);
	procedure WrapString(ws: in out WrappedString; s: string);

	package NumberFunctions is
		new Ada.Numerics.Generic_Elementary_Functions(Number);

	package Options is
		function Output_Filename return string;
		function Width return integer;
		function Height return integer;
		function Scene_Filename return string;
	end;

	procedure ParseOptions;

	function DegToRad(n: Number) return Number is
		(n * (Pi / 180.0));

	LF: constant character := ASCII.LF;
	CR: constant character := ASCII.CR;
end;

