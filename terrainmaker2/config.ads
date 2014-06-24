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

	type number is new Interfaces.C.double;

	type wrapped_string_t is new Ada.Finalization.Limited_Controlled with
	record
		c: chars_ptr;
	end record;

	procedure Finalize(ws: in out wrapped_string_t);
	procedure Wrap(ws: in out wrapped_string_t; s: string);

	package Number_Functions is
		new Ada.Numerics.Generic_Elementary_Functions(number);

	package Options is
		function Output_Filename return string;
		function Width return integer;
		function Height return integer;
		function Scene_Filename return string;
		function Number_Of_Threads return integer;
	end;

	procedure ParseOptions;

	function DegToRad(n: number) return number is
		(n * (Pi / 180.0));

	LF: constant character := ASCII.LF;
	CR: constant character := ASCII.CR;
end;

