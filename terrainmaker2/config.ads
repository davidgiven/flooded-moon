with Interfaces.C;
with Ada.Numerics;
with Ada.Numerics.Generic_Elementary_Functions;

use Ada.Numerics;

package Config is
	type Number is new Interfaces.C.double;

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
end;

