with Interfaces.C;

package Config is
	type Number is new Interfaces.C.double;

	package Options is
		function Output_Filename return string;
		function Width return integer;
		function Height return integer;
		function Scene_Filename return string;
	end;

	procedure ParseOptions;
end;

