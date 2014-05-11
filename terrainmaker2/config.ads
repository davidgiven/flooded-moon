with Interfaces.C;

package Config is
	type Number is new Interfaces.C.double;

	package Options is
		function Output_Filename return string;
	end;
end;

