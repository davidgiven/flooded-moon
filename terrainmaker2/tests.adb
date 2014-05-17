with Ada.Text_IO;
with Ada.Strings;
with Config;
with Matrices;
with Vectors;
with ImageWriter;
with Colours;
with Utils;

use Ada.Text_IO;
use Ada.Strings;
use Config;
use Colours;
use Vectors;
use all type Vectors.Vector3;
use Matrices;
use all type Matrices.Matrix3;

procedure Tests is
	LF: character := ASCII.LF;

	function CompareWS(s1, s2: string) return boolean is
		i1: integer := s1'first;
		i2: integer := s2'first;
		c1, c2: character;

		function isws(c: character) return boolean is
			((c = LF) or (c = ' '));
	begin
		while (i1 <= s1'last) and (i2 <= s2'last) loop
			c1 := s1(i1);
			c2 := s2(i2);

			if not isws(c1) and not isws(c2) then
				if (c1 /= c2) then
					return false;
				end if;
				i1 := i1 + 1;
				i2 := i2 + 1;
			else
				if isws(c1) then
					i1 := i1 + 1;
				end if;
				if isws(c2) then
					i2 := i2 + 1;
				end if;
			end if;
		end loop;

		return true;
	end;

	procedure Check(b: boolean; msg: string) is
	begin
		if not b then
			Put_Line(Standard_Error, "Test failed: " & msg);
			Utils.Error("Test failure");
		end if;
	end;

	procedure MatrixMultiplication is
		m: Matrix3 := ((3.0, 2.0, 4.0),
					   (2.0,-3.0, 1.0),
					   (1.0, 1.0, 2.0));
	begin
		m := Invert(m);
		Check(
			CompareWS(ToString(m),
				"( 1.00000000000000E+00, 0.00000000000000E+00,-2.00000000000000E+00 4.28571428571428E-01,-2.85714285714286E-01,-7.14285714285714E-01 -7.14285714285714E-01, 1.42857142857143E-01, 1.85714285714286E+00)"),
			"MatrixMultiplication fail:" & LF & ToString(m));
	end;

begin
	MatrixMultiplication;
end;



