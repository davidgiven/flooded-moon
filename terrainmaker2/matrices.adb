with Matrices;
with Config;
with Ada.Characters.Latin_1;

use Config;

package body Matrices is
	function "+"(left, right: Matrix) return Matrix is
		r: Matrix;
	begin
		for y in 0..3 loop
			for x in 0..3 loop
				r(x, y) := left(x, y) + right(x, y);
			end loop;
		end loop;
		return r;
	end;

	function "*"(left, right: Matrix) return Matrix is
		r: Matrix;
		sum: Number;
	begin
		for i in 0..3 loop
			for j in 0..3 loop
				sum := 0.0;
				for k in 0..3 loop
					sum := sum + left(i, k) * right(k, j);
				end loop;
				r(i, j) := sum;
			end loop;
		end loop;
		return r;
	end;

	function ToString(m: Matrix) return string is
		LF: string := (1 => Ada.Characters.Latin_1.LF);

		function S(x, y: integer) return String is
			(Number'Image(m(x, y)));
	begin
		return "(" & S(0, 0) & "," & S(1, 0) & "," & S(2, 0) & "," & S(3, 0) & LF
		     & " " & S(0, 1) & "," & S(1, 1) & "," & S(2, 1) & "," & S(3, 1) & LF
		     & " " & S(0, 2) & "," & S(1, 2) & "," & S(2, 2) & "," & S(3, 2) & LF
		     & " " & S(0, 3) & "," & S(1, 3) & "," & S(2, 3) & "," & S(3, 3) & ")";
	end;
end;

