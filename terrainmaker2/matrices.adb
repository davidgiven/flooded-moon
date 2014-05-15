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

	function "-"(left, right: Matrix) return Matrix is
		r: Matrix;
	begin
		for y in 0..3 loop
			for x in 0..3 loop
				r(x, y) := left(x, y) - right(x, y);
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

	function Transpose(m: Matrix) return Matrix is
		r: Matrix;
	begin
		for x in 0..3 loop
			for y in 0..3 loop
				r(x, y) := m(y, x);
			end loop;
		end loop;
		return r;
	end;

	function Invert(m: Matrix) return Matrix is
		r: Matrix;
		invdet, det: Number;

		function minor(r0, r1, r2, c0, c1, c2: integer) return Number is
		begin
			return
				m(r0, c0) * (m(r1, c1)*m(r2, c2) - m(r2, c1)*m(r1, c2)) -
				m(r0, c1) * (m(r1, c0)*m(r2, c2) - m(r2, c0)*m(r1, c2)) +
				m(r0, c2) * (m(r1, c0)*m(r2, c1) - m(r2, c0)*m(r1, c1));
		end;
	begin
		r(0, 0) :=  minor(1, 2, 3, 1, 2, 3);
		r(1, 0) := -minor(0, 2, 3, 1, 2, 3);
		r(2, 0) :=  minor(0, 1, 3, 1, 2, 3);
		r(3, 0) := -minor(0, 1, 2, 1, 2, 3);

		r(0, 1) := -minor(1, 2, 3, 0, 2, 3);
		r(1, 1) :=  minor(0, 2, 3, 0, 2, 3);
		r(2, 1) := -minor(0, 1, 3, 0, 2, 3);
		r(3, 1) :=  minor(0, 1, 2, 0, 2, 3);

		r(0, 2) :=  minor(1, 2, 3, 0, 1, 3);
		r(1, 2) := -minor(0, 2, 3, 0, 1, 3);
		r(2, 2) :=  minor(0, 1, 3, 0, 1, 3);
		r(3, 2) := -minor(0, 1, 2, 0, 1, 3);

		r(0, 3) := -minor(1, 2, 3, 0, 1, 2);
		r(1, 3) :=  minor(0, 2, 3, 0, 1, 2);
		r(2, 3) := -minor(0, 1, 3, 0, 1, 2);
		r(3, 3) :=  minor(0, 1, 2, 0, 1, 2);
										 
		det := m(0, 0)*minor(1, 2, 3, 1, 2, 3) -
			   m(1, 0)*minor(1, 2, 3, 0, 2, 3) +
			   m(2, 0)*minor(1, 2, 3, 0, 1, 3) -
			   m(3, 0)*minor(1, 2, 3, 0, 1, 2);

		invdet := 1.0 / det;
		for x in 0..3 loop
			for y in 0..3 loop
				r(x, y) := r(x, y) * invdet;
			end loop;
		end loop;
		
		return r;
	end;

	function ToString(m: Matrix) return string is
		LF: character := ASCII.LF;

		function S(x, y: integer) return String is
			(Number'Image(m(x, y)));
	begin
		return "(" & S(0, 0) & "," & S(1, 0) & "," & S(2, 0) & "," & S(3, 0) & LF
		     & " " & S(0, 1) & "," & S(1, 1) & "," & S(2, 1) & "," & S(3, 1) & LF
		     & " " & S(0, 2) & "," & S(1, 2) & "," & S(2, 2) & "," & S(3, 2) & LF
		     & " " & S(0, 3) & "," & S(1, 3) & "," & S(2, 3) & "," & S(3, 3) & ")";
	end;
end;

