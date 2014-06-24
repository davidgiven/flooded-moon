with Ada.Text_IO;
with Config;

use Ada.Text_IO;
use Config;

package body GenericMatrix is
	function Identity return Matrix is
		r: Matrix := Zero;
	begin
		return r: Matrix do
			for i in Index loop
				r(i, i) := 1.0;
			end loop;
		end return;
	end;

	function "*" (m: Matrix; n: number) return Matrix is
	begin
		return r: Matrix do
			for x in Index loop
				for y in Index loop
					r(x, y) := m(x, y) * n;
				end loop;
			end loop;
		end return;
	end;

	function "*" (m: Matrix; v: Vector) return Vector is
		n: number;
	begin
		return r: Vector do
			for x in Index loop
				n := 0.0;
				for y in Index loop
					n := n + m(x,y)*v(y);
				end loop;
				r(x) := n;
			end loop;
		end return;
	end;

	function "*" (m: Matrix; o: Matrix) return Matrix is
		sum: number;
	begin
		return r: Matrix do
			for x in Index loop
				for y in Index loop
					sum := 0.0;
					for z in Index loop
						sum := sum + m(x,z)*o(y,z);
					end loop;
					r(x,y) := sum;
				end loop;
			end loop;
		end return;
	end;
		
	function Invert(m: Matrix) return Matrix is
		a: Matrix := m;
		c: Matrix := Zero;
		L: Matrix := Zero;
		U: Matrix := Zero;
		b, d, x: Vector;
		coeff: number;
	begin
		-- Forward elimination
		for k in Index'first..(Index'last - 1) loop
			for i in (k+1)..(Index'last) loop
				coeff := a(i,k) / a(k,k);
				L(i,k) := coeff;
				for j in (k+1)..(Index'last) loop
					a(i,j) := a(i,j) - coeff*a(k,j);
				end loop;
			end loop;
		end loop;

		-- Prepare L: diagonal elements are 1.0.
		for i in Index loop
			L(i,i) := 1.0;
		end loop;

		-- Prepare U: the upper triangular part of a.
		for j in Index loop
			for i in Index'first..j loop
				U(i,j) := a(i,j);
			end loop;
		end loop;

		-- Actually computer the inverse.
		for k in Index loop
			b(k) := 1.0;
			d(Index'first) := b(Index'first);

			-- Solve Ld = b using forward substitution.
			for i in (Index'first+1)..(Index'last) loop
				d(i) := b(i);
				for j in Index'first..(i-1) loop
					d(i) := d(i) - L(i,j)*d(j);
				end loop;
			end loop;

			-- Solve Ux=d using back substitution.
			x(Index'last) := d(Index'last) / U(Index'last, Index'last);
			for i in reverse (Index'first)..(Index'last-1) loop
				x(i) := d(i);
				for j in reverse (i+1)..(Index'last) loop
					x(i) := x(i) - U(i,j)*x(j);
				end loop;
				x(i) := x(i) / u(i,i);
			end loop;

			-- Fill the solutions x(n) into row k of c.
			for i in (Index'first)..(Index'last) loop
				c(k,i) := x(i);
			end loop;
			b(k) := 0.0;
		end loop;

		return c;
	end;

	function ToString(m: Matrix) return string is
		function cols(x, y: Index) return string is
			(m(x, y)'img &
				(if (x < Index'last) then (',' & cols(x+1, y)) else ""));

		function rows(y: Index) return string is
			(' ' & cols(Index'first, y) & LF &
				(if (y < Index'last) then rows(y+1) else ""));
	begin
		return '(' & LF & rows(Index'first) & ')';
	end;
end;


