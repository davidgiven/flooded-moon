with Ada.Text_IO;
with Config;

use Ada.Text_IO;
use Config;

package body GenericMatrix is
	function Identity return Matrix is
		r: Matrix := Zero;
	begin
		for i in 1..size loop
			r(i, i) := 1.0;
		end loop;
		return r;
	end;

	function "*" (m: Matrix; n: Number) return Matrix is
		r: Matrix;
	begin
		for x in 1..size loop
			for y in 1..size loop
				r(x, y) := m(x, y) * n;
			end loop;
		end loop;
		return r;
	end;

	function "*" (m: Matrix; v: Vector) return Vector is
		r: Vector;
		n: Number;
	begin
		for x in 1..size loop
			n := 0.0;
			for y in 1..size loop
				n := n + m(x,y)*v(y);
			end loop;
			r(x) := n;
		end loop;
		return r;
	end;

	function Invert(m: Matrix) return Matrix is
		a: Matrix := m;
		c: Matrix := Zero;
		L: Matrix := Zero;
		U: Matrix := Zero;
		b, d, x: Vector;
		coeff: Number;
	begin
		-- Forward elimination
		for k in 1..(size-1) loop
			for i in (k+1)..size loop
				coeff := a(i,k) / a(k,k);
				L(i,k) := coeff;
				for j in (k+1)..size loop
					a(i,j) := a(i,j) - coeff*a(k,j);
				end loop;
			end loop;
		end loop;

		-- Prepare L: diagonal elements are 1.0.
		for i in 1..size loop
			L(i,i) := 1.0;
		end loop;

		-- Prepare U: the upper triangular part of a.
		for j in 1..size loop
			for i in 1..j loop
				U(i,j) := a(i,j);
			end loop;
		end loop;

		-- Actually computer the inverse.
		for k in 1..size loop
			b(k) := 1.0;
			d(1) := b(1);

			-- Solve Ld = b using forward substitution.
			for i in 2..size loop
				d(i) := b(i);
				for j in 1..(i-1) loop
					d(i) := d(i) - L(i,j)*d(j);
				end loop;
			end loop;

			-- Solve Ux=d using back substitution.
			x(size) := d(size) / U(size,size);
			for i in reverse 1..(size-1) loop
				x(i) := d(i);
				for j in reverse (i+1)..size loop
					x(i) := x(i) - U(i,j)*x(j);
				end loop;
				x(i) := x(i) / u(i,i);
			end loop;

			-- Fill the solutions x(n) into row k of c.
			for i in 1..size loop
				c(k,i) := x(i);
			end loop;
			b(k) := 0.0;
		end loop;

		return c;
	end;

	function ToString(m: Matrix) return string is
		LF: character := ASCII.LF;

		function cols(x, y: integer) return string is
			(Number'image(m(x, y)) &
				(if (x < size) then (',' & cols(x+1, y)) else ""));

		function rows(y: integer) return string is
			(' ' & cols(1, y) & LF &
				(if (y < size) then rows(y+1) else ""));
	begin
		return '(' & LF & rows(1) & ')';
	end;
end;


