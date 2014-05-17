with Config;

use Config;

package body GenericMatrix is
	function Zero return Matrix is
		r: Matrix := (others => (others => 0.0));
	begin
		return r;
	end;

	function Identity return Matrix is
		r: Matrix := Zero;
	begin
		for i in 0..(size-1) loop
			r(i, i) := 1.0;
		end loop;
		return r;
	end;

	function Invert(m: Matrix) return Matrix is
		r: Matrix := Zero;
	begin
		return r;
	end;

	function ToString(m: Matrix) return string is
		LF: character := ASCII.LF;

		function cols(x, y: integer) return string is
			(Number'image(m(x, y)) &
				(if (x < (size-1)) then (',' & cols(x+1, y)) else ""));

		function rows(y: integer) return string is
			(' ' & cols(0, y) & LF &
				(if (y < (size-1)) then rows(y+1) else ""));
	begin
		return '(' & LF & rows(0) & ')';
	end;
end;


