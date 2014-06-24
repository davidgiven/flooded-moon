with Config;

use Config.Number_Functions;

package body GenericVector is
	function "+" (v1, v2: Vector) return Vector is
		r: Vector;
	begin
		for i in Index loop
			r(i) := v1(i) + v2(i);
		end loop;
		return r;
	end;

	function "-" (v1, v2: Vector) return Vector is
		r: Vector;
	begin
		for i in Index loop
			r(i) := v1(i) - v2(i);
		end loop;
		return r;
	end;

	function "*" (v: Vector; n: number) return Vector is
		r: Vector;
	begin
		for i in Index loop
			r(i) := v(i) * n;
		end loop;
		return r;
	end;

	function Length(v: Vector) return number is
		len2: number := 0.0;
	begin
		for i in Index loop
			len2 := len2 + v(i)**2;
		end loop;
		return sqrt(len2);
	end;

	function Normalise(v: Vector) return Vector is
		len: number := Length(v);
	begin
		return r: Vector do
			for i in Index loop
				r(i) := v(i) / len;
			end loop;
		end return;
	end;

	function Dot(v1, v2: Vector) return number is
		d: number := 0.0;
	begin
		for i in Index loop
			d := d + v1(i)*v2(i);
		end loop;
		return d;
	end;

	function ToString(v: Vector) return string is
		function cells(x: Index) return string is
			(v(x)'img &
				(if (x < Index'last) then (',' & cells(x+1)) else ""));
	begin
		return '(' & cells(Index'first) & ')';
	end;
end;

