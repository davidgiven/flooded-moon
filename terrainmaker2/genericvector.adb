package body GenericVector is
	function "+" (v1, v2: Vector) return Vector is
		r: Vector;
	begin
		for i in 1..size loop
			r(i) := v1(i) + v2(i);
		end loop;
		return r;
	end;

	function "*" (v: Vector; n: Number) return Vector is
		r: Vector;
	begin
		for i in 1..size loop
			r(i) := v(i) * n;
		end loop;
		return r;
	end;

	function ToString(v: Vector) return string is
		LF: character := ASCII.LF;

		function cells(x: integer) return string is
			(Number'image(v(x)) &
				(if (x < size) then (',' & cells(x+1)) else ""));
	begin
		return '(' & cells(1) & ')';
	end;
end;
