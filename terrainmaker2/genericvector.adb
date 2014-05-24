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

	function "*" (v: Vector; n: Number) return Vector is
		r: Vector;
	begin
		for i in Index loop
			r(i) := v(i) * n;
		end loop;
		return r;
	end;

	function Length(v: Vector) return Number is
		len: Number := 0.0;
	begin
		for i in Index loop
			len := len + v(i);
		end loop;
		return len;
	end;

	function Normalise(v: Vector) return Vector is
		len: Number := Length(v);
	begin
		return r: Vector do
			for i in Index loop
				r(i) := v(i) / len;
			end loop;
		end return;
	end;

	function ToString(v: Vector) return string is
		LF: character := ASCII.LF;

		function cells(x: Index) return string is
			(Number'image(v(x)) &
				(if (x < Index'last) then (',' & cells(x+1)) else ""));
	begin
		return '(' & cells(Index'first) & ')';
	end;
end;

