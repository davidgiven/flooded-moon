package body GenericVector is
	function "+" (v1, v2: Vector) return Vector is
		r: Vector;
	begin
		for i in v1'range loop
			r(i) := v1(i) + v2(i);
		end loop;
		return r;
	end;

	function "*" (v: Vector; n: Number) return Vector is
		r: Vector;
	begin
		for i in v'range loop
			r(i) := v(i) * n;
		end loop;
		return r;
	end;
end;

