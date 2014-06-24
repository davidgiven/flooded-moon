
package body Colours is
	function Mix(c1, c2: colour_t; a: number) return colour_t is
	begin
		return RGB(
			c1.r*(1.0-a) + c2.r*a,
			c1.g*(1.0-a) * c2.g*a,
			c1.b*(1.0-a) * c2.b*a
		);
	end;

	function Load(cf: node_t) return colour_t is
	begin
		if (cf.Length /= 3) then
			raise config_parse_exception with "expected colour";
		end if;
		
		return RGB(
			cf(0).Value,
			cf(1).Value,
			cf(2).Value);
	end;
end;

