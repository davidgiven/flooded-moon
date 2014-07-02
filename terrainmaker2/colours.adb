
package body Colours is
	function Load(cf: node_t) return colour_t is
	begin
		if (cf.Length /= 3) then
			raise config_parse_exception with "expected colour";
		end if;
		
		return (cf(0).Value, cf(1).Value, cf(2).Value);
	end;
end;

