with Config;
with ConfigFiles;

use Config;
use ConfigFiles;

package body Vectors is
	function Load(cf: node_t) return vec3_t is
	begin
		if (cf.Length /= 3) then
			raise config_parse_exception with "expected vector";
		end if;
		
		return (cf(0).Value, cf(1).Value, cf(2).Value);
	end;

	function Perpendicular(v: vec3_t) return vec3_t is
		-- To find a vector w which is perpendicular to v, we take the
		-- cross product of v with an arbitrary vector which isn't
		-- colinear with v. That gives us a vector which is perpendicular
		-- to both.
	begin
		-- This will of course fail if the normal is very close to the vector...
		-- but it'll hardly ever happen and if it turns out to be a problem
		-- I'll fix it later.
		return Cross(Normalise(v), (0.0, 0.0, 1.0));
	end;
end;
