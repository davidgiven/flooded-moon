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
end;
