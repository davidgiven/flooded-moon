with Config;
with ConfigFiles;

use Config;
use ConfigFiles;

package body Vectors is
	function Load(cf: node_t) return Vector3 is
	begin
		if (cf.Length /= 3) then
			raise config_parse_exception with "expected vector";
		end if;
		
		return Vector3'(
			cf(0).Value,
			cf(1).Value,
			cf(2).Value);
	end;
end;
