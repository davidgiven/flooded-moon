with Vectors;
with Config;

use Vectors;
use Config;

package Planet is
	type Class is tagged record
		-- Assigned properties
		location: vec3_t := (0.0, 0.0, 0.0);
		nominal_radius: number := 0.0;
		atmospheric_depth: number := 0.0;
		is_light_source: boolean := false;

		-- Calculated properties
		bounding_radius: number;
	end record;

	subtype Object is Class'class;

	procedure Init(self: in out Class);

	function Set_Location(self: access Class; location: vec3_t)
		return access Object;
	function Set_Radius(self: access Class; radius: number)
		return access Object;

end;
