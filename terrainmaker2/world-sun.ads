with Sun_Planet;
with Config;
with Vectors;
with Colours;

use Config;
use Vectors;
use Colours;

package World.Sun is
	subtype Super is Sun_Planet.Class;
	type Class is new Super with
	record
		null;
	end record;

	subtype Object is Class'class;

	procedure Init(self: in out Class);

	procedure Sample_Surface_P(self: Class;
			xyz_rel_planet: vec3_t;
			camera_direction, sun_direction, surface_normal: vec3_t;
			sun_colour: colour_t;
			ambient_colour: colour_t;
			emission: out colour_t);

	function Create return access Class
		is (new Class);
end;

