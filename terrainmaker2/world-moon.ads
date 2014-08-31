with Atmospheric_Planet;
with Config;
with Vectors;
with Colours;

use type Atmospheric_Planet.Class;
use Config;
use Vectors;
use Colours;

package World.Moon is
	subtype Super is Atmospheric_Planet.Class;
	type Class is new Super with
	record
		null;
	end record;

	subtype Object is Class'class;

	procedure Init(self: in out Class);

	function Get_Sample_Distance_P(self: Class;
				xyz_rel_planet: vec3_t)
			return number;

	function Get_Actual_Radius_P(self: Class;
				xyz_rel_planet: vec3_t)
			return number;

	procedure Sample_Atmosphere_P(self: Class;
			xyz_rel_planet: vec3_t;
			camera_direction, sun_direction: vec3_t;
			sun_colour: colour_t;
			extinction, emission: out colour_t);

	procedure Sample_Surface_P(self: Class;
			xyz_rel_planet: vec3_t;
			camera_direction, sun_direction, surface_normal: vec3_t;
			sun_colour: colour_t;
			ambient_colour: colour_t;
			emission: out colour_t);

	function Create return access Class
		is (new Class);
end;


