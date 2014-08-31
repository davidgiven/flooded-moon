with Vectors;
with Config;
with Planet;
with Colours;

use Vectors;
use Config;
use Colours;

package Atmospheric_Planet is
	subtype Super is Planet.Class;
	type Class is abstract new Super with null record;

	subtype Object is Class'class;

	procedure Init(self: in out Class);

	-- Overridable.
	
	procedure Sample_Atmosphere_P(self: Class;
			xyz_r: vec3_t;
			camera_direction, sun_direction: vec3_t;
			sun_colour: colour_t;
			extinction, emission: out colour_t) is abstract;

	procedure Render_Atmosphere_For_Segment(object: Class;
			segment_start, segment_end: vec3_t;
			emission, transmittance: in out colour_t);

	-- Non-overridable.
	
	procedure Sample_Atmosphere(self: Object;
			xyz: vec3_t;
			camera_direction, sun_direction: vec3_t;
			sun_colour: colour_t;
			extinction, emission: out colour_t);
end;

