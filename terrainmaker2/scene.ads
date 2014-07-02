with Vectors;
with Images;
with Config;
with Colours;

use Vectors;
use Images;
use Config;
use Colours;

package Scene is
	type ray_t is record
		location: vec3_t;
		direction: vec3_t;
	end record;

	type intersection_t is record
		planet_t: integer;
		ray_entry: vec3_t;
		ray_exit: vec3_t;
	end record;
	type Intersections is array(natural range 0..Max_Objects) of intersection_t;

	procedure Load(filename: string);
	function Compute_Primary_Ray(x, y: integer; img: image_t) return ray_t;
	procedure Compute_Object_Intersections(r: ray_t;
		ints: out Intersections; num: out natural;
		include_atmosphere: boolean := true);
	function Compute_Pixel_Colour(r: ray_t) return colour_t;
end;

