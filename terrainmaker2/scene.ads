with Vectors;
with Images;
with Config;
with Colours;
with Rays;
with Intersections;

use Vectors;
use Images;
use Config;
use Colours;
use Rays;
use Intersections;

package Scene is
	procedure Load(filename: string);
	function Compute_Primary_Ray(x, y: integer; img: image_t) return ray_t;
	procedure Compute_Object_Intersections(r: ray_t;
		ints: out intersection_list_t; num: out natural;
		include_atmosphere: boolean := true);
	function Compute_Pixel_Colour(ray: ray_t) return colour_t;

	function Get_Camera_Location return vec3_t;
	function Get_Field_Of_View return vec2_t;

	camera_location: vec3_t;
end;

