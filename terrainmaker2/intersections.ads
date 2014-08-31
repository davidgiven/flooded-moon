with Vectors;
with Rays;
with Planet;
with Config;

use Vectors;
use Rays;
use Config;

package Intersections is
	type intersection_t is record
		planet: access Planet.Object;
		ray_entry: vec3_t;
		ray_exit: vec3_t;
	end record;
	type intersection_list_t is
		array(natural range 0..Max_Objects) of intersection_t;
end;

