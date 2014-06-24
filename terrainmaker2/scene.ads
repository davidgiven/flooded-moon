with Vectors;
with Images;
with Config;
with Colours;

use Vectors;
use Images;
use Config;
use Colours;

package Scene is
	type Ray is record
		location: Point;
		direction: Vector3;
	end record;

	type Intersection is record
		planet: integer;
		rayEntry: Point;
		rayExit: Point;
	end record;
	type Intersections is array(natural range 0..MaxObjects) of Intersection;

	procedure Load(filename: string);
	function Compute_Primary_Ray(x, y: integer; img: image_t) return Ray;
	procedure Compute_Object_Intersections(r: Ray;
		ints: out Intersections; num: out natural;
		Clip_Against_Atmosphere: boolean := true);
	function Compute_Pixel_Colour(r: Ray) return colour_t;
end;

