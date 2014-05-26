with Vectors;
with Images;
with Config;

use Vectors;
use Images;
use Config;

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
	function ComputePrimaryRay(x, y: integer; img: Image) return Ray;
	procedure ComputeObjectIntersections(r: Ray;
		ints: in out Intersections; num: in out natural);
end;

