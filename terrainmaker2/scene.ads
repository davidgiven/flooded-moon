with Vectors;
with Images;

use Vectors;
use Images;

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
	type Intersections is array(natural range <>) of Intersection;

	procedure Load(filename: string);
	function ComputePrimaryRay(x, y: integer; img: Image) return Ray;
	function ComputeObjectIntersections(r: Ray) return Intersections;
end;

