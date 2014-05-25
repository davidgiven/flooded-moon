with Config;
with Images;
with Vectors;

use Config;
use Images;
use Vectors;

package Renderer is
	type Ray is record
		location: Point;
		direction: Vector3;
	end record;

	function Render(width, height: integer) return Image;
end;

