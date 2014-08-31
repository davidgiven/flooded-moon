with Vectors;
with Config;

use Vectors;
use Config;

package Rays is
	type ray_t is record
		location: vec3_t;
		direction: vec3_t;
	end record;
end;

