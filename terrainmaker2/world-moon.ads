with Atmospheric_Planet;
with Config;
with Vectors;

use type Atmospheric_Planet.Class;
use Config;
use Vectors;

package World.Moon is
	subtype Super is Atmospheric_Planet.Class;
	type Class is new Super with
	record
		null;
	end record;

	procedure Init(self: in out Class);

	function Create return access Class
		is (new Class);
end;


