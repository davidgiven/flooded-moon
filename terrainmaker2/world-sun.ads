with Planet;
with Config;
with Vectors;

use type Planet.Class;
use Config;
use Vectors;

package World.Sun is
	subtype Super is Planet.Class;
	type Class is new Super with
	record
		null;
	end record;

	procedure Init(self: in out Class);

	function Create return access Class
		is (new Class);
end;

