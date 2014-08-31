with Vectors;
with Config;
with Planet;
with Colours;

use Vectors;
use Config;
use Colours;

package Sun_Planet is
	subtype Super is Planet.Class;
	type Class is abstract new Super with record
		colour: colour_t;
	end record;

	subtype Object is Class'class;

	procedure Init(self: in out Class);
end;


