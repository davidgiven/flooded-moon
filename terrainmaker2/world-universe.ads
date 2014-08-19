with Ada.Containers.Vectors;
with Planet;
with Config;

use Config;
use type Planet.Class;

package World.Universe is
	type Array_Of_Bodies is array(natural range <>) of access Planet.Object;

	Bodies: access Array_Of_Bodies;

	procedure Init;
end;

