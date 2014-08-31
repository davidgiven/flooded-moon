with Planet;
with Sun_Planet;
with Config;

use Config;
use type Planet.Class;

package World.Universe is
	type Planet_Ref is access all Planet.Object;
	type Planet_Array is array (natural range <>) of Planet_Ref;

	Bodies: access Planet_Array;
	Sun: access Sun_Planet.Object;

	procedure Init;
end;

