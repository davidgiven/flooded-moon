with Vectors;
with Config;
with Planet;

use Vectors;
use Config;

package Atmospheric_Planet is
	subtype Super is Planet.Class;
	type Class is new Super with null record;

	subtype Object is Class'class;

	procedure Init(self: in out Class);

	function Set_Atmospheric_Depth(self: access Class; depth: number)
		return access Object;
end;

