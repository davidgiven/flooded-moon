with Config;

use Config;

package Colours is
	type Colour is record
		r, g, b: Number;
	end record;

	function RGB(r, g, b: Number) return Colour is
		(r, g, b);
end;

