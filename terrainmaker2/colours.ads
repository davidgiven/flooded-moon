with Config;
with ConfigFiles;

use Config;
use Config.NumberFunctions;
use ConfigFiles;

package Colours is
	type ElementArray is array(0..3) of number;

	type Colour is record
		r: Number := 0.0;
		g: Number := 0.0;
		b: Number := 0.0;
	end record;
	pragma Convention(C, Colour);
	for Colour'alignment use (4*8);

	Black: constant Colour := (0.0, 0.0, 0.0);
	White: constant Colour := (1.0, 1.0, 1.0);

	function RGB(r, g, b: Number) return Colour is
		(r, g, b);
	function Mix(c1, c2: Colour; a: Number) return Colour;
	function Mix(c1, c2: Colour; a: Colour) return Colour is
		(
			c1.r*(1.0-a.r) + c2.r*a.r,
			c1.g*(1.0-a.g) + c2.g*a.g,
			c1.b*(1.0-a.b) + c2.b*a.b
		);

	function Length2(c: Colour) return number is
		(c.r**2 + c.g**2 + c.b**2);

	function "*" (c: Colour; n: Number) return Colour is
		(c.r*n, c.g*n, c.b*n);
	function "*" (n: number; c: Colour) return Colour is
		(n*c.r, n*c.g, n*c.b);
	function "*" (c1, c2: Colour) return Colour is
		(c1.r*c2.r, c1.g*c2.g, c1.b*c2.b);
	function "/" (c: Colour; n: Number) return Colour is
		(c.r/n, c.g/n, c.b/n);
	function "/" (n: Number; c: Colour) return Colour is
		(n/c.r, n/c.g, n/c.b);
	function "+" (c1, c2: Colour) return Colour is
		(c1.r+c2.r, c1.g+c2.g, c1.b+c2.b);
	function "-" (c1, c2: Colour) return Colour is
		(c1.r-c2.r, c1.g-c2.g, c1.b-c2.b);
	function "-" (n: Number; c: Colour) return Colour is
		(n-c.r, n-c.g, n-c.b);
	function "-" (c: Colour) return Colour is
		(-c.r, -c.g, -c.b);

	function exp(c: Colour) return Colour is
		(exp(c.r), exp(c.g), exp(c.b));

	function ToString(c: Colour) return string is
		("(" & c.r'img & "," & c.g'img & "," & c.b'img & ")");

	function Load(cf: ConfigFile) return Colour;
end;

