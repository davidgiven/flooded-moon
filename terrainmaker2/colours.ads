with Config;
with ConfigFiles;

use Config;
use Config.Number_Functions;
use ConfigFiles;

package Colours is
	type ElementArray is array(0..3) of number;

	type colour_t is record
		r: number := 0.0;
		g: number := 0.0;
		b: number := 0.0;
	end record;
	pragma Convention(C, colour_t);
	for colour_t'alignment use (4*8);

	Black: constant colour_t := (0.0, 0.0, 0.0);
	White: constant colour_t := (1.0, 1.0, 1.0);

	function RGB(r, g, b: number) return colour_t is
		(r, g, b);
	function Mix(c1, c2: colour_t; a: number) return colour_t;
	function Mix(c1, c2: colour_t; a: colour_t) return colour_t is
		(
			c1.r*(1.0-a.r) + c2.r*a.r,
			c1.g*(1.0-a.g) + c2.g*a.g,
			c1.b*(1.0-a.b) + c2.b*a.b
		);

	function Length2(c: colour_t) return number is
		(c.r**2 + c.g**2 + c.b**2);

	function "*" (c: colour_t; n: number) return colour_t is
		(c.r*n, c.g*n, c.b*n);
	function "*" (n: number; c: colour_t) return colour_t is
		(n*c.r, n*c.g, n*c.b);
	function "*" (c1, c2: colour_t) return colour_t is
		(c1.r*c2.r, c1.g*c2.g, c1.b*c2.b);
	function "/" (c: colour_t; n: number) return colour_t is
		(c.r/n, c.g/n, c.b/n);
	function "/" (n: number; c: colour_t) return colour_t is
		(n/c.r, n/c.g, n/c.b);
	function "+" (c1, c2: colour_t) return colour_t is
		(c1.r+c2.r, c1.g+c2.g, c1.b+c2.b);
	function "-" (c1, c2: colour_t) return colour_t is
		(c1.r-c2.r, c1.g-c2.g, c1.b-c2.b);
	function "-" (n: number; c: colour_t) return colour_t is
		(n-c.r, n-c.g, n-c.b);
	function "-" (c: colour_t) return colour_t is
		(-c.r, -c.g, -c.b);

	function exp(c: colour_t) return colour_t is
		(exp(c.r), exp(c.g), exp(c.b));

	function ToString(c: colour_t) return string is
		("(" & c.r'img & "," & c.g'img & "," & c.b'img & ")");

	function Load(cf: node_t) return colour_t;
end;

