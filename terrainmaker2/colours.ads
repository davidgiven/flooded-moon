with Config;
with ConfigFiles;
with Vectors;

use Config;
use Config.Number_Functions;
use ConfigFiles;
use Vectors;

package Colours is
	type ElementArray is array(0..3) of number;

	subtype colour_t is vec3_t;

	type Ref(e: access number) is null record
	with
		Implicit_Dereference => e;

	function Red(c: in out colour_t) return Ref is
		(Ref'(e => c.x'access));
	function Green(c: in out colour_t) return Ref is
		(Ref'(e => c.y'access));
	function Blue(c: in out colour_t) return Ref is
		(Ref'(e => c.z'access));
	
	function Black return colour_t is
		(0.0, 0.0, 0.0);
	function White return colour_t is
		(1.0, 1.0, 1.0);

	function Mix(c1, c2: colour_t; a: number) return colour_t is
		(c1*(1.0-a) + c2*a);
	function Mix(c1, c2: colour_t; a: colour_t) return colour_t is
		(c1*(1.0-a) + c2*a);

	function Load(cf: node_t) return colour_t;
end;

