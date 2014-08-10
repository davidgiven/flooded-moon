with Config;
with ConfigFiles;
with Utils;

use Config;
use ConfigFiles;
use Config.Number_Functions;
use Utils;

package Vectors is
	-- 4-vectors

	type vec4_t is
	record
		x, y, z, w: aliased number := 0.0;
	end record;
	pragma convention(C, vec4_t);
	for vec4_t'alignment use (4*8);

	function "*" (a, b: vec4_t) return vec4_t is
		(a.x*b.x, a.y*b.y, a.z*b.z, a.w*b.w);
	function "/" (a, b: vec4_t) return vec4_t is
		(a.x/b.x, a.y/b.y, a.z/b.z, a.w/b.w);
	function "+" (a, b: vec4_t) return vec4_t is
		(a.x+b.x, a.y+b.y, a.z+b.z, a.w+b.w);
	function "-" (a, b: vec4_t) return vec4_t is
		(a.x-b.x, a.y-b.y, a.z-b.z, a.w-b.w);
	
	function "*" (a: vec4_t; b: number) return vec4_t is
		(a.x*b, a.y*b, a.z*b, a.w*b);
	function "/" (a: vec4_t; b: number) return vec4_t is
		(a.x/b, a.y/b, a.z/b, a.w/b);
	function "+" (a: vec4_t; b: number) return vec4_t is
		(a.x+b, a.y+b, a.z+b, a.w+b);
	function "-" (a: vec4_t; b: number) return vec4_t is
		(a.x-b, a.y-b, a.z-b, a.w-b);

	function "*" (a: number; b: vec4_t) return vec4_t is
		(a*b.x, a*b.y, a*b.z, a*b.w);
	function "/" (a: number; b: vec4_t) return vec4_t is
		(a/b.x, a/b.y, a/b.z, a/b.w);
	function "+" (a: number; b: vec4_t) return vec4_t is
		(a+b.x, a+b.y, a+b.z, a+b.w);
	function "-" (a: number; b: vec4_t) return vec4_t is
		(a-b.x, a-b.y, a-b.z, a-b.w);

	function "-" (a: vec4_t) return vec4_t is
		(-a.x, -a.y, -a.z, -a.w);

	function "abs"(a: vec4_t) return vec4_t is
		(abs(a.x), abs(a.y), abs(a.z), abs(a.w));
	function floor(a: vec4_t) return vec4_t is
		(floor(a.x), floor(a.y), floor(a.z), floor(a.w));
	function step(edge, a: vec4_t) return vec4_t is
		(step(edge.x, a.x), step(edge.y, a.y),
	     step(edge.z, a.z), step(edge.w, a.w));
	function max(a, b: vec4_t) return vec4_t is
		(max(a.x, b.x), max(a.y, b.y), max(a.z, b.z), max(a.w, b.w));
	function max(a: vec4_t; b: number) return vec4_t is
		(max(a.x, b), max(a.y, b), max(a.z, b), max(a.w, b));
	function dot(a, b: vec4_t) return number is
		(a.x*b.x + a.y*b.y + a.z*b.z + a.w*b.w);

	-- 3-vectors
	
	type vec3_t is
	record
		x, y, z: aliased number := 0.0;
	end record;
	pragma convention(C, vec3_t);
	for vec3_t'alignment use (4*8);

	function X return vec3_t is (1.0, 0.0, 0.0);
	function Y return vec3_t is (0.0, 1.0, 0.0);
	function Z return vec3_t is (0.0, 0.0, 1.0);
	function Zero return vec3_t is (0.0, 0.0, 0.0);

	function "*" (a, b: vec3_t) return vec3_t is
		(a.x*b.x, a.y*b.y, a.z*b.z);
	function "/" (a, b: vec3_t) return vec3_t is
		(a.x/b.x, a.y/b.y, a.z/b.z);
	function "+" (a, b: vec3_t) return vec3_t is
		(a.x+b.x, a.y+b.y, a.z+b.z);
	function "-" (a, b: vec3_t) return vec3_t is
		(a.x-b.x, a.y-b.y, a.z-b.z);
	
	function "*" (a: vec3_t; b: number) return vec3_t is
		(a.x*b, a.y*b, a.z*b);
	function "/" (a: vec3_t; b: number) return vec3_t is
		(a.x/b, a.y/b, a.z/b);
	function "+" (a: vec3_t; b: number) return vec3_t is
		(a.x+b, a.y+b, a.z+b);
	function "-" (a: vec3_t; b: number) return vec3_t is
		(a.x-b, a.y-b, a.z-b);

	function "*" (a: number; b: vec3_t) return vec3_t is
		(a*b.x, a*b.y, a*b.z);
	function "/" (a: number; b: vec3_t) return vec3_t is
		(a/b.x, a/b.y, a/b.z);
	function "+" (a: number; b: vec3_t) return vec3_t is
		(a+b.x, a+b.y, a+b.z);
	function "-" (a: number; b: vec3_t) return vec3_t is
		(a-b.x, a-b.y, a-b.z);

	function "-" (a: vec3_t) return vec3_t is
		(-a.x, -a.y, -a.z);

	function Length2(a: vec3_t) return number is
		(a.x*a.x + a.y*a.y + a.z*a.z);
	function Length(a: vec3_t) return number is
		(sqrt(Length2(a)));
	function Normalise(a: vec3_t) return vec3_t is
		(a / Length(a));
	function NormaliseToSphere(v: vec3_t; r: number) return vec3_t is
		(Normalise(v) * r);
	function Dot(a, b: vec3_t) return number is
		(a.x*b.x + a.y*b.y + a.z*b.z);
	function Dot(a: vec3_t; b: number) return number is
		(a.x*b + a.y*b + a.z*b);
	function Cross(a, b: vec3_t) return vec3_t is
		(a.y*b.z - a.y*b.y,
		 a.z*b.y - a.x*b.z,
		 a.x*b.y - a.y*b.x);

	function Load(cf: node_t) return vec3_t;

	function sqrt(a: vec3_t) return vec3_t is
		(sqrt(a.x), sqrt(a.y), sqrt(a.z));
	function exp(a: vec3_t) return vec3_t is
		(exp(a.x), exp(a.y), exp(a.z));
	function floor(a: vec3_t) return vec3_t is
		(floor(a.x), floor(a.y), floor(a.z));
	function step(edge, a: vec3_t) return vec3_t is
		(step(edge.x, a.x), step(edge.y, a.y), step(edge.z, a.z));
	function min(a, b: vec3_t) return vec3_t is
		(min(a.x, b.x), min(a.y, b.y), min(a.z, b.z));
	function max(a, b: vec3_t) return vec3_t is
		(max(a.x, b.x), max(a.y, b.y), max(a.z, b.z));
	function max(a: vec3_t; b: number) return vec3_t is
		(max(a.x, b), max(a.y, b), max(a.z, b));

	function ToString(v: vec3_t) return string is
		("(" & v.x & "," & v.y & "," & v.z & ")");
	function "&" (s: string; v: vec3_t) return string is
		(s & ToString(v));

	-- 2-vectors

	type vec2_t is
	record
		x, y: aliased number := 0.0;
	end record;
	pragma convention(C, vec2_t);
	for vec2_t'alignment use (2*8);

	function floor(a: vec2_t) return vec2_t is
		(floor(a.x), floor(a.y));
end;

