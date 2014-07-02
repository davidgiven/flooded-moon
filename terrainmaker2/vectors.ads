with Config;
with GenericVector;
with ConfigFiles;

use Config;
use ConfigFiles;
use Config.Number_Functions;

package Vectors is
	type Vector2Index is range 0..1;
	package Vectors2 is new GenericVector(Vector2Index);
	type Vector2 is new Vectors2.Vector;
	for Vector2'alignment use (2*8);

	type Vector3Index is range 0..2;
	package Vectors3 is new GenericVector(Vector3Index);
	type Vector3 is new Vectors3.Vector;
	for Vector3'alignment use (4*8);

	function X return Vector3 is (1.0, 0.0, 0.0);
	function Y return Vector3 is (0.0, 1.0, 0.0);
	function Z return Vector3 is (0.0, 0.0, 1.0);
	function Zero return Vector3 is (0.0, 0.0, 0.0);

	type Vector4Index is range 0..3;
	package Vectors4 is new GenericVector(Vector4Index);
	type Vector4 is new Vectors4.Vector;
	for Vector4'alignment use (4*8);

	subtype Point is Vector3;

	function Cross(v1, v2: Vector3) return Vector3 is
		(Vector3'(
			v1(1)*v2(2) - v1(2)*v2(1),
			v1(2)*v2(1) - v1(0)*v2(2),
			v1(0)*v2(1) - v1(1)*v2(0)));

	function NormaliseToSphere(v: Vector3; r: number) return Vector3 is
		(Normalise(v) * r);

	function Load(cf: node_t) return Vector3;
	
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
	function Cross(a, b: vec3_t) return vec3_t is
		(a.y*b.z - a.y*b.y,
		 a.z*b.y - a.x*b.z,
		 a.x*b.y - a.y*b.x);

	function sqrt(a: vec3_t) return vec3_t is
		(sqrt(a.x), sqrt(a.y), sqrt(a.z));
	function exp(a: vec3_t) return vec3_t is
		(exp(a.x), exp(a.y), exp(a.z));

	function ToString(v: vec3_t) return string is
		("(" & v.x & "," & v.y & "," & v.z & ")");
	function "&" (s: string; v: vec3_t) return string is
		(s & ToString(v));
end;

