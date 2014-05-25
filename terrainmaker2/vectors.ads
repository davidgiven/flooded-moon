with Config;
with GenericVector;
with ConfigFiles;

use Config;
use ConfigFiles;

package Vectors is
	type Vector2Index is range 0..1;
	package Vectors2 is new GenericVector(Vector2Index);
	type Vector2 is new Vectors2.Vector;

	type Vector3Index is range 0..2;
	package Vectors3 is new GenericVector(Vector3Index);
	type Vector3 is new Vectors3.Vector;

	X: constant Vector3 := (1.0, 0.0, 0.0);
	Y: constant Vector3 := (0.0, 1.0, 0.0);
	Z: constant Vector3 := (0.0, 0.0, 1.0);
	Zero: constant Vector3 := (0.0, 0.0, 0.0);

	type Vector4Index is range 0..3;
	package Vectors4 is new GenericVector(Vector4Index);
	type Vector4 is new Vectors4.Vector;

	subtype Point is Vector3;

	function Cross(v1, v2: Vector3) return Vector3 is
		(Vector3'(
			v1(1)*v2(2) - v1(2)*v2(1),
			v1(2)*v2(1) - v1(0)*v2(2),
			v1(0)*v2(1) - v1(1)*v2(0)));

	function Load(cf: ConfigFile) return Vector3;
end;
