with Config;
with Matrices;
with Vectors;

use Config;
use Matrices;
use Vectors;
use Config.NumberFunctions;

package Transforms is
	type TransformMatrix is tagged
	record
		t: Matrix4;
		it: Matrix4;
		itset: boolean := false;
	end record;

	function Transform(t: TransformMatrix; p: Point) return Point;
	function Untransform(t: in out TransformMatrix; p: Point) return Point;
	function Inverse(t: in out TransformMatrix) return Matrix4;

	procedure Apply(t: in out TransformMatrix; o: Matrix4);
	procedure Translate(t: in out TransformMatrix; v: Vector3);
	procedure Scale(t: in out TransformMatrix; v: Vector3);
	procedure Scale(t: in out TransformMatrix; d: Number);
	procedure Rotate(t: in out TransformMatrix; v: Vector3; angle: Number);
	function LookAt(p, t: Point; up: Vector3) return TransformMatrix;
end;

