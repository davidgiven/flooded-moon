with Config;
with Matrices;
with Vectors;
with ConfigFiles;

use Config;
use Config.NumberFunctions;
use Matrices;
use Vectors;
use ConfigFiles;

package Transforms is
	type TransformMatrix is tagged
	record
		t: Matrix4 := (others => (others => 0.0));
		it: Matrix4;
		itset: boolean := false;
	end record;

	function Transform(t: TransformMatrix; p: Point) return Point;
	function Untransform(t: in out TransformMatrix; p: Point) return Point;
	function Inverse(t: in out TransformMatrix) return Matrix4;

	procedure Set(t: in out TransformMatrix; m: Matrix4);
	procedure Reset(t: in out TransformMatrix);
	procedure LookAt(t: in out TransformMatrix; location, target: Point;
			up: Vector3);
	procedure Apply(t: in out TransformMatrix; o: Matrix4);
	procedure Translate(t: in out TransformMatrix; v: Vector3);
	procedure Scale(t: in out TransformMatrix; v: Vector3);
	procedure Scale(t: in out TransformMatrix; d: Number);
	procedure Rotate(t: in out TransformMatrix; v: Vector3; angle: Number);

	function Load(cf: ConfigFile) return TransformMatrix;
end;

