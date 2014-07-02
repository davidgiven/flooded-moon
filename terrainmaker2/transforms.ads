with Config;
with Matrices;
with Vectors;
with ConfigFiles;

use Config;
use Config.Number_Functions;
use Matrices;
use Vectors;
use ConfigFiles;

package Transforms is
	type TransformMatrix is tagged
	record
		t: mat4_t := (others => (others => 0.0));
		it: mat4_t;
		itset: boolean := false;
	end record;

	function Transform(t: TransformMatrix; p: vec3_t) return vec3_t;
	function Untransform(t: in out TransformMatrix; p: vec3_t) return vec3_t;
	function Inverse(t: in out TransformMatrix) return mat4_t;

	procedure Set(t: in out TransformMatrix; m: mat4_t);
	procedure Reset(t: in out TransformMatrix);
	procedure LookAt(t: in out TransformMatrix; location, target: vec3_t;
			up: vec3_t);
	procedure Apply(t: in out TransformMatrix; o: mat4_t);
	procedure Translate(t: in out TransformMatrix; v: vec3_t);
	procedure Scale(t: in out TransformMatrix; v: vec3_t);
	procedure Scale(t: in out TransformMatrix; d: number);
	procedure Rotate(t: in out TransformMatrix; v: vec3_t; angle: number);

	function Load(cf: node_t) return TransformMatrix;
end;

