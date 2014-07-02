with Ada.Numerics.Generic_Real_Arrays;
with Config;
with Vectors;
with GenericMatrix;

use Config;
use Vectors;

package Matrices is
	package Matrices2 is new GenericMatrix(Vectors.Vectors2.Index, Vector2);
	type Matrix2 is new Matrices2.Matrix;

	package Matrices3 is new GenericMatrix(Vectors.Vectors3.Index, Vector3);
	type Matrix3 is new Matrices3.Matrix;

	package Matrices4 is new GenericMatrix(Vectors.Vectors4.Index, Vector4);
	type Matrix4 is new Matrices4.Matrix;

	-- x, y
	subtype mat4_index_t is natural range 0..3;
	type mat4_t is array(mat4_index_t, mat4_index_t) of number;
	-- Ensure column-major order, not row-major order.
	pragma Convention(Fortran, mat4_t);
	for mat4_t'alignment use (16*8);

	function zero return mat4_t is
		(others => (others => 0.0));
	function identity return mat4_t is
		((1.0, 0.0, 0.0, 0.0),
		 (0.0, 1.0, 0.0, 0.0),
		 (0.0, 0.0, 1.0, 0.0),
		 (0.0, 0.0, 0.0, 1.0));

	function "*" (m: mat4_t; n: number) return mat4_t;
	function "*" (m: mat4_t; v: vec3_t) return vec3_t;
	function "*" (m: mat4_t; o: mat4_t) return mat4_t;

	function "/" (m: mat4_t; n: number) return mat4_t is
		(m * (1.0 / n));

	function Invert(m: mat4_t) return mat4_t;
	function Transpose(m: mat4_t) return mat4_t;
	function ToString(m: mat4_t) return string;
	function "&" (s: string; m: mat4_t) return string is
		(s & ToString(m));
end;
