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
end;
