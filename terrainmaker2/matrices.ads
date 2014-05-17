with Ada.Numerics.Generic_Real_Arrays;
with Config;
with Vectors;
with GenericMatrix;

use Config;
use Vectors;

package Matrices is
	package Matrices2 is new GenericMatrix(2);
	use all type Matrices2.Matrix;
	subtype Matrix2 is Matrices2.Matrix;

	package Matrices3 is new GenericMatrix(3);
	use all type Matrices3.Matrix;
	subtype Matrix3 is Matrices3.Matrix;

	package Matrices4 is new GenericMatrix(4);
	use all type Matrices4.Matrix;
	subtype Matrix4 is Matrices4.Matrix;
end;
