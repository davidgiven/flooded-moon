with Config;
with Vectors;

use Config;
use Vectors;

package Matrices is
	-- Column-major, so the order is x, y!
	type Matrix is array(integer range 0..3, integer range 0..3) of Number;
	-- Ensure column-major order, not row-major order.
	pragma Convention(Fortran, Matrix);

	Identity: constant Matrix := (
		(1.0, 0.0, 0.0, 0.0),
		(0.0, 1.0, 0.0, 0.0),
		(0.0, 0.0, 1.0, 0.0),
		(0.0, 0.0, 0.0, 1.0)
	);

	Zero: constant Matrix := (
		(0.0, 0.0, 0.0, 0.0),
		(0.0, 0.0, 0.0, 0.0),
		(0.0, 0.0, 0.0, 0.0),
		(0.0, 0.0, 0.0, 0.0)
	);

	function "+" (left, right: Matrix) return Matrix;
	function "-" (left, right: Matrix) return Matrix;
	function "*" (left, right: Matrix) return Matrix;

	function Transpose(m: Matrix) return Matrix;
	function invert(m: Matrix) return Matrix;

	function ToString(m: Matrix) return string;
end;

