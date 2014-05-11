with Config;
with Vectors;

use Config;
use Vectors;

package Matrices is
	type Matrix is array(integer range 0..3, integer range 0..3) of Number;

	Identity: constant Matrix := (
		(1.0, 0.0, 0.0, 0.0),
		(0.0, 1.0, 0.0, 0.0),
		(0.0, 0.0, 1.0, 0.0),
		(0.0, 0.0, 0.0, 1.0)
	);

	function "+"(left, right: Matrix) return Matrix;
	function "*"(left, right: Matrix) return Matrix;

	function ToString(m: Matrix) return string;
end;

