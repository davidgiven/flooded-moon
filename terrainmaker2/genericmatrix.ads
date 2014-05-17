with Config;

use Config;

generic
	size: natural;
package GenericMatrix is
	-- x, y
	type Matrix is array(0..(size-1), 0..(size-1)) of Number;
	-- Ensure column-major order, not row-major order.
	pragma Convention(Fortran, Matrix);

	function Zero return Matrix;
	function Identity return Matrix;

	function Invert(m: Matrix) return Matrix;
	function ToString(m: Matrix) return string;

end;



