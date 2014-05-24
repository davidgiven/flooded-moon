with Config;

use Config;

generic
	type Index is range <>;
	type Vector is array(Index) of Number;
package GenericMatrix is
	-- x, y
	type Matrix is array(Index, Index) of Number;
	-- Ensure column-major order, not row-major order.
	pragma Convention(Fortran, Matrix);

	Zero: constant Matrix := (others => (others => 0.0));
	function Identity return Matrix;

	function "*" (m: Matrix; n: Number) return Matrix;
	function "*" (m: Matrix; v: Vector) return Vector;
	function "*" (m: Matrix; o: Matrix) return Matrix;

	function Invert(m: Matrix) return Matrix;
	function ToString(m: Matrix) return string;
end;



