with Config;
with GenericVector;

use Config;

generic
	size: natural;
	with package VectorsN is new GenericVector(size);
package GenericMatrix is
	subtype Vector is VectorsN.Vector;

	-- x, y
	type Matrix is array(1..size, 1..size) of Number;
	-- Ensure column-major order, not row-major order.
	pragma Convention(Fortran, Matrix);

	Zero: constant Matrix := (others => (others => 0.0));
	function Identity return Matrix;

	function "*" (m: Matrix; n: Number) return Matrix;

	function Invert(m: Matrix) return Matrix;
	function ToString(m: Matrix) return string;

end;



