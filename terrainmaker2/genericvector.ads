with Config;
use Config;

generic
	size: natural;
package GenericVector is
	type Index is new positive range 1..size;
	type Vector is array(Index) of Number;

	Zero: constant Vector := (others => 0.0);

	function "+" (v1, v2: Vector) return Vector;
	function "*" (v: Vector; n: Number) return Vector;

	function ToString(v: Vector) return string;
end;

