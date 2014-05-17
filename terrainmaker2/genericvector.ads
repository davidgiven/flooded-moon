with Config;
use Config;

generic
	size: natural;
package GenericVector is
	type Vector is array(0..(size-1)) of Number;

	function "+" (v1, v2: Vector) return Vector;
	function "*" (v: Vector; n: Number) return Vector;
end;

