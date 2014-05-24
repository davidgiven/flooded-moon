with Config;
use Config;

generic
	type IndexFormal is range <>;
package GenericVector is
	subtype Index is IndexFormal;
	type Vector is array(Index) of Number;

	Zero: constant Vector := (others => 0.0);

	function "+" (v1, v2: Vector) return Vector;
	function "-" (v1, v2: Vector) return Vector;
	function "*" (v: Vector; n: Number) return Vector;

	function Length(v: Vector) return Number;
	function Normalise(v: Vector) return Vector;
	function ToString(v: Vector) return string;
end;

