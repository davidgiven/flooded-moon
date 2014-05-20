with GenericVector;

package Vectors is
	package Vectors2 is new GenericVector(2);
	type Vector2 is new Vectors2.Vector;

	package Vectors3 is new GenericVector(3);
	type Vector3 is new Vectors3.Vector;

	package Vectors4 is new GenericVector(4);
	type Vector4 is new Vectors4.Vector;
end;
