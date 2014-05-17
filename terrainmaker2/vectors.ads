with GenericVector;

package Vectors is
	package Vectors2 is new GenericVector(2);
	use all type Vectors2.Vector;
	subtype Vector2 is Vectors2.Vector;

	package Vectors3 is new GenericVector(3);
	use all type Vectors3.Vector;
	subtype Vector3 is Vectors3.Vector;

	package Vectors4 is new GenericVector(4);
	use all type Vectors4.Vector;
	subtype Vector4 is Vectors4.Vector;
end;
