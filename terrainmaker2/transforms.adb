with Config;

use Config;

package body Transforms is
	function MultiplyPoint(m: Matrix4; p: Point) return Point is
		x: Number := m(0,0)*p(0) + m(0,1)*p(1) + m(0,2)*p(2) + m(0,3);
		y: Number := m(1,0)*p(0) + m(1,1)*p(1) + m(1,2)*p(2) + m(1,3);
		z: Number := m(2,0)*p(0) + m(2,1)*p(1) + m(2,2)*p(2) + m(2,3);
		w: Number := m(3,0)*p(0) + m(3,1)*p(1) + m(3,2)*p(2) + m(3,3);
	begin
		return Point'(x/w, y/w, z/w);
	end;

	function Transform(t: TransformMatrix; p: Point) return Point is
	begin
		return MultiplyPoint(t.t, p);
	end;

	function Inverse(t: in out TransformMatrix) return Matrix4 is
	begin
		if t.itset then
			t.it := Invert(t.t);
			t.itset := true;
		end if;
		return t.it;
	end;

	function Untransform(t: in out TransformMatrix; p: Point) return Point is
	begin
		return MultiplyPoint(t.Inverse, p);
	end;

	procedure Set(t: in out TransformMatrix; m: Matrix4) is
	begin
		t.t := m;
		t.itset := false;
	end;

	procedure Reset(t: in out TransformMatrix) is
	begin
		for x in t.t'range(1) loop
			for y in t.t'range(2) loop
				t.t(x,y) := 0.0;
			end loop;
			t.t(x,x) := 1.0;
		end loop;
		t.itset := false;
	end;

	procedure LookAt(t: in out TransformMatrix; location, target: Point;
			up: Vector3) is
		dir: Vector3 := Normalise(target-location);
		left: Vector3 := Normalise(Cross(up, dir));
		newUp: Vector3 := Cross(dir, left);
	begin
		t.Set(
			Matrix4'(
				(left(0),     left(1),     left(2),     0.0),
				(newUp(0),    newUp(1),    newUp(2),    0.0),
				(dir(0),      dir(1),      dir(2),      0.0),
				(location(0), location(1), location(2), 1.0))
		);
	end;

	procedure Apply(t: in out TransformMatrix; o: Matrix4) is
	begin
		t.t := o*t.t;
		t.itset := false;
	end;

	procedure Translate(t: in out TransformMatrix; v: Vector3) is
	begin
		t.apply(
			Matrix4'(
				(1.0, 0.0, 0.0, v(0)),
				(0.0, 1.0, 0.0, v(1)),
				(0.0, 0.0, 1.0, v(2)),
				(0.0, 0.0, 0.0, 1.0))
		);
	end;

	procedure Scale(t: in out TransformMatrix; v: Vector3) is
	begin
		t.apply(
			Matrix4'(
				(v(0), 0.0,  0.0,  0.0),
				(0.0,  v(1), 0.0,  0.0),
				(0.0,  0.0,  v(2), 0.0),
				(0.0,  0.0,  0.0,  1.0))
			);
	end;

	procedure Scale(t: in out TransformMatrix; d: Number) is
	begin
		t.Scale(Vector3'(d, d, d));
	end;

	procedure Rotate(t: in out TransformMatrix; v: Vector3; angle: Number) is
		vn: Vector3 := Normalise(v);
		vnx: Number := vn(0);
		vny: Number := vn(1);
		vnz: Number := vn(2);
		angleRad: Number := degToRad(angle);
		sinTheta: Number := sin(angleRad);
		cosTheta: Number := cos(angleRad);
	begin
		t.apply(
			Matrix4'(
				(
					vnx * vnx + (1.0 - vnx * vnx) * cosTheta,
					vnx * vnx * (1.0 - cosTheta) - vnz * sinTheta,
					vnx * vnx * (1.0 - cosTheta) + vny * sinTheta,
					0.0
				), (
					vnx * vny * (1.0 - cosTheta) - vnz * sinTheta,
					vny * vny + (1.0 - vny * vny) * cosTheta,
					vny * vnz * (1.0 - cosTheta) + vnx * sinTheta,
					0.0
				), (
					vnx * vnz * (1.0 - cosTheta) - vny * sinTheta,
					vny * vnz * (1.0 - cosTheta) + vnx * sinTheta,
					vnz * vnz + (1.0 - vnz * vnz) * cosTheta,
					0.0
				), (
					0.0, 0.0, 0.0, 1.0
				))
			);
	end;

	function Load(cf: ConfigFile) return TransformMatrix is
	begin
		return t: TransformMatrix do
			t.Reset;

			if cf.Exists("latitude") then
				t.Rotate(Vectors.X, cf("latitude").Value);
			end if;
			if cf.Exists("longitude") then
				t.Rotate(Vectors.Z, cf("longitude").Value);
			end if;
			if cf.Exists("tilt") then
				t.Rotate(Vectors.X, cf("tilt").Value);
			end if;
			if cf.Exists("location") then
				t.Translate(Load(cf("location")));
			end if;
		end return;
	end;
end;

