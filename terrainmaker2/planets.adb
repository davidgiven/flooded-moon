with Ada.Text_IO;
with Config;
with Matrices;
with Utils;
with BigFiles;

use Ada.Text_IO;
use Config;
use Matrices;
use Utils;
use BigFiles;

package body Planets is
	procedure Init(p: in out Planet; cf: ConfigFile) is
	begin
		Put_Line("Loading planet: " & cf.Name);
		p.cf := cf;
		p.nominal_radius := cf("nominal_radius").Value;
		p.atmospheric_depth := cf("atmospheric_depth").Value;
		p.location := Load(cf("location"));

		p.bounding_radius := p.nominal_radius + p.atmospheric_depth;
		if cf.Exists("transform") then
			p.transform := Load(cf("transform"));
		else
			p.transform.Reset;
		end if;

		p.terrain_radius_func.InitialiseFromFile(
			cf("terrain_radius_func").Value,
			"(" &
				"xyz: vector*3," & 
				"boundingRadius: real," &
				"nominalRadius: real" &
			"): (" & 
				"radius: real" &
			")");

		if (p.atmospheric_depth > 0.0) then
			p.atmosphere_func.InitialiseFromFile(
				cf("atmosphere_func").Value,
				"(" &
					"xyz: vector*3," & 
					"boundingRadius: real," &
					"nominalRadius: real," &
					"cameraDirection: vector*3," &
					"sunDirection: vector*3," &
					"sunColour: vector*3" &
				"): (" & 
					"extinction: vector*3," &
					"emission: vector*3," &
					"density: real" &
				")");
		end if;
	end;

	function TestIntersection(p: Planet;
				r: Ray; rayEntry, rayExit: in out Point;
				Clip_Against_Atmosphere: boolean := true)
			return boolean is
		radius: Number;
		ro: Vector3;
		a, b, c, q: Number;
		disc2, disc: Number;
		t0, t1: Number;

		procedure Swap(a, b: in out Number) is
			t: Number;
		begin
			t := a;
			a := b;
			b := t;
		end;
	begin
		if Clip_Against_Atmosphere then
			radius := p.bounding_radius;
		else
			radius := p.nominal_radius;
		end if;

		ro := r.location - p.location;
		a := Dot(r.direction, r.direction);
		b := 2.0 * Dot(r.direction, ro);
		c := Dot(ro, ro) - (radius*radius);
		disc2 := b*b - 4.0*a*c;

		if (disc2 < 0.0) then
			-- Ray does not intersect sphere at all.
			return false;
		end if;

		disc := sqrt(disc2);
		if (b < 0.0) then
			q := (-b - disc)/2.0;
		else
			q := (-b + disc)/2.0;
		end if;

		t0 := q / a;
		t1 := c / q;
		if (t0 > t1) then
			Swap(t0, t1);
		end if;

		if (t1 < 0.0) then
			-- The sphere is completely behind the ray.
			return false;
		end if;
		if (t0 < 1.0) then
			-- Ray start appears to be inside the sphere.
			t0 := 1.0;
		end if;

		rayEntry := r.location + r.direction*t0;
		rayExit := r.location + r.direction*t1;
		return true;
	end;

	function GetActualRadius(p: Planet; xyz: Point) return number is
		normalised_xyz: Point := NormaliseToSphere(xyz, p.nominal_radius);
		radius: number;
	begin
		p.terrain_radius_func.Call.all(normalised_xyz, p.bounding_radius,
			p.nominal_radius, radius);
		return radius;
	end;

	function IsPointUnderground(p: Planet; xyz: Point) return boolean is
		xyzr: number := Length(xyz);
		realr: number := p.GetActualRadius(xyz);
	begin
		return xyzr < realr;
	end;

	procedure SampleAtmosphere(p: Planet; xyz: Point;
			cameraDirection, sunDirection: Vector3;
			sunColour: Colour;
			extinction, emission: out Colour;
			density: out number) is
	begin
		p.atmosphere_func.Call.all(xyz, p.bounding_radius, p.nominal_radius,
				cameraDirection, sunDirection, sunColour,
				extinction, emission, density);
	end;
end;

