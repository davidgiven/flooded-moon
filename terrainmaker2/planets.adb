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
					"sunDirection: vector*3," &
					"sunColour: vector*3" &
				"): (" & 
					"extinction: vector*3," &
					"emission: vector*3" &
				")");
		end if;
	end;

	function TestIntersection(p: Planet; r: Ray; rayEntry, rayExit: in out Point)
			return boolean is
		tca, d2, thc: Number;
		radius2: Number := p.bounding_radius**2;
		L: Vector3 := p.location - r.location;
	begin
		tca := Dot(L, r.direction);
		if (tca < 0.0) then
			return false;
		end if;
		d2 := Dot(L, L) - tca**2;
		if (d2 > radius2) then
			return false;
		end if;

		thc := sqrt(radius2 - d2);
		rayEntry := r.location + r.direction*(tca-thc);
		rayExit := r.location + r.direction*(tca+thc);
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

	procedure SampleAtmosphere(p: Planet; xyz: Point; sunDir: Vector3;
			sunColour: Colour; extinction: out Colour; emission: out Colour) is
	begin
		p.atmosphere_func.Call.all(xyz, p.bounding_radius, p.nominal_radius,
				sunDir, sunColour,
				extinction, emission);
	end;
end;

