with Ada.Text_IO;
with Config;
with Matrices;
with Utils;

use Ada.Text_IO;
use Config;
use Matrices;
use Utils;

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
end;

