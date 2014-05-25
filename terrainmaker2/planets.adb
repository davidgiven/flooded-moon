with Ada.Text_IO;
with Config;
with Matrices;

use Ada.Text_IO;
use Config;
use Matrices;

package body Planets is
	procedure Init(p: in out Planet; cf: ConfigFile) is
	begin
		Put_Line("Loading planet: " & cf.Name);
		p.cf := cf;
		p.nominal_radius := cf("nominal_radius").Value;
		p.atmospheric_depth := cf("atmospheric_depth").Value;

		p.bounding_radius := p.nominal_radius + p.atmospheric_depth;
		if cf.Exists("transform") then
			p.transform := Load(cf("transform"));
		else
			p.transform.Reset;
		end if;
	end;

	function TestIntersection(p: Planet; r: Ray; rayEntry, rayExit: in out Point)
			return boolean is
	begin
		return true;
	end;
end;

