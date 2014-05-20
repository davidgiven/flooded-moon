with Ada.Text_IO;
with Config;
with Matrices;

use Ada.Text_IO;
use Config;
use Matrices;

package body Planets is
	function Create(cf: ConfigFile) return Planet is
		p: Planet;
	begin
		Put_Line("Loading planet: " & cf.Name);
		p.cf := cf;
		p.nominal_radius := cf("nominal_radius").Value;
		p.atmospheric_depth := cf("atmospheric_depth").Value;

		p.bounding_radius := p.nominal_radius + p.atmospheric_depth;
		return p;
	end;
end;
