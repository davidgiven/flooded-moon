with Ada.Containers.Vectors;
with Config;
with Matrices;
with ConfigFiles;

use Config;
use Matrices;
use ConfigFiles;

package Planets is
	type Planet is record
		cf: ConfigFile;
		nominal_radius: Number;
		atmospheric_depth: Number;

		bounding_radius: Number;
		transform: Matrix4;
	end record;

	function Create(cf: ConfigFile) return Planet;

	package Container is new Ada.Containers.Vectors(Natural, Planet);
	type Vector is new Container.Vector with null record;
end;


