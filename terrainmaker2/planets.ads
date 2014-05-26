with Ada.Containers.Indefinite_Vectors;
with Config;
with Matrices;
with ConfigFiles;
with GenericLists;
with Transforms;
with Scene;
with Vectors;
with Calculon;

use Config;
use Config.NumberFunctions;
use Matrices;
use ConfigFiles;
use Transforms;
use Scene;
use Vectors;

package Planets is
	type TerrainFunc is access procedure(r: out Number);
	package TerrainCalculon is new Calculon(TerrainFunc);

	type Planet is tagged limited record
		cf: ConfigFile;
		location: Point;
		nominal_radius: Number;
		atmospheric_depth: Number;
		terrain: TerrainCalculon.Func;

		bounding_radius: Number;
		transform: TransformMatrix;
	end record;

	procedure Init(p: in out Planet; cf: ConfigFile);
	function TestIntersection(p: Planet; r: Ray;
			rayEntry, rayExit: in out Point)
			return boolean;

	package Lists is new GenericLists(Planet);
	subtype List is Lists.List;
end;


