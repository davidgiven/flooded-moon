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
	type TerrainRadiusFunc is access procedure(
			xyz: Point;
			boundingRadius: number;
			nominalRadius: number;
			radius: out number
		);
	pragma Convention(C, TerrainRadiusFunc);
	package TerrainRadiusCalculon is new Calculon(TerrainRadiusFunc);

	type Planet is tagged limited record
		cf: ConfigFile;
		location: Point;
		nominal_radius: Number;
		atmospheric_depth: Number;
		terrain_radius_func: TerrainRadiusCalculon.Func;

		bounding_radius: Number;
		transform: TransformMatrix;
	end record;

	procedure Init(p: in out Planet; cf: ConfigFile);
	-- These take WORLD coordinates.
	function TestIntersection(p: Planet; r: Ray;
			rayEntry, rayExit: in out Point)
			return boolean;

	-- These take PLANET coordinates.
	function GetActualRadius(p: Planet; xyz: Point)
			return number;
	function IsPointUnderground(p: Planet; xyz: Point)
			return boolean;

	package Lists is new GenericLists(Planet);
	subtype List is Lists.List;
end;


