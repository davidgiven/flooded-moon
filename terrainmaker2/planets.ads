with Ada.Containers.Indefinite_Vectors;
with Config;
with Matrices;
with ConfigFiles;
with GenericLists;
with Transforms;
with Scene;
with Vectors;
with Calculon;
with Colours;

use Config;
use Config.Number_Functions;
use Matrices;
use ConfigFiles;
use Transforms;
use Scene;
use Vectors;
use Colours;

package Planets is
	type TerrainRadiusFunc is access procedure(
			xyz: Point;
			boundingRadius: number;
			nominalRadius: number;
			radius: out number
		);
	pragma Convention(C, TerrainRadiusFunc);
	package TerrainRadiusCalculon is new Calculon(TerrainRadiusFunc);

	type AtmosphereFunc is access procedure(
			xyz: Point;
			boundingRadius: number;
			nominalRadius: number;
			cameraDirection: vector3;
			sunDirection: vector3;
			sunColour: colour_t;
			extinction: in out colour_t;
			emission: in out colour_t
		);
	pragma Convention(C, AtmosphereFunc);
	package AtmosphereCalculon is new Calculon(AtmosphereFunc);

	type Planet is tagged limited record
		cf: node_t;
		location: Point;
		nominal_radius: number;
		atmospheric_depth: number;
		terrain_radius_func: TerrainRadiusCalculon.Func;
		atmosphere_func: AtmosphereCalculon.Func;

		bounding_radius: number;
		transform: TransformMatrix;
	end record;

	procedure Init(p: in out Planet; cf: node_t);
	-- These take WORLD coordinates.
	function Test_Intersection(p: Planet; r: Ray;
			rayEntry, rayExit: in out Point;
			Clip_Against_Atmosphere: boolean := true)
			return boolean;

	-- These take PLANET coordinates.
	function Get_Actual_Radius(p: Planet; xyz: Point)
			return number;
	function Is_Point_Underground(p: Planet; xyz: Point)
			return boolean;
	procedure Sample_Atmosphere(p: Planet; xyz: Point;
			cameraDirection, sunDirection: Vector3;
			sunColour: colour_t;
			extinction, emission: out colour_t);

	package Lists is new GenericLists(Planet);
	subtype List is Lists.List;
end;


