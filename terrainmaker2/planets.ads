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
use Config.NumberFunctions;
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
			sunColour: Colour;
			kappa: in out Colour;
			extinction: in out Colour;
			emission: in out Colour
		);
	pragma Convention(C, AtmosphereFunc);
	package AtmosphereCalculon is new Calculon(AtmosphereFunc);

	type Planet is tagged limited record
		cf: ConfigFile;
		location: Point;
		nominal_radius: Number;
		atmospheric_depth: Number;
		terrain_radius_func: TerrainRadiusCalculon.Func;
		atmosphere_func: AtmosphereCalculon.Func;

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
	procedure SampleAtmosphere(p: Planet; xyz: Point;
			cameraDirection, sunDirection: Vector3;
			sunColour: Colour;
			kappa: out Colour; extinction: out Colour; emission: out Colour);

	package Lists is new GenericLists(Planet);
	subtype List is Lists.List;
end;


