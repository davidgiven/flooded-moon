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
	type terrain_radius_f is access procedure(
			xyz: Point;
			bounding_radius: number;
			nominal_radius: number;
			radius: out number
		);
	pragma Convention(C, terrain_radius_f);
	package terrain_radius_c_f is new Calculon(terrain_radius_f);

	type atmosphere_media_f is access procedure(
			xyz: Point;
			bounding_radius: number;
			nominal_radius: number;
			camera_direction: vector3;
			sun_direction: vector3;
			sun_colour: colour_t;
			extinction: in out colour_t;
			emission: in out colour_t
		);
	pragma Convention(C, atmosphere_media_f);
	package atmosphere_media_c_t is new Calculon(atmosphere_media_f);

	type Planet is tagged limited record
		cf: node_t;
		location: Point;
		nominal_radius: number;
		atmospheric_depth: number;
		terrain_radius_func: terrain_radius_c_f.Func;
		atmosphere_func: atmosphere_media_c_t.Func;

		bounding_radius: number;
		transform: TransformMatrix;
	end record;

	procedure Init(p: in out Planet; cf: node_t);
	-- These take WORLD coordinates.
	function Test_Intersection(p: Planet; r: Ray;
			ray_entry, ray_exit: in out Point;
			include_atmosphere: boolean := true)
			return boolean;

	-- These take PLANET coordinates.
	function Get_Actual_Radius(p: Planet; xyz: Point)
			return number;
	function Is_Point_Underground(p: Planet; xyz: Point)
			return boolean;
	procedure Sample_Atmosphere(p: Planet; xyz: Point;
			camera_direction, sun_direction: Vector3;
			sun_colour: colour_t;
			extinction, emission: out colour_t);

	package Lists is new GenericLists(Planet);
	subtype List is Lists.List;
end;


