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
			xyz: vec3_t;
			bounding_radius: number;
			nominal_radius: number;
			radius: out number
		);
	pragma Convention(C, terrain_radius_f);
	package terrain_radius_c_f is
			new Calculon.Script(terrain_radius_f);

	type terrain_surface_f is access procedure(
			xyz: vec3_t;
			nominal_radius: number;
			camera_direction: vec3_t;
			sun_direction: vec3_t;
			sun_colour: colour_t;
			ambient_colour: colour_t;
			emission: out colour_t
		);
	pragma Convention(C, terrain_surface_f);
	package terrain_surface_c_f is
			new Calculon.Script(terrain_surface_f);

	type atmosphere_media_f is access procedure(
			xyz: vec3_t;
			bounding_radius: number;
			nominal_radius: number;
			camera_direction: vec3_t;
			sun_direction: vec3_t;
			sun_colour: colour_t;
			extinction: in out colour_t;
			emission: in out colour_t
		);
	pragma Convention(C, atmosphere_media_f);
	package atmosphere_media_c_t is
			new Calculon.Script(atmosphere_media_f);

	type planet_t is tagged limited record
		cf: node_t;
		location: vec3_t;
		nominal_radius: number;
		atmospheric_depth: number;
		terrain_radius_func: terrain_radius_c_f.Func;
		terrain_surface_func: terrain_surface_c_f.Func;
		atmosphere_func: atmosphere_media_c_t.Func;

		bounding_radius: number;
		transform: TransformMatrix;
	end record;

	procedure Init(p: in out planet_t; cf: node_t);
	-- These take WORLD coordinates.
	function Test_Intersection(p: planet_t; r: ray_t;
			ray_entry, ray_exit: in out vec3_t;
			include_atmosphere: boolean := true)
			return boolean;

	-- These take planet_t coordinates.
	function Get_Actual_Radius(p: planet_t; xyz: vec3_t)
			return number;
	function Is_Point_Underground(p: planet_t; xyz: vec3_t)
			return boolean;
	procedure Sample_Atmosphere(p: planet_t; xyz: vec3_t;
			camera_direction, sun_direction: vec3_t;
			sun_colour: colour_t;
			extinction, emission: out colour_t);
	procedure Sample_Surface(p: planet_t; xyz: vec3_t;
			camera_direction, sun_direction: vec3_t;
			sun_colour: colour_t;
			ambient_colour: colour_t;
			emission: out colour_t);

	package Lists is new GenericLists(planet_t);
	subtype planet_list_t is Lists.List;
end;


