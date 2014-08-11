with Ada.Text_IO;
with Config;
with Matrices;
with Utils;
with BigFiles;

use Ada.Text_IO;
use Config;
use Matrices;
use Utils;
use BigFiles;

package body Planets is
	procedure Init(p: in out planet_t; cf: node_t) is
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

		p.terrain_radius_func.Initialise_From_File(
			cf("terrain_radius_func").Value,
			"(" &
				"xyz: vector*3," & 
				"bounding_radius: real," &
				"nominal_radius: real" &
			"): (" & 
				"radius: real" &
			")");

		p.terrain_surface_func.Initialise_From_File(
			cf("terrain_surface_func").Value,
			"(" &
				"xyz: vector*3," &
				"nominal_radius: real," &
				"camera_direction: vector*3," &
				"sun_direction: vector*3," &
				"surface_normal: vector*3," &
				"sun_colour: vector*3," &
				"ambient_colour: vector*3" &
			"): (" & 
				"emission: vector*3" &
			")");

		if (p.atmospheric_depth > 0.0) then
			p.atmosphere_func.Initialise_From_File(
				cf("atmosphere_func").Value,
				"(" &
					"xyz: vector*3," & 
					"bounding_radius: real," &
					"nominal_radius: real," &
					"camera_direction: vector*3," &
					"sun_direction: vector*3," &
					"sun_colour: vector*3" &
				"): (" & 
					"extinction: vector*3," &
					"emission: vector*3" &
				")");
		end if;
	end;

	function Test_Intersection(p: planet_t;
				r: ray_t; ray_entry, ray_exit: in out vec3_t;
				include_atmosphere: boolean := true)
			return boolean is
		radius: number;
		ro: vec3_t;
		a, b, c, q: number;
		disc2, disc: number;
		t0, t1: number;

		procedure Swap(a, b: in out number) is
			t: number;
		begin
			t := a;
			a := b;
			b := t;
		end;
	begin
		if include_atmosphere then
			radius := p.bounding_radius;
		else
			radius := p.nominal_radius;
		end if;

		ro := r.location - p.location;
		a := Dot(r.direction, r.direction);
		b := 2.0 * Dot(r.direction, ro);
		c := Dot(ro, ro) - (radius*radius);
		disc2 := b*b - 4.0*a*c;

		if (disc2 < 0.0) then
			-- ray_t does not intersect sphere at all.
			return false;
		end if;

		disc := sqrt(disc2);
		if (b < 0.0) then
			q := (-b - disc)/2.0;
		else
			q := (-b + disc)/2.0;
		end if;

		t0 := q / a;
		t1 := c / q;
		if (t0 > t1) then
			Swap(t0, t1);
		end if;

		if (t1 < 0.0) then
			-- The sphere is completely behind the ray_t.
			return false;
		end if;
		if (t0 < 1.0) then
			-- ray_t start appears to be inside the sphere.
			t0 := 1.0;
		end if;

		ray_entry := r.location + r.direction*t0;
		ray_exit := r.location + r.direction*t1;
		return true;
	end;

	-- xyz_rel_planet is a *planet* coordinate.
	function Get_Actual_Radius_P(p: planet_t; xyz_rel_planet: vec3_t)
			return number is
		normalised_xyz: vec3_t := NormaliseToSphere(xyz_rel_planet,
			p.nominal_radius);
		radius: number;
	begin
		p.terrain_radius_func.Call.all(normalised_xyz, p.bounding_radius,
			p.nominal_radius, radius);
		return radius;
	end;

	-- xyz_rel_planet is a *planet* coordinate.
	function Normalise_To_Surface_P(p: planet_t; xyz_rel_planet: vec3_t)
			return vec3_t is
	begin
		return NormaliseToSphere(xyz_rel_planet,
				Get_Actual_Radius_P(p, xyz_rel_planet));
	end;

	-- xyz is a *world* coordinate.
	function Get_Surface_Normal(p: planet_t; xyz: vec3_t) return vec3_t is
		xyz_rel_planet: vec3_t := xyz - p.location;

		-- isosurface function representing the planet:
		--   |xyz| - r = 0
		function isosurface(xyz: vec3_t) return number is
			(Length(xyz) - Get_Actual_Radius_P(p, xyz));

		value: number := isosurface(xyz_rel_planet);
		value_x: number := isosurface(xyz_rel_planet + (1.0, 0.0, 0.0)) - value;
		value_y: number := isosurface(xyz_rel_planet + (0.0, 1.0, 0.0)) - value;
		value_z: number := isosurface(xyz_rel_planet + (0.0, 0.0, 1.0)) - value;
	begin
		return Normalise((value_x, value_y, value_z));
	end;

	-- xyz is a *world* coordinate.
	function Is_Point_Underground(p: planet_t; xyz: vec3_t) return boolean is
		xyz_r: number := Length(xyz - p.location);
		real_r: number := p.Get_Actual_Radius(xyz);
	begin
		return xyz_r < real_r;
	end;

	-- xyz is a *world* coordinate.
	procedure Sample_Atmosphere(p: planet_t; xyz: vec3_t;
			camera_direction, sun_direction: vec3_t;
			sun_colour: colour_t;
			extinction, emission: out colour_t) is
		xyz_rel_planet: vec3_t := xyz - p.location;
	begin
		p.atmosphere_func.Call.all(xyz_rel_planet,
				p.bounding_radius, p.nominal_radius,
				camera_direction, sun_direction, sun_colour,
				extinction, emission);
	end;

	-- xyz is a *world* coordinate.
	procedure Sample_Surface(p: planet_t; xyz: vec3_t;
			camera_direction, sun_direction, surface_normal: vec3_t;
			sun_colour, ambient_colour: colour_t;
			emission: out colour_t) is
		xyz_rel_planet: vec3_t := xyz - p.location;
	begin
		p.terrain_surface_func.Call.all(xyz_rel_planet,
				p.nominal_radius,
				camera_direction, sun_direction, surface_normal,
				sun_colour, ambient_colour,
				emission);
	end;
end;

