with World.Universe;
with Intersections;
with Scene;
with Utils;

use Intersections;
use Utils;

package body Planet is
	procedure Init(self: in out Class)
	is
	begin
		self.bounding_radius := self.nominal_radius + self.atmospheric_depth;
	end;

	function Get_Actual_Radius_P(self: Class;
				xyz_rel_planet: vec3_t)
			return number
	is
	begin
		return self.nominal_radius;
	end;

	function Get_Sample_Distance_P(self: Class;
				xyz_rel_planet: vec3_t)
			return number
	is
	begin
		return 10.0e3;
	end;

	procedure Render_Atmosphere_For_Segment(object: Class;
			segment_start, segment_end: vec3_t;
			emission, transmittance: in out colour_t)
	is
	begin
		emission := Black;
		transmittance := White;
	end;

	procedure Sample_Surface(self: Object;
			xyz: vec3_t;
			camera_direction, sun_direction, surface_normal: vec3_t;
			sun_colour: colour_t;
			ambient_colour: colour_t;
			emission: out colour_t)
	is
	begin
		self.Sample_Surface_P(xyz - self.location,
			camera_direction, sun_direction, surface_normal,
			sun_colour,
			ambient_colour,
			emission);
	end;

	function Normalise_To_Surface_P(self: Object;
				xyz_rel_planet: vec3_t)
			return vec3_t
	is
	begin
		return NormaliseToSphere(xyz_rel_planet,
				self.Get_Actual_Radius_P(xyz_rel_planet));
	end;

	-- xyz is a *world* coordinate.
	function Get_Surface_Normal(self: Object;
				xyz: vec3_t)
			return vec3_t
	is
		xyz_rel_planet: vec3_t := xyz - self.location;

		-- isosurface function representing the planet:
		--   |xyz| - r = 0
		function isosurface(xyz: vec3_t) return number is
			(Length(xyz) - self.Get_Actual_Radius_P(xyz));

		value: number := isosurface(xyz_rel_planet);
		value_x: number := isosurface(xyz_rel_planet + (1.0, 0.0, 0.0)) - value;
		value_y: number := isosurface(xyz_rel_planet + (0.0, 1.0, 0.0)) - value;
		value_z: number := isosurface(xyz_rel_planet + (0.0, 0.0, 1.0)) - value;
	begin
		return Normalise((value_x, value_y, value_z));
	end;

	-- xyz is a *world* coordinate.
	function Is_Point_Underground(self: Object;
				xyz: vec3_t)
			return boolean
	is
		xyz_rel_planet: vec3_t := xyz - self.location;
		xyz_r: number := Length(xyz_rel_planet);
		real_r: number := self.Get_Actual_Radius_P(xyz_rel_planet);
	begin
		return xyz_r < real_r;
	end;

	-- Don't override.
	function Test_Intersection(self: Object;
				ray: ray_t;
				ray_entry, ray_exit: in out vec3_t;
				include_atmosphere: boolean := true)
			return boolean
	is
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
			radius := self.bounding_radius;
		else
			radius := self.nominal_radius;
		end if;

		ro := ray.location - self.location;
		a := Dot(ray.direction, ray.direction);
		b := 2.0 * Dot(ray.direction, ro);
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

		ray_entry := ray.location + ray.direction*t0;
		ray_exit := ray.location + ray.direction*t1;
		return true;
	end;

	-- Coordinates here are *world* based.
	function Find_Intersection_With_Terrain(self: Object;
			segment_start: vec3_t; segment_end: in out vec3_t)
				return boolean
	is
		segment_delta: vec3_t;
		segment_length: number;
		plo, phi, pmid: number;
		p: vec3_t;
	begin
		-- Bail immediately if the end of the segment isn't underground.
		if not self.Is_Point_Underground(segment_end) then
			return false;
		end if;

		-- So the terrain surface is somewhere inside our segment. We're going
		-- to binary chop the segment until we get reasonably close to it.
		segment_delta := segment_end - segment_start;
		segment_length := Length(segment_delta);
		plo := 0.0;
		phi := 1.0;
		loop
			-- Calculate our subsegment midpoint; give up if it's too small.
			pmid := (plo + phi) / 2.0;
			p := segment_start + segment_delta*pmid;
			exit when (segment_length * (phi-plo)) < 1.0;

			if self.Is_Point_Underground(p) then
				phi := pmid;
			else
				plo := pmid;
			end if;
		end loop;

		-- p is now approximately at the surface.
		segment_end := p;
		return true;
	end;

	function Sunlight_From_Point(self: Object;
			here: vec3_t; sun_dir: vec3_t; sunlight: colour_t;
			allow_self_shadowing: boolean := false) return colour_t
	is
		ray: ray_t;
		ints: intersection_list_t;
		int: intersection_t renames ints(0);
		num: natural;
		transmittance: colour_t;
		dummy: boolean;
	begin
		-- Set up a ray from our sample point towards the sun.
		ray.location := here;
		ray.direction := sun_dir;

		-- Offset the start point a tiny amount above the surface of the
		-- planet, to avoid edge cases where the planet is flat.
		ray.location := ray.location + 
			Normalise(ray.location - self.location) * 1.0;

		-- Crudely intersect it with the object graph.
		Scene.Compute_Object_Intersections(ray, ints, num,
			include_atmosphere => false);
		if (num = 0) or else (ints(0).planet /= World.Universe.Sun) then
			-- The first hit from the ray isn't the sun, therefore we're
			-- in eclipse.
			return Black;
		end if;

		-- If we haven't been asked to do self-shadowing, which is
		-- expensive, assume the point is in full sunlight.
		if not allow_self_shadowing then
			return sunlight;
		end if;

		-- Re-intersect the ray with the planet's atmosphere, which will
		-- clip the ray to the atmosphere's bounds.
		dummy := self.Test_Intersection(ray, int.ray_entry, int.ray_exit,
			include_atmosphere => true);
		
		-- Now calculate the light colour based on self-shadowing
		-- and atmospheric scattering from that planet.
		transmittance := self.Compute_Sunlight(int.ray_entry, int.ray_exit);
		return Mix(black, sunlight, transmittance);
	end;

	procedure Light_Terrain(self: Object;
			here: vec3_t;
			emission, transmittance: in out colour_t)
	is
		camera_dir, sun_dir: vec3_t;
		surface_normal: vec3_t;
		sunlight, directlight, ambient: colour_t;
		emission_here: colour_t;
	begin
		-- Fake a little ambient light.
		sunlight := World.Universe.Sun.colour;
		ambient := sunlight * 0.1;
		sunlight := sunlight - ambient;

		sun_dir := Normalise(World.Universe.Sun.location - here);
		directlight := self.Sunlight_From_Point(here,
				sun_dir, sunlight,
				allow_self_shadowing => true);
		camera_dir := Normalise(Scene.camera_location - here);

		surface_normal := self.Get_Surface_Normal(here);

		self.Sample_Surface(here,
				camera_dir, sun_dir, surface_normal,
				directlight, ambient,
				emission_here);
		emission := emission + transmittance*emission_here;
		transmittance := Black;
	end;

	procedure Render_Planet(self: Object;
			ray_start, ray_end: vec3_t;
			emission, transmittance: in out colour_t)
	is
		t: number := 0.0;
		maxt: number := Length(ray_end - ray_start);
		ray_direction: vec3_t := (ray_end - ray_start) / maxt;
		segment_start, segment_end: vec3_t;
		step_size: number;
		underground: boolean;
	begin
		segment_end := ray_start;
		while (t < maxt) loop
			-- Determine segment under consideration.
			segment_start := segment_end;

			step_size := self.Get_Sample_Distance(segment_start);
			t := clamp(t + step_size, 0.0, maxt);
			segment_end := ray_start + ray_direction*t;

			-- Are we underground? If so, adjust the end of the segment to
			-- be at ground level.
			underground := self.Find_Intersection_With_Terrain(
				segment_start, segment_end);

			-- Render atmosphere for the (possibly adjusted) segment.
			self.Render_Atmosphere_For_Segment(
				segment_start, segment_end,
				emission, transmittance);

			-- If we've reached the planet, render that and stop.
			if underground then
				self.Light_Terrain(segment_end,
					emission, transmittance);
				exit;
			end if;

			-- Stop iterating if we're unlikely to see any more down this
			-- ray.
			exit when Length2(transmittance) < 0.01;
		end loop;
	end;

	function Compute_Sunlight(self: Object;
			ray_start, ray_end: vec3_t)
			return colour_t
	is
		t: number := 0.0;
		maxt: number := Length(ray_end - ray_start);
		ray_direction: vec3_t := (ray_end - ray_start) / maxt;
		segment_start, segment_end: vec3_t;
		step_size: number;
		transmittance: colour_t := White;
	begin
		segment_end := ray_start;
		while (t < maxt) loop
			-- Determine segment under consideration.
			segment_start := segment_end;

			step_size := self.Get_Sample_Distance(segment_start);
			t := clamp(t + step_size, 0.0, maxt);
			segment_end := ray_start + ray_direction*t;

			-- Are we underground? If so, give up --- no sunlight here.
			if self.Find_Intersection_With_Terrain(segment_start, segment_end) then
				return Black;
			end if;

			-- TODO: consider atmospheric opacity.

			-- Stop iterating if we're unlikely to see any more down this
			-- ray.
			exit when Length2(transmittance) < 0.01;
		end loop;
		return transmittance;
	end;

end;

