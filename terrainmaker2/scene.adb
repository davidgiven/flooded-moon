with Ada.Text_IO;
with Ada.Containers.Generic_Sort;
with Config;
with ConfigFiles;
with Planets;
with Vectors;
with Transforms;
with Images;
with Utils;
with Colours;

use Ada.Text_IO;
use Config;
use Config.Number_Functions;
use ConfigFiles;
use Planets;
use Vectors;
use Transforms;
use Images;
use Utils;
use Colours;

package body Scene is
	scene_cf: node_t := ConfigFiles.Create;
	planets_list: planet_list_t;
	sun: integer := -1;
	sun_colour: colour_t;
	hfov, vfov: number;
	camera_location: vec3_t;
	camera_forward: vec3_t;
	camera_right: vec3_t;
	camera_up: vec3_t;

	function Compute_Light_Visibility(planet: planet_t;
			int: intersection_t;
			ray: ray_t) return colour_t;

	procedure Load(filename: string) is
	begin
		scene_cf.Load(filename);

		-- Crudely initialise the camera.
		declare
			cf: node_t := scene_cf("camera");
			aspect: number;
		begin
			hfov := cf("hfov").Value;
			aspect := cf("aspect").Value;
			vfov := hfov / aspect;
			camera_location := Load(cf("location"));
			camera_forward := Load(cf("forward"));
			camera_up := Load(cf("up"));
			camera_right := Cross(camera_forward, camera_up);

			if cf.Exists("pitch") then
				declare
					t: TransformMatrix;
				begin
					t.Reset;
					t.Rotate(camera_right, -cf("pitch").Value);
					camera_forward := t.Transform(camera_forward);
					camera_up := t.Transform(camera_up);
					camera_right := t.Transform(camera_right);
				end;
			end if;
		end;

		-- Read in list of planets.
		declare
			cf: node_t := scene_cf("planets");
		begin
			for i in 0..(cf.Length-1) loop
				planets_list.Add.Init(cf(i));

				if (cf(i).Name = "sun") then
					sun := i;
					sun_colour := Load(cf(i)("colour"));
				end if;
			end loop;
		end;
		if (sun = -1) then
			Error("Scene has no planet called 'sun' in it!");
		end if;
	end;

	function Compute_Primary_Ray(x, y: integer; img: image_t) return ray_t is
		t: TransformMatrix;
		xdegPerPixel: number := hfov / number(img.Width);
		ydegPerPixel: number := vfov / number(img.Height);
		dir: vec3_t;
	begin
		t.Reset;
		t.Rotate(camera_up, xdegPerPixel * number(x));
		t.Rotate(camera_right, ydegPerPixel * number(y));
		dir := t.Transform(camera_forward);
		return ray_t'(camera_location, dir);
	end;

	procedure Compute_Object_Intersections(r: ray_t;
			ints: out intersection_list_t; num: out natural;
			include_atmosphere: boolean := true) is
		procedure Test_Single_Object(p: planet_t; i: in out intersection_t;
				index: natural) is
		begin
			if p.Test_Intersection(r, i.ray_entry, i.ray_exit,
					include_atmosphere => include_atmosphere) then
				i.planet := index;
				num := num + 1;
			end if;
		end;

		function Before(left, right: natural) return boolean is
			leftDistance: number := Length(ints(left).ray_entry - r.location);
			rightDistance: number := Length(ints(right).ray_entry - r.location);
		begin
			return leftDistance < rightDistance;
		end;

		procedure Swap(a, b: natural) is
			t: intersection_t := ints(a);
		begin
			ints(a) := ints(b);
			ints(b) := t;
		end;

		procedure Sort is new Ada.Containers.Generic_Sort(
				Index_Type => natural,
				Swap => Swap,
				Before => Before);
	begin
		num := 0;
		for index in (planets_list.First_Index)..(planets_list.Last_Index) loop
			Test_Single_Object(planets_list(index), ints(num), index);
		end loop;

		if (num > 1) then
			Sort(0, num-1);
		end if;
	end;

	function Sunlight_From_Point(planet: planet_t;
			here: vec3_t; sun_dir: vec3_t; sunlight: colour_t;
			allow_self_shadowing: boolean := false) return colour_t is
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
			Normalise(ray.location - planet.location) * 1.0;

		-- Crudely intersect it with the object graph.
		Compute_Object_Intersections(ray, ints, num,
			include_atmosphere => false);
		if (num = 0) or else (ints(0).planet /= sun) then
			-- No --- we're in eclipse.
			return Black;
		end if;

		-- If we haven't been asked to do self-shadowing, which is
		-- expensive, assume the point is in full sunlight.
		if not allow_self_shadowing then
			return sun_colour;
		end if;

		-- Re-intersect the ray with the planet we're interested in,
		-- but including the atmosphere this time. This will tell us
		-- the length of the atmospheric segment.
		dummy := planet.Test_Intersection(ray, int.ray_entry, int.ray_exit,
			include_atmosphere => true);
		
		-- Now calculate the light colour based on self-shadowing
		-- and atmospheric scattering from that planet.
		transmittance := Compute_Light_Visibility(planet, int, ray);
		return Mix(black, sunlight, transmittance);
	end;

	-- Coordinates here are *world* based.
	procedure Render_Atmosphere_For_Segment(planet: planet_t;
			segment_start, segment_end: vec3_t;
			emission, transmittance: in out colour_t) is
		here: vec3_t;
		camera_dir, sun_dir: vec3_t;
		sunlight: colour_t;
		extinction_here, emission_here, transmittance_here: colour_t;
		segment_length: number;
	begin
		-- Give up immediately if there is no atmosphere.
		if (planet.atmospheric_depth = 0.0) then
			return;
		end if;

		-- Sampling is done for the middle of our segment.
		here := (segment_start + segment_end) / 2.0;

		-- Calculate sunlight at this point.
		sun_dir := Normalise(planets_list(sun).location - here);
		sunlight := Sunlight_From_Point(planet, here, sun_dir, sun_colour,
			allow_self_shadowing => false);

		-- Sample the atmosphere here.
		camera_dir := Normalise(camera_location - here);
		planet.Sample_Atmosphere(here,
				camera_dir, sun_dir, sunlight,
				extinction_here, emission_here);

		-- Calculate transmittance for this segment (0 means opaque,
		-- 1.0 means transparent).
		segment_length := Length(segment_end - segment_start);
		transmittance_here := exp(-extinction_here * segment_length);

		-- Calculate cumulative transmittance and emission.
		transmittance := transmittance * transmittance_here;

		-- colour_t of this pixel is the colour_t accumulated so far, plus
		-- the colour_t of the new segment attenuated 
		emission := emission +
			transmittance*emission_here*segment_length*sunlight;
	end;

	-- Coordinates here are *world* based.
	function Find_Intersection_With_Terrain(planet: planet_t;
			segment_start: vec3_t; segment_end: in out vec3_t) return boolean is
		segment_delta: vec3_t;
		segment_length: number;
		plo, phi, pmid: number;
		p: vec3_t;
	begin
		-- Bail immediately if the end of the segment isn't underground.
		if not planet.Is_Point_Underground(segment_end) then
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

			if planet.Is_Point_Underground(p) then
				phi := pmid;
			else
				plo := pmid;
			end if;
		end loop;

		-- p is now approximately at the surface.
		segment_end := p;
		return true;
	end;

	-- Coordinates here are *world* based.
	procedure Render_Terrain(planet: planet_t;
			here: vec3_t;
			emission, transmittance: in out colour_t) is
		camera_dir, sun_dir: vec3_t;
		surface_normal: vec3_t;
		sunlight, directlight, ambient: colour_t;
		emission_here: colour_t;
	begin
		-- Fake a little ambient light.
		sunlight := sun_colour;
		ambient := sunlight * 0.1;
		sunlight := sunlight - ambient;

		sun_dir := Normalise(planets_list(sun).location - here);
		directlight := Sunlight_From_Point(planet, here,
				sun_dir, sunlight,
				allow_self_shadowing => true);
		camera_dir := Normalise(camera_location - here);

		surface_normal := planet.Get_Surface_Normal(here);

		planet.Sample_Surface(here,
				camera_dir, sun_dir, surface_normal,
				directlight, ambient,
				emission_here);
		emission := emission + transmittance*emission_here;
		transmittance := Black;
	end;

	-- Coordinates here are *world* based.
	procedure Render_Planet(planet: planet_t;
			int: intersection_t;
			ray: ray_t; emission, transmittance: in out colour_t) is
		t: number := 0.0;
		maxt: number := Length(int.ray_exit - int.ray_entry);
		segment_start, segment_end: vec3_t;
		step_size: number;
		underground: boolean;
	begin
		while (t < maxt) loop
			-- Determine segment under consideration.
			step_size := 1000.0;
			segment_start := int.ray_entry + ray.direction*t;
			t := t + step_size;
			segment_end := int.ray_entry + ray.direction*t;

			-- Are we underground? If so, adjust the end of the segment to
			-- be at ground level.
			underground := Find_Intersection_With_Terrain(planet,
				segment_start, segment_end);

			-- Render atmosphere for the (possibly adjusted) segment.
			Render_Atmosphere_For_Segment(planet,
				segment_start, segment_end,
				emission, transmittance);

			-- If we've reached the planet, render that and stop.
			if underground then
				Render_Terrain(planet, segment_end,
					emission, transmittance);
				exit;
			end if;

			-- Stop iterating if we're unlikely to see any more down this
			-- ray.
			exit when Length2(transmittance) < 0.01;
		end loop;
	end;

	-- This is a simplified version of Render_Planet used for
	-- shadow casting. It considers only transmittance.
	-- Coordinates here are *world* based.
	function Compute_Light_Visibility(planet: planet_t;
			int: intersection_t;
			ray: ray_t) return colour_t is
		t: number := 0.0;
		maxt: number := Length(int.ray_exit - int.ray_entry);
		segment_start, segment_end: vec3_t;
		step_size: number;

		transmittance: colour_t := white;
	begin
		while (t < maxt) loop
			-- Determine segment under consideration.
			step_size := 1000.0;
			segment_start := int.ray_entry + ray.direction*t;
			t := t + step_size;
			segment_end := int.ray_entry + ray.direction*t;

			-- Are we underground? If so, give up --- no light here.
			if Find_Intersection_With_Terrain(planeT,
				segment_start, segment_end) then
				return black;
			end if;

			-- Stop iterating if we're unlikely to see any more down this
			-- ray.
			exit when Length2(transmittance) < 0.01;
		end loop;

		return transmittance;
	end;

	function Compute_Pixel_Colour(ray: ray_t) return colour_t is
		ints: intersection_list_t;
		num: natural;
		emission: colour_t := Black;
		transmittance: colour_t := White;
	begin
		Compute_Object_Intersections(ray, ints, num,
					include_atmosphere => true);
		for i in 0..(num-1) loop
			Render_Planet(
					planets_list(ints(i).planet), ints(i), ray,
					emission, transmittance);
			exit when Length2(transmittance) < 0.01;
		end loop;

		return emission; -- Mix(emission, (0.0, 1.0, 0.0), transmittance);
	end;

end;

