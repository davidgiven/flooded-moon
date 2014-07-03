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
			ints: out Intersections; num: out natural;
			include_atmosphere: boolean := true) is
		procedure TestSingleObject(p: planet_t; i: in out intersection_t;
				index: natural) is
		begin
			if p.Test_Intersection(r, i.ray_entry, i.ray_exit,
					include_atmosphere => include_atmosphere) then
				i.planet_t := index;
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
			TestSingleObject(planets_list(index), ints(num), index);
		end loop;

		if (num > 1) then
			Sort(0, num-1);
		end if;
	end;

	function Sunlight_From_Point(planet: planet_t;
			loc: vec3_t; sunDir: vec3_t) return colour_t is
		r: ray_t;
		ints: Intersections;
		num: natural;
	begin
		r.location := loc;
		r.direction := sunDir;
		-- Crudely intersect the ray_t of light with our objects
		-- to see if we're in eclipse.
		Compute_Object_Intersections(r, ints, num,
			include_atmosphere => false);
		if (num > 0) and (ints(0).planet_t = sun) then
			return sun_colour;
		else
			return Black;
		end if;
	end;

	-- Coordinates here are *world* based.
	procedure Calculate_Atmosphere_For_Segment(planet: planet_t;
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
		sunlight := Sunlight_From_Point(planet, here, sun_dir);

		-- Sample the atmosphere here.
		camera_dir := Normalise(camera_location - here);
		planet.Sample_Atmosphere(here - planet.location,
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
	begin
		return planet.Is_Point_Underground(segment_end - planet.location);
	end;

	-- Coordinates here are *world* based.
	procedure Render_Terrain(planet: planet_t;
			here: vec3_t;
			emission, transmittance: in out colour_t) is
		camera_dir, sun_dir: vec3_t;
		sunlight: colour_t;
	begin
		sun_dir := Normalise(planets_list(sun).location - here);
		sunlight := Sunlight_From_Point(planet, here, sun_dir);
		camera_dir := Normalise(camera_location - here);

		emission := emission + transmittance*(1.0, 0.0, 0.0)*sunlight;
		transmittance := Black;
	end;

	-- Coordinates here are *world* based.
	procedure Accumulate_Samples_Through_Planet(planet: planet_t;
			int: intersection_t;
			r: ray_t; emission, transmittance: in out colour_t) is
		t: number := 0.0;
		maxt: number := Length(int.ray_exit - int.ray_entry);
		segment_start, segment_end: vec3_t;
		step_size: number;
		underground: boolean;

		sunlight: colour_t;
		sunDir, cameraDir: vec3_t;
		extinctionHere, emissionHere: colour_t;
		transmittanceHere: colour_t;
	begin
		while (t < maxt) loop
			-- Determine segment under consideration.
			step_size := 1000.0;
			segment_start := int.ray_entry + r.direction*t;
			t := t + step_size;
			segment_end := int.ray_entry + r.direction*t;

			-- Are we underground? If so, adjust the end of the segment to
			-- be at ground level.
			underground := Find_Intersection_With_Terrain(planet,
				segment_start, segment_end);

			-- Render atmosphere for the (possibly adjusted) segment.
			Calculate_Atmosphere_For_Segment(planet,
				segment_start, segment_end,
				emission, transmittance);

			-- If we've reached the planet, render that and stop.
			if underground then
				Render_Terrain(planet, segment_end,
					emission, transmittance);
				exit;
			end if;

			-- Stop iterating if we're unlikely to see any more down this
			-- ray_t.
			exit when Length2(transmittance) < 0.01;
		end loop;

		-- Finished with ray.
	end;

	function Compute_Pixel_Colour(r: ray_t) return colour_t is
		ints: Intersections;
		num: natural;
		emission: colour_t := Black;
		transmittance: colour_t := White;
	begin
		Compute_Object_Intersections(r, ints, num,
					include_atmosphere => true);
		for i in 0..(num-1) loop
			Accumulate_Samples_Through_Planet(
					planets_list(ints(i).planet_t), ints(i), r,
					emission, transmittance);
			exit when Length2(transmittance) < 0.01;
		end loop;

		return emission; --Mix(emission, RGB(0.0, 0.0, 1.0), transmission);
	end;

end;

