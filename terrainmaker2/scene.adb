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
	camera_location: Point;
	camera_forward: Vector3;
	camera_right: Vector3;
	camera_up: Vector3;

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
		dir: Vector3;
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

	function SunlightFromPoint(p: planet_t;
			loc: Point; sunDir: Vector3) return colour_t is
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

	procedure AccumulateSamplesThroughPlanet(p: planet_t; int: intersection_t;
			r: ray_t; emission, transmittance: in out colour_t) is
		t: number := 0.0;
		maxt: number := Length(int.ray_exit - int.ray_entry);
		loc, ploc: Point;
		stepSize: number;

		sunObject: planet_t renames planets_list(sun);
		sunlight: colour_t;
		sunDir, cameraDir: Vector3;
		extinctionHere, emissionHere: colour_t;
		transmittanceHere: colour_t;
	begin
		--Put_Line("sample");
		while (t < maxt) loop
			-- Sample half-way along this segment (for slightly better
			-- accuracy).
			loc := int.ray_entry + r.direction*t;
			ploc := loc - p.location;
			stepSize := 1000.0; -- one km
			sunDir := Normalise(sunObject.location - loc);
			cameraDir := Normalise(camera_location - loc);
			sunlight := SunlightFromPoint(p, loc, sunDir);

			if p.Is_Point_Underground(ploc) then
				-- ray_t gets stoppped by ground.
				emissionHere := RGB(1.0, 0.0, 0.0);
				emission := emission + transmittance*emissionHere;
				transmittance := Black;
				return;
			elsif (p.atmospheric_depth > 0.0) then
				-- ray_t travels through atmosphere.
				p.Sample_Atmosphere(ploc, cameraDir, sunDir, sunlight,
						extinctionHere, emissionHere);
			else
				-- planet_t *has* no atmosphere! This shouldn't happen; it's
				-- an edge case --- the next step will likely intercept the
				-- surface.
				extinctionHere := Black;
				emissionHere := Black;
			end if;

			-- Calculate transmittance for this segment (0 means opaque,
			-- 1.0 means transparent).
			transmittanceHere := exp(-extinctionHere * stepSize);

			-- Calculate cumulative transmittance and emission.
			transmittance := transmittance * transmittanceHere;

			-- colour_t of this pixel is the colour_t accumulated so far, plus
			-- the colour_t of the new segment attenuated 
			emission := emission + transmittance*emissionHere*stepSize*sunlight;

			-- Stop iterating if we're unlikely to see any more down this
			-- ray_t.
			exit when Length2(transmittance) < 0.01;

			t := t + stepSize;
		end loop;

		-- Finished with ray_t.
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
			AccumulateSamplesThroughPlanet(
					planets_list(ints(i).planet_t), ints(i), r,
					emission, transmittance);
			exit when Length2(transmittance) < 0.01;
		end loop;

		return emission; --Mix(emission, RGB(0.0, 0.0, 1.0), transmission);
	end;

end;

