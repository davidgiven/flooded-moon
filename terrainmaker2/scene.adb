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
use Config.NumberFunctions;
use ConfigFiles;
use Planets;
use Vectors;
use Transforms;
use Images;
use Utils;
use Colours;

package body Scene is
	scene_cf: ConfigFile := ConfigFiles.Create;
	planets_list: Planets.List;
	sun: integer := -1;
	sunColour: Colour;
	hfov, vfov: Number;
	camera_location: Point;
	camera_forward: Vector3;
	camera_right: Vector3;
	camera_up: Vector3;

	procedure Load(filename: string) is
	begin
		scene_cf.Load(filename);

		-- Crudely initialise the camera.
		declare
			cf: ConfigFile := scene_cf("camera");
			aspect: Number;
		begin
			hfov := cf("hfov").Value;
			aspect := cf("aspect").Value;
			vfov := hfov / aspect;
			camera_location := Load(cf("location"));
			camera_forward := Load(cf("forward"));
			camera_up := Load(cf("up"));
			camera_right := Cross(camera_forward, camera_up);
		end;

		-- Read in list of planets.
		declare
			cf: ConfigFile := scene_cf("planets");
		begin
			for i in 0..(cf.Length-1) loop
				planets_list.Add.Init(cf(i));

				if (cf(i).Name = "sun") then
					sun := i;
					sunColour := Load(cf(i)("colour"));
				end if;
			end loop;
		end;
		if (sun = -1) then
			Error("Scene has no planet called 'sun' in it!");
		end if;
	end;

	function ComputePrimaryRay(x, y: integer; img: Image) return Ray is
		t: TransformMatrix;
		xdegPerPixel: Number := hfov / Number(img.Width);
		ydegPerPixel: Number := vfov / Number(img.Height);
		dir: Vector3;
	begin
		t.Reset;
		t.Rotate(camera_up, xdegPerPixel * Number(x));
		t.Rotate(camera_right, ydegPerPixel * Number(y));
		dir := t.Transform(camera_forward);
		return Ray'(camera_location, dir);
	end;

	procedure ComputeObjectIntersections(r: Ray;
			ints: out Intersections; num: out natural;
			Clip_Against_Atmosphere: boolean := true) is
		procedure TestSingleObject(p: Planet; i: in out Intersection;
				index: natural) is
		begin
			if p.TestIntersection(r, i.rayEntry, i.rayExit,
					Clip_Against_Atmosphere => Clip_Against_Atmosphere) then
				i.planet := index;
				num := num + 1;
			end if;
		end;

		function Before(left, right: natural) return boolean is
			leftDistance: Number := Length(ints(left).rayEntry - r.location);
			rightDistance: Number := Length(ints(right).rayEntry - r.location);
		begin
			return leftDistance < rightDistance;
		end;

		procedure Swap(a, b: natural) is
			t: Intersection := ints(a);
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

	function SunlightFromPoint(p: Planet;
			loc: Point; sunDir: Vector3) return Colour is
		r: Ray;
		ints: Intersections;
		num: natural;
	begin
		r.location := loc;
		r.direction := sunDir;
		-- Crudely intersect the ray of light with our objects
		-- to see if we're in eclipse.
		ComputeObjectIntersections(r, ints, num,
			Clip_Against_Atmosphere => false);
		if (num > 0) and (ints(0).planet = sun) then
			return sunColour;
		else
			return Black;
		end if;
	end;

	procedure AccumulateSamplesThroughPlanet(p: Planet; int: Intersection;
			r: Ray; emission, transmittance: in out Colour) is
		t: Number := 0.0;
		maxt: Number := Length(int.rayExit - int.rayEntry);
		loc, ploc: Point;
		stepSize: Number;

		sunObject: Planet renames planets_list(sun);
		sunlight: Colour;
		sunDir, cameraDir: Vector3;
		extinctionHere, emissionHere: Colour;
		transmittanceHere: Colour;
		densityHere: number;
	begin
		--Put_Line("sample");
		while (t < maxt) loop
			-- Sample half-way along this segment (for slightly better
			-- accuracy).
			loc := int.rayEntry + r.direction*t;
			ploc := loc - p.location;
			stepSize := 1000.0; -- one km
			sunDir := Normalise(sunObject.location - loc);
			cameraDir := Normalise(camera_location - loc);
			sunlight := SunlightFromPoint(p, loc, sunDir);

			if p.IsPointUnderground(ploc) then
				-- Ray gets stoppped by ground.
				emissionHere := RGB(1.0, 0.0, 0.0);
				emission := emission + transmittance*emissionHere;
				transmittance := Black;
				return;
			elsif (p.atmospheric_depth > 0.0) then
				-- Ray travels through atmosphere.
				p.SampleAtmosphere(ploc, cameraDir, sunDir, sunlight,
						extinctionHere, emissionHere, densityHere);
			else
				-- Planet *has* no atmosphere! This shouldn't happen; it's
				-- an edge case --- the next step will likely intercept the
				-- surface.
				extinctionHere := Black;
				emissionHere := Black;
			end if;

			-- Calculate transmittance for this segment (0 means opaque,
			-- 1.0 means transparent).
			transmittanceHere := exp(-extinctionHere *
					densityHere * stepSize);

			-- Calculate cumulative transmittance and emission.
			transmittance := transmittance * transmittanceHere;

			-- Colour of this pixel is the colour accumulated so far, plus
			-- the colour of the new segment attenuated 
			emission := emission + transmittance*emissionHere*densityHere*stepSize*sunlight;

			-- Stop iterating if we're unlikely to see any more down this
			-- ray.
			exit when Length2(transmittance) < 0.01;

			t := t + stepSize;
		end loop;

		-- Finished with ray.
	end;

	function ComputePixelColour(r: Ray) return Colour is
		ints: Intersections;
		num: natural;
		emission: Colour := Black;
		transmittance: Colour := White;
	begin
		ComputeObjectIntersections(r, ints, num,
					Clip_Against_Atmosphere => true);
		for i in 0..(num-1) loop
			AccumulateSamplesThroughPlanet(
					planets_list(ints(i).planet), ints(i), r,
					emission, transmittance);
			exit when Length2(transmittance) < 0.01;
		end loop;

		return emission; --Mix(emission, RGB(0.0, 0.0, 1.0), transmission);
	end;

end;

