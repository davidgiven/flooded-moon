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
			ints: out Intersections; num: out natural) is
		procedure TestSingleObject(p: Planet; i: in out Intersection;
				index: natural) is
		begin
			if p.TestIntersection(r, i.rayEntry, i.rayExit) then
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

	procedure AccumulateSamplesThroughPlanet(p: Planet; int: Intersection;
			r: Ray; emission, transmission: in out Colour) is
		t: Number := 0.0;
		maxt: Number := Length(int.rayExit - int.rayEntry);
		loc, ploc: Point;
		stepSize: Number;

		sunObject: Planet renames planets_list(sun);
		sunDir: Vector3;
		extinctionHere, emissionHere: Colour;
	begin
		while (t < maxt) loop
			loc := int.rayEntry + r.direction*t;
			ploc := loc - p.location;
			stepSize := 1000.0; -- one km
			sunDir := loc - sunObject.location;

			-- Is this sample underground?
			if p.IsPointUnderground(ploc) then
				-- Ray gets stoppped by ground.
				emission := emission + transmission*RGB(1.0, 0.0, 0.0);
				transmission := RGB(0.0, 0.0, 0.0);
				return;
			end if;
			if (p.atmospheric_depth > 0.0) then
				p.SampleAtmosphere(ploc, sunDir, sunColour,
						extinctionHere, emissionHere);
			else
				extinctionHere := (0.0, 0.0, 0.0);
				emissionHere := (0.0, 0.0, 0.0);
			end if;

			transmission := transmission * exp(-extinctionHere * stepSize);
			emission := emission + transmission * emissionHere;

			-- Stop iterating if we're unlikely to see any more down this
			-- ray.
			exit when Length2(transmission) < 0.01;

			t := t + stepSize;
		end loop;

		-- Ray escapes into space.
	end;

	function ComputePixelColour(r: Ray) return Colour is
		ints: Intersections;
		num: natural;
		emission: Colour := RGB(0.0, 0.0, 0.0);
		transmission: Colour := RGB(1.0, 1.0, 1.0);
	begin
		ComputeObjectIntersections(r, ints, num);
		for i in 0..(num-1) loop
			AccumulateSamplesThroughPlanet(
					planets_list(ints(i).planet), ints(i), r,
					emission, transmission);
		end loop;

		return emission; --Mix(emission, RGB(0.0, 0.0, 1.0), transmission);
	end;

end;

