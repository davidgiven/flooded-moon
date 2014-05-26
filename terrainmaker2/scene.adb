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

	function ComputePixelColour(r: Ray) return Colour is
		ints: Intersections;
		num: natural;
	begin
		ComputeObjectIntersections(r, ints, num);
		if (num = 0) then
			return RGB(0.0, 0.0, 0.0);
		else
			declare
				p: Planets.Lists.Ref := planets_list(ints(0).planet);
				locRelToPlanet: Point := ints(0).rayEntry - p.location;
				pixelColour: Vector3;
			begin
				p.terrain.Call.all(locRelToPlanet, p.bounding_radius, pixelColour);
				return RGB(pixelColour(0), pixelColour(1), pixelColour(2));
			end;
		end if;
	end;

end;

