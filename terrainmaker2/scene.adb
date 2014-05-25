with Ada.Text_IO;
with Ada.Containers.Generic_Array_Sort;
with Config;
with ConfigFiles;
with Planets;
with Vectors;
with Transforms;
with Images;
with Utils;

use Ada.Text_IO;
use Config;
use ConfigFiles;
use Planets;
use Vectors;
use Transforms;
use Images;
use Utils;

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
		t.Rotate(camera_right, ydegPerPixel * Number(x));
		dir := t.Transform(camera_forward);
		return Ray'(camera_location, dir);
	end;

	function ComputeObjectIntersections(r: Ray) return Intersections is
		maxObjects: natural := planets_list.Length;
		list: Intersections(0..maxObjects);
		hitObjects: natural := 0;

		procedure TestSingleObject(p: Planet; i: in out Intersection;
				index: natural) is
		begin
			if p.TestIntersection(r, i.rayEntry, i.rayExit) then
				i.planet := index;
				hitObjects := hitObjects + 1;
			end if;
		end;

		function Comparator(left, right: Intersection) return boolean is
			leftDistance: Number := Length(left.rayEntry - r.location);
			rightDistance: Number := Length(right.rayEntry - r.location);
		begin
			return leftDistance < rightDistance;
		end;
	begin
		for index in (planets_list.First_Index)..(planets_list.Last_Index) loop
			TestSingleObject(planets_list(index), list(hitObjects), index);
		end loop;

		-- Produce a correctly-sized array of intersections, from nearest to
		-- furthest.
		declare
			smallerList: Intersections := list(list'first..(hitObjects-1));

			procedure Sort is new Ada.Containers.Generic_Array_Sort(
					Index_Type => natural,
					Element_Type => Intersection,
					Array_Type => Intersections,
					"<" => Comparator);
		begin
			Sort(smallerList);
			return smallerList;
		end;
	end;
end;

