with Ada.Text_IO;
with Ada.Containers.Generic_Sort;
with Config;
with ConfigFiles;
with Vectors;
with Transforms;
with Images;
with Utils;
with Colours;
with World.Universe;
with Planet;

use Ada.Text_IO;
use Config;
use Config.Number_Functions;
use ConfigFiles;
use Vectors;
use Transforms;
use Images;
use Utils;
use Colours;

package body Scene is
	scene_cf: node_t := ConfigFiles.Create;
	sun: integer := -1;
	sun_colour: colour_t;
	hfov, vfov: number;
	camera_forward: vec3_t;
	camera_right: vec3_t;
	camera_up: vec3_t;

	function Get_Camera_Location return vec3_t is
	begin
		return camera_location;
	end;

	function Get_Field_Of_View return vec2_t is
	begin
		return (hfov, vfov);
	end;

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

			if cf.Exists("bearing") then
				declare
					t: TransformMatrix;
				begin
					t.Reset;
					t.Rotate(camera_up, -cf("bearing").Value);
					camera_forward := t.Transform(camera_forward);
					camera_up := t.Transform(camera_up);
					camera_right := t.Transform(camera_right);
				end;
			end if;

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
			ints: out intersection_list_t;
			num: out natural;
			include_atmosphere: boolean := true) is
		procedure Test_Single_Object(p: access Planet.Object; i: in out intersection_t)
		is
		begin
			if p.Test_Intersection(r, i.ray_entry, i.ray_exit,
					include_atmosphere => include_atmosphere) then
				i.planet := p;
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
		for object of World.Universe.Bodies.all loop
			Test_Single_Object(object, ints(num));
		end loop;
		Test_Single_Object(World.Universe.Sun, ints(num));

		if (num > 1) then
			Sort(0, num-1);
		end if;
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
			ints(i).planet.Render_Planet(
				ints(i).ray_entry, ints(i).ray_exit,
					emission, transmittance);
			exit when Length2(transmittance) < 0.01;
		end loop;

		return emission; -- Mix(emission, (0.0, 1.0, 0.0), transmittance);
	end;

end;

