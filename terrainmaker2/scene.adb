with Ada.Text_IO;
with Config;
with ConfigFiles;
with Planets;
with Vectors;
with Transforms;

use Ada.Text_IO;
use Config;
use ConfigFiles;
use Planets;
use Vectors;
use Transforms;

package body Scene is
	scene_cf: ConfigFile := ConfigFiles.Create;
	planets_list: Planets.List;
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
			end loop;
		end;
	end;
end;

