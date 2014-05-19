with Ada.Text_IO;
with ConfigFiles;
with Planets;

use Ada.Text_IO;
use ConfigFiles;
use Planets;

package body Scene is
	scene_cf: ConfigFile := ConfigFiles.Create;
	planets_list: Planets.Vector;

	procedure Load(filename: string) is
		planets_cf: ConfigFile;

		procedure add_planet(cf: ConfigFile) is
		begin
			Put_Line("add planet");
		end;
	begin
		scene_cf.Load(filename);

		planets_cf := scene_cf("planets");
		for i in 0..(planets_cf.Length-1) loop
			declare
				cf: ConfigFile := planets_cf(i);
				p: Planet := Planets.Create(cf);
			begin
				planets_list.Append(p);
			end;
		end loop;
	end;
end;

