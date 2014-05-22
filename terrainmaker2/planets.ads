with Ada.Containers.Indefinite_Vectors;
with Config;
with Matrices;
with ConfigFiles;
with CountedPointers;

use Config;
use Matrices;
use ConfigFiles;

package Planets is
	type Planet is tagged limited record
		cf: ConfigFile;
		nominal_radius: Number;
		atmospheric_depth: Number;

		bounding_radius: Number;
		transform: Matrix4;
	end record;

	procedure Init(p: in out Planet; cf: ConfigFile);

	package Pointers is new CountedPointers(Planet);
	use Pointers;
	subtype Ptr is Pointers.Ptr;

	function Create(cf: ConfigFile) return Ptr;

	package Container is new Ada.Containers.Indefinite_Vectors(Natural, Ptr);
	type Vector is new Container.Vector with null record;
end;


