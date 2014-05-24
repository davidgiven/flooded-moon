with Ada.Containers.Indefinite_Vectors;
with Config;
with Matrices;
with ConfigFiles;
with GenericLists;
with Transforms;

use Config;
use Matrices;
use ConfigFiles;
use Transforms;

package Planets is
	type Planet is tagged limited record
		cf: ConfigFile;
		nominal_radius: Number;
		atmospheric_depth: Number;

		bounding_radius: Number;
		transform: TransformMatrix;
	end record;

	procedure Init(p: in out Planet; cf: ConfigFile);

	package Lists is new GenericLists(Planet);
	type List is new Lists.List with null record;
end;


