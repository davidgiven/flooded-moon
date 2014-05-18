with Ada.Finalization;
with Interfaces.C;
with Config;

use Config;

package ConfigFiles is
	type ConfigFile is tagged private with
		Constant_Indexing => Get;

	procedure Adjust(cf: in out ConfigFile);
	procedure Finalize(cf: in out ConfigFile);

	function Load(filename: string) return ConfigFile;
	function Get(cf: ConfigFile; element: string) return ConfigFile;

	function Get(cf: ConfigFile) return Number;
	function Get(cf: ConfigFile) return string;
private
	type ConfigFileC is null record;
	type ConfigFileCRef is access ConfigFileC;
	
	type ConfigFileImpl is limited record
		refcount: natural := 1;
		c: ConfigFileCRef;
	end record;
	type ConfigFileImplRef is access ConfigFileImpl;

	type ConfigSettingC is null record;
	type ConfigSettingCRef is access ConfigSettingC;

	type ConfigFile is new Ada.Finalization.Controlled with
	record
		impl: ConfigFileImplRef;
		s: ConfigSettingCRef;
	end record;
end;
