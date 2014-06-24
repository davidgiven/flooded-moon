with Ada.Finalization;
with Interfaces.C;
with Config;

use Config;

package ConfigFiles is
	type node_t is tagged private with
		Constant_Indexing => Get;

	config_exception: exception;
	config_parse_exception: exception;

	procedure Adjust(cf: in out node_t);
	procedure Finalize(cf: in out node_t);

	function Create return node_t;
	procedure Load(cf: in out node_t; filename: string);
	function Get(cf: node_t; element: string) return node_t;
	function Get(cf: node_t; element: integer) return node_t;
	function Exists(cf: node_t; element: string) return boolean;
	function Length(cf: node_t) return integer;
	function Name(cf: node_t) return string;

	function Value(cf: node_t) return number;
	function Value(cf: node_t) return string;
private
	type node_c_t is null record;
	type node_c_ref is access node_c_t;
	
	type node_impl_t is limited record
		refcount: natural := 1;
		c: node_c_ref;
	end record;
	type node_impl_ref is access node_impl_t;

	type setting_c_t is null record;
	type setting_c_ref is access setting_c_t;

	type node_t is new Ada.Finalization.Controlled with
	record
		impl: node_impl_ref;
		s: setting_c_ref;
	end record;
end;
