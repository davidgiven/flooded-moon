with Interfaces.C;
with Ada.Text_IO;
with System.Address_Image;

use Interfaces.C;
use Ada.Text_IO;

package body Garbage_Collector
is
	package C is
		procedure GC_init;
		pragma import(C, GC_init, "GC_init");

		function GC_malloc(size: size_t)
			return System.Address;
		pragma import(C, GC_malloc, "GC_malloc");

		procedure GC_gcollect;
		pragma import(C, GC_gcollect, "GC_gcollect");

		function GC_get_heap_size
			return System.Storage_Elements.Storage_Count;
		pragma import(C, GC_get_heap_size, "GC_get_heap_size");
	end;

	function Storage_Size(this: Pool_Class)
		return System.Storage_Elements.Storage_Count
	is
	begin
		return 0;
	end;

	procedure Allocate(
			this: in out Pool_Class;
			address: out System.Address;
			size: System.Storage_Elements.Storage_Count;
			alignment: System.Storage_Elements.Storage_Count)
	is
	begin
		address := C.GC_malloc(size_t(size));
	end;

	procedure Deallocate(
			this: in out Pool_Class;
			address: System.Address;
			size: System.Storage_Elements.Storage_Count;
			alignment: System.Storage_Elements.Storage_Count)
	is
	begin
		null;
	end;

	procedure Collect(this: in out Pool_Class)
	is
	begin
		C.GC_gcollect;
	end;

	function Heap_Size(this: Pool_Class)
		return System.Storage_Elements.Storage_Count
	is
	begin
		return C.GC_get_heap_size;
	end;
begin
	C.GC_init;
end;


