with System.Storage_Elements;
with System.Storage_Pools;

package Garbage_Collector
is
	type Pool_Class is new System.Storage_Pools.Root_Storage_Pool
		with null record;

	function Storage_Size(this: Pool_Class)
		return System.Storage_Elements.Storage_Count;

	procedure Allocate(
			this: in out Pool_Class;
			address: out System.Address;
			size: System.Storage_Elements.Storage_Count;
			alignment: System.Storage_Elements.Storage_Count);

	procedure Deallocate(
			this: in out Pool_Class;
			address: System.Address;
			size: System.Storage_Elements.Storage_Count;
			alignment: System.Storage_Elements.Storage_Count);

	procedure Collect(this: in out Pool_Class);

	function Heap_Size(this: Pool_Class)
		return System.Storage_Elements.Storage_Count;

	Pool: Pool_Class;
end;

