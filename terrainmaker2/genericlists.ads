with Ada.Unchecked_Deallocation;
with Ada.Finalization;
with Ada.Containers.Vectors;

generic
	type Element is limited private;
package GenericLists is
	type ElementRef is access Element;
	package ElementRefVectors is new Ada.Containers.Vectors(
			natural, ElementRef);

	type Ref(e: access Element) is null record
	with
		Implicit_Dereference => e;

	type List is new Ada.Finalization.Limited_Controlled with
	record
		impl: ElementRefVectors.Vector;
	end record
	with
		Variable_Indexing => Get;

	procedure Finalize(v: in out List);
	function Add(v: in out List) return Ref;
	procedure Add(v: in out List);
	function Get(v: in out List; index: natural) return Ref is
		(Ref'(e => v.impl.Element(index)));
private
	procedure Free is new Ada.Unchecked_Deallocation(Element, ElementRef);
end;
