with Ada.Finalization;
with Ada.Unchecked_Deallocation;

generic
	type Element is limited private;
package CountedPointers is
	type ElementRef is access Element;
	type CountRef is access natural;

	procedure Free is new Ada.Unchecked_Deallocation(Element, ElementRef);

	type Ptr(e: ElementRef; c: CountRef) is
			new Ada.Finalization.Controlled
			with null record;

	function NewPtr return Ptr is
		(Ptr'(Ada.Finalization.Controlled with
			e => new Element, c => new natural'(1)
		));

	function Get(r: Ptr) return ElementRef is (r.e);
	procedure Adjust(r: in out Ptr);
	procedure Finalize(r: in out Ptr);
end;

