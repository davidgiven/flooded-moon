with Ada.Text_IO;

use Ada.Text_IO;

package body GenericLists is
	procedure Finalize(v: in out List) is
	begin
		for e of v.impl loop
			Free(e);
		end loop;
	end;

	function Add(v: in out List) return Ref is
		e: ElementRef := new Element;
	begin
		v.impl.Append(e);
		return Ref'(e => e);
	end;

	procedure Add(v: in out List) is
		r: Ref := v.Add;
	begin
		null;
	end;
end;

