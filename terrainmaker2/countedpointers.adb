with Ada.Text_IO;
with Ada.Finalization;

use Ada.Text_IO;

package body CountedPointers is
	function NewPtr(object: ElementRef) return Ptr is
		c: CountRef := new integer'(1);
		r: Ptr(object, c);
	begin
		return r;
	end;

	procedure Adjust(r: in out Ptr) is
	begin
		r.c.all := r.c.all + 1;
	end;

	procedure Finalize(r: in out Ptr) is
	begin
		r.c.all := r.c.all - 1;
		if (r.c.all = 0) then
			declare
				p: ElementRef := r.e;
			begin
				Free(p);
			end;
		end if;
	end;
end;


