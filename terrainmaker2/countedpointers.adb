with Ada.Text_IO;
with Ada.Finalization;

use Ada.Text_IO;

package body CountedPointers is
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


