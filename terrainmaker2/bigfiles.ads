with Ada.Finalization;
with System;
with System.Storage_Elements;

use System;
use System.Storage_Elements;

package BigFiles is
	bigfile_exception: exception;

	type bigfile_t is new Ada.Finalization.Limited_Controlled with
	record
		fd: integer := -1;
		address: System.Address := Null_Address;
		size: Storage_Offset;
	end record;

	procedure Open(bf: in out bigfile_t; filename: string);
	procedure Finalize(bf: in out bigfile_t);
end;

