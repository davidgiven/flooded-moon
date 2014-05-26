with Ada.Finalization;
with System.Storage_Elements;

use System.Storage_Elements;

package BigFiles is
	BigFileException: exception;
	NullAddress: constant System.Address := To_Address(0);

	type BigFile is new Ada.Finalization.Limited_Controlled with
	record
		fd: integer := -1;
		address: System.Address := NullAddress;
		size: Storage_Offset;
	end record;

	procedure Open(bf: in out BigFile; filename: string);
	procedure Finalize(bf: in out BigFile);
end;

