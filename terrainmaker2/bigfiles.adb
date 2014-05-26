with Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Interfaces.C.Strings;
with System;
with System.Storage_Elements;
with Config;

use Ada.Text_IO;
use Interfaces.C.Strings;
use System;
use System.Storage_Elements;
use Config;

package body BigFiles is
	package C is
		function Open(filename: chars_ptr) return integer;
		pragma import(C, Open, "ada_bigfile_open");

		procedure Close(fd: integer);
		pragma import(C, Close, "ada_bigfile_close");

		function Error return chars_ptr;
		pragma import(C, Error, "ada_bigfile_error");

		function Size(fd: integer) return Storage_Offset;
		pragma import(C, Size, "ada_bigfile_size");

		function Map(fd: integer) return System.Address;
		pragma import(C, Map, "ada_bigfile_map");

		procedure Unmap(fd: integer; address: System.Address);
		pragma import(C, Unmap, "ada_bigfile_unmap");
	end;

	procedure Open(bf: in out BigFile; filename: string) is
		filenamew: WrappedString;
	begin
		Finalize(bf);

		WrapString(filenamew, filename);
		bf.fd := C.Open(filenamew.c);
		if (bf.fd = -1) then
			raise BigFileException with
				filename & " open: " & Value(C.Error);
		end if;

		bf.size := C.Size(bf.fd);
		bf.address := C.Map(bf.fd);
		if (bf.address = NullAddress) then
			raise BigFileException with
				filename & " map: " & Value(C.Error);
		end if;
	end;

	procedure Finalize(bf: in out BigFile) is
	begin
		if (bf.fd /= -1) then
			if (bf.address /= NullAddress) then
				C.Unmap(bf.fd, bf.address);
				bf.address := NullAddress;
			end if;

			C.Close(bf.fd);
			bf.fd := -1;
		end if;
	end;
end;

