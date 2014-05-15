with Ada.Sequential_IO;
with Interfaces.C;
with Config;
with Colours;
with Utils;

use Colours;
use Utils;
use Config;

package body ImageWriter is
	function Create(width, height: integer) return Image is
		img: Image(width-1, height-1);
	begin
		return img;
	end;

	procedure Write(img: Image; filename: string) is
		type byte is mod 2**8;
		type word is mod 2**16;
		package SIO is new Ada.Sequential_IO(byte);

		LF: constant character := ASCII.LF;
		fp: SIO.File_Type;

		procedure write(c: character) is
		begin
			SIO.Write(fp, byte'val(character'pos(c)));
		end;

		procedure write(b: byte) is
		begin
			SIO.Write(fp, b);
		end;

		procedure write(s: string) is
		begin
			for i in s'range loop
				write(s(i));
			end loop;
		end;

		procedure write(w: word) is
			lo: word := w mod 256;
			hi: word := w / 256;
		begin
			write(byte'val(lo));
			write(byte'val(hi));
		end;

		procedure write(n: Number) is
			val: word := word(Clamp(n, 0.0, 1.0) * 65535.0);
		begin
			write(val);
		end;
	begin
		SIO.Create(fp, SIO.Out_File, filename);
		write("P6" & LF);
		write(integer'image(img.Width));
		write(integer'image(img.Height));
		write(LF);
		write("65535" & LF);

		for y in 0..img.maxh loop
			for x in 0..img.maxw loop
				declare
					c: Colour := img.Data(x, y);
				begin
					write(c.r);
					write(c.g);
					write(c.b);
				end;
			end loop;
		end loop;

		SIO.Close(fp);
	end;
end;

