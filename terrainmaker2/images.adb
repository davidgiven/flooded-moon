with Ada.Sequential_IO;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Interfaces.C;
with Config;
with Colours;
with Utils;

use Ada.Text_IO;
use Colours;
use Utils;
use Config;

package body Images is
	function Create(width, height: integer) return image_t is
		halfw: integer := width / 2;
		halfh: integer := height / 2;
		img: image_t;
	begin
		img.pixels := new pixelstore_t(-halfw, halfw, -halfh, halfh);
		return img;
	end;

	procedure Adjust(img: in out image_t) is
	begin
		if (img.pixels /= null) then
			img.pixels.refcount := img.pixels.refcount + 1;
		end if;
	end;

	procedure Finalize(img: in out image_t) is
		procedure FreePixels is
			new Ada.Unchecked_Deallocation(pixelstore_t, pixelstore_ref);
	begin
		if (img.pixels /= null) then
			img.pixels.refcount := img.pixels.refcount - 1;
			if (img.pixels.refcount = 0) then
				FreePixels(img.pixels);
			end if;
		end if;
	end;

	procedure Write(img: image_t; filename: string) is
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
			write(byte'val(hi));
			write(byte'val(lo));
		end;

		procedure write(n: number) is
			val: word := word(Clamp(n, 0.0, 1.0) * 65535.0);
		begin
			write(val);
		end;
	begin
		Put_Line("Writing image_t to " & filename);

		SIO.Create(fp, SIO.Out_File, filename);
		write("P6" & LF);
		write(img.Width'img);
		write(img.Height'img);
		write(LF);
		write("65535" & LF);

		for y in img.pixels.data'range(2) loop
			for x in img.pixels.data'range(1) loop
				declare
					c: colour_t := img(x, y);
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

