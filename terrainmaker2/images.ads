with Ada.Finalization;
with Colours;

use Colours;

package Images is
	type Pixels is array(integer range <>, integer range <>) of aliased Colour;

	type PixelStore(maxw, maxh: integer) is limited
	record
		refcount: natural := 1;
		data: Pixels(0..maxw, 0..maxh);
	end record;
	type PixelStoreRef is access PixelStore;

	type Image(maxw, maxh: integer) is new Ada.Finalization.Controlled with
	record
		pixels: PixelStoreRef;
	end record with
		Constant_Indexing => ConstGet,
		Variable_Indexing => Get;

	function Width(img: Image) return integer is
		(img.pixels.maxw + 1);
	function Height(img: Image) return integer is
		(img.pixels.maxh + 1);

	type ColourRef(pixel: not null access Colour) is null record with
		Implicit_Dereference => pixel;
	function Get(img: Image; x, y: integer) return ColourRef is
		(ColourRef'(pixel => img.pixels.data(x, y)'access));

	type ConstColourRef(pixel: not null access constant Colour) is null record with
		Implicit_Dereference => pixel;
	function ConstGet(img: Image; x, y: integer) return ConstColourRef is
		(ConstColourRef'(pixel => img.pixels.data(x, y)'access));

	function Create(width, height: integer) return Image;
	procedure Adjust(img: in out Image);
	procedure Finalize(img: in out Image);
	procedure Write(img: Image; filename: string);
end;

