with Ada.Finalization;
with Colours;

use Colours;

package Images is
	type pixels_t is
		array(integer range <>, integer range <>) of aliased colour_t;

	type pixelstore_t(minw, maxw, minh, maxh: integer) is limited
	record
		refcount: natural := 1;
		data: pixels_t(minw..maxw, minh..maxh);
	end record;
	type pixelstore_ref is access pixelstore_t;

	type image_t is new Ada.Finalization.Controlled with
	record
		pixels: pixelstore_ref;
	end record with
		Constant_Indexing => ConstGet,
		Variable_Indexing => Get;

	function Width(img: image_t) return integer is
		(img.pixels.data'length(1));
	function Height(img: image_t) return integer is
		(img.pixels.data'length(2));

	type ColourRef(pixel: not null access colour_t) is null record with
		Implicit_Dereference => pixel;
	function Get(img: image_t; x, y: integer) return ColourRef is
		(ColourRef'(pixel => img.pixels.data(x, y)'access));

	type ConstColourRef(pixel: not null access constant colour_t) is null record with
		Implicit_Dereference => pixel;
	function ConstGet(img: image_t; x, y: integer) return ConstColourRef is
		(ConstColourRef'(pixel => img.pixels.data(x, y)'access));

	function Create(width, height: integer) return image_t;
	procedure Adjust(img: in out image_t);
	procedure Finalize(img: in out image_t);
	procedure Write(img: image_t; filename: string);
end;

