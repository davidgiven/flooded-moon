with Colours;

use Colours;

package ImageWriter is
	type PixelStore is array(integer range <>,
		integer range <>) of Colour;

	type Image(maxw, maxh: integer) is record
		Width: integer := maxw + 1;
		Height: integer := maxh + 1;
		Data: PixelStore(0..maxw, 0..maxh);
	end record;

	function Create(width, height: integer) return Image;
	procedure Write(img: Image);
end;

