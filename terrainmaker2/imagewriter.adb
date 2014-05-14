with Colours;

use Colours;

package body ImageWriter is
	function Create(width, height: integer) return Image is
		img: Image(width-1, height-1);
	begin
		return img;
	end;

	procedure Write(img: Image) is
	begin
		null;
	end;
end;

