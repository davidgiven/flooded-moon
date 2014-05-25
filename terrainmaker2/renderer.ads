with Config;
with Images;
with Vectors;

use Config;
use Images;
use Vectors;

package Renderer is
	function Render(width, height: integer) return Image;
end;

