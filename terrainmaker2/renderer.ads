with Config;
with Images;

use Config;
use Images;

package Renderer is
	function Render(width, height: integer) return Image;
end;

