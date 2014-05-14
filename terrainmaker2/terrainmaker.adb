with Ada.Text_IO;
with Config;
with Matrices;
with ImageWriter;
with Colours;

use Ada.Text_IO;
use Config;
use Colours;

procedure TerrainMaker is
	img: ImageWriter.Image := ImageWriter.Create(
		Config.Options.Width, Config.Options.Height);
begin
	img.Data(0, 0) := RGB(0.0, 0.0, 0.0);
	
end;

