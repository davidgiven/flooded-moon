with Ada.Text_IO;
with Config;
with Matrices;
with ImageWriter;
with Colours;

use Ada.Text_IO;
use Config;
use Colours;

procedure TerrainMaker is
	procedure Render is
		img: ImageWriter.Image := ImageWriter.Create(
			Config.Options.Width, Config.Options.Height);
	begin
		img(0, 0) := RGB(1.0, 0.0, 0.0);
		img(1, 0) := RGB(1.0, 1.0, 0.0);
		img(2, 0) := RGB(1.0, 1.0, 1.0);
		img(3, 0) := RGB(0.0, 1.0, 1.0);
		img(4, 0) := RGB(0.0, 0.0, 1.0);
		ImageWriter.Write(img, Config.Options.Output_Filename);
	end;
begin
	ParseOptions;
	Render;
end;


