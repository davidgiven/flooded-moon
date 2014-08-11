with Ada.Text_IO;
with Config;
with Matrices;
with Vectors;
with Images;
with Colours;
with Renderer;
with Scene;
with Noise;

use Ada.Text_IO;
use Config;
use Colours;
use Images;
use Vectors;
use Matrices;

procedure TerrainMaker is
	img: image_t;
begin
	Parse_Options;
	Scene.Load(Config.Options.Scene_Filename);

	img := Renderer.Render(Config.Options.Width, Config.Options.Height);
	Write(img, Config.Options.Output_Filename);
end;


