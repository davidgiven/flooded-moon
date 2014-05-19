with Ada.Text_IO;
with Config;
with Matrices;
with Vectors;
with Images;
with Colours;
with Renderer;
with Scene;

use Ada.Text_IO;
use Config;
use Colours;
use Images;
use Vectors;
use all type Vectors.Vector3;
use Matrices;
use all type Matrices.Matrix3;

procedure TerrainMaker is
	img: Image;
begin
	ParseOptions;
	Scene.Load(Config.Options.Scene_Filename);

	img := Renderer.Render(Config.Options.Width, Config.Options.Height);
	Write(img, Config.Options.Output_Filename);
end;


