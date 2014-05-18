with Ada.Text_IO;
with Config;
with Matrices;
with Vectors;
with Images;
with Colours;
with Renderer;

use Ada.Text_IO;
use Config;
use Colours;
use Images;
use Vectors;
use all type Vectors.Vector3;
use Matrices;
use all type Matrices.Matrix3;

procedure TerrainMaker is
	procedure Render is
		img: Image := Renderer.Render;
	begin
		Write(img, Config.Options.Output_Filename);
	end;
begin
	ParseOptions;
	Render;
end;


