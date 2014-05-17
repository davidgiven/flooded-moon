with Ada.Text_IO;
with Config;
with Matrices;
with Vectors;
with ImageWriter;
with Colours;

use Ada.Text_IO;
use Config;
use Colours;
use Vectors;
use all type Vectors.Vector3;
use Matrices;
use all type Matrices.Matrix3;

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

	m: Matrix3 := ((1.0, 1.0, 1.0),
	               (1.0, 1.0, 1.0),
				   (1.0, 1.0, 1.0));
begin
	m := Invert(m);
	Put_Line(ToString(m));

	ParseOptions;
	Render;
end;


