with Config;
with Vectors;

use Config;
use Config.Number_Functions;
use Vectors;

package Simplex is
	function Raw_Noise_3D(xyz: vec3_t) return number;
	pragma convention(C, Raw_Noise_3D);

	function Octave_Noise_3D(xyz: vec3_t;
			octaves, persistence, scale: number) return number;
	pragma convention(C, Octave_Noise_3D);
end;

