with Config;
with Vectors;

use Config;
use Config.Number_Functions;
use Vectors;

package Simplex is
	function Simplex_Noise(xyz: vec3_t) return number;
	pragma convention(C, Simplex_Noise);

	function fBm(xyz: vec3_t; lacunarity, octaves: number)
			return number;
	pragma convention(C, fBm);

	function fBm3(xyz: vec3_t; lacunarity, octaves, offset, gain: number)
		return number;
	pragma convention(C, fBm3);
end;

