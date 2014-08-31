with Config;
with Vectors;

use Config;
use Config.Number_Functions;
use Vectors;

package Noise is
	function Simplex_Noise(xyz: vec3_t) return number;

	function fBm(xyz: vec3_t; lacunarity, octaves: number)
			return number;

	function fBm3(xyz: vec3_t; lacunarity, octaves, offset, gain: number)
		return number;
end;

