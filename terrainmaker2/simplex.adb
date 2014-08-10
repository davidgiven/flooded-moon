with Ada.Text_IO;
with Calculon;
with Utils;

use Utils;

package body Simplex is
	-- Returns raw 3D simplex noise.
	--
	-- The value returned is in the range [-1..1].
	--
	-- Algorithm ported from GLSL here:
	-- https://github.com/ashima/webgl-noise/blob/master/src/noise3D.glsl
	function Simplex_Noise(xyz: vec3_t) return number is
		function mod289(x: vec3_t) return vec3_t is
			(x - floor(x * (1.0 / 289.0)) * 289.0);

		function mod289(x: vec4_t) return vec4_t is
			(x - floor(x * (1.0 / 289.0)) * 289.0);

		function permute(x: vec4_t) return vec4_t is
			(mod289(((x*34.0) + 1.0) * x));

		function taylorInvSqrt(r: vec4_t) return vec4_t is
			(1.79284291400159 - 0.85373472095314 * r);

		C: constant vec2_t := (1.0/6.0, 1.0/3.0);
		D: constant vec4_t := (0.0, 0.5, 1.0, 2.0);

		-- First corner
		i: vec3_t := floor(xyz + dot(xyz, C.y));
		x0: vec3_t := xyz - i + dot(i, C.x);
		
		-- Other corners
		g: vec3_t := step((x0.y, x0.z, x0.x), (x0.x, x0.y, x0.z));
		l: vec3_t := 1.0 - g;
		i1: vec3_t := min(g, (l.z, l.x, l.y));
		i2: vec3_t := max(g, (l.z, l.x, l.y));

		x1: vec3_t := x0 - i1 + C.x;
		x2: vec3_t := x0 - i2 + C.y;
		x3: vec3_t := x0 - D.y;

		-- Permutations
		ii: vec3_t := mod289(i);
		p: vec4_t := permute(permute(permute(
						 ii.z + (0.0, i1.z, i2.z, 1.0))
					   + ii.y + (0.0, i1.y, i2.y, 1.0)) 
					   + ii.x + (0.0, i1.x, i2.x, 1.0));

		-- Gradients: 7x7 points over a square, mapped onto an octahedron.
		-- The ring size 17*17 = 289 is close to a multiple of 49 (49*6 = 294)
		ns: vec3_t := (1.0/7.0) * (D.w, D.y, D.z) - (D.x, D.z, D.x);

		j: vec4_t := p - 49.0 * floor(p * ns.z * ns.z);

		xx: vec4_t := floor(j * ns.z);
		yy: vec4_t := floor(j - 7.0 * xx);
		x: vec4_t := xx*ns.x + ns.y;
		y: vec4_t := yy*ns.x + ns.y;
		h: vec4_t := 1.0 - abs(x) - abs(y);

		b0: vec4_t := (x.x, x.y, y.x, y.y);
		b1: vec4_t := (x.z, x.w, y.z, y.w);

		s0: vec4_t := floor(b0)*2.0 + 1.0;
		s1: vec4_t := floor(b1)*2.0 + 1.0;
		sh: vec4_t := -step(h, (0.0, 0.0, 0.0, 0.0));

		a0: vec4_t := (b0.x + s0.x*sh.x,
		               b0.z + s0.z*sh.x,
					   b0.y + s0.y*sh.y,
					   b0.w + s0.w*sh.y);
		a1: vec4_t := (b1.x + s1.x*sh.z,
		               b1.z + s1.z*sh.z,
					   b1.y + s1.y*sh.w,
					   b1.w + s1.w*sh.w);

		p0: vec3_t := (a0.x, a0.y, h.x);
		p1: vec3_t := (a0.z, a0.w, h.y);
		p2: vec3_t := (a1.x, a1.y, h.z);
		p3: vec3_t := (a1.z, a1.w, h.w);

		-- Normalise gradients
		norm: vec4_t := taylorInvSqrt(
				(dot(p0, p0), dot(p1, p1), dot(p2, p2), dot(p3, p3)));
		p0n: vec3_t := p0 * norm.x;
		p1n: vec3_t := p1 * norm.y;
		p2n: vec3_t := p2 * norm.z;
		p3n: vec3_t := p3 * norm.w;

		-- Mix final noise value
		m: vec4_t := 
			max(0.6 - (dot(x0, x0), dot(x1, x1), dot(x2, x2), dot(x3, x3)), 0.0);
		m2: vec4_t := m*m;

		result: number := dot(m*m, (dot(p0, x0), dot(p1, x1), 
                                dot(p2, x2), dot(p3, x3)));
	begin
		-- By observation, result varies from about -0.13 to +0.13, so scale
		-- it to a more useful -1..+1.
		return result / 0.13;
	end;

	-- 3D multi-octave noise.
	--
	-- Algorithm stolen from here:
	-- https://github.com/jdupuy/fractalTerrain/blob/master/terrain.glsl
	
	function fBm(xyz: vec3_t; lacunarity, octaves: number)
			return number is
		p: vec3_t := xyz;
		signal: number;
		value: number := 0.0;
		frequency: number := 1.0;
	begin
		for i in 1..integer(octaves) loop
			signal := Simplex_Noise(p);
			value := value + signal/frequency;
			p := p * lacunarity;
			frequency := frequency * lacunarity;
		end loop;

		return value;
	end;

	-- 3D ridged multifractal noise.
	--
	-- Algorithm stolen from here:
	-- https://github.com/jdupuy/fractalTerrain/blob/master/terrain.glsl
	
	lo: number := 100.0;
	hi: number := -100.0;
	function fBm3(xyz: vec3_t; lacunarity, octaves, offset, gain: number)
			return number is
		p: vec3_t := xyz;

		function make_noise return number is
			((offset - abs(Simplex_Noise(p))) ** 2);

		frequency: number := 1.0;
		weight: number := 1.0;
		signal: number := make_noise;
		value: number := signal;
		max_value: number := 1.0;

	begin
		for i in 1..integer(octaves) loop
			p := p * lacunarity;
			weight := clamp(signal*gain, 0.0, 1.0);
			signal := make_noise * weight;
			value := value + (signal / frequency);
			max_value := max_value + (weight / frequency);
			frequency := frequency * lacunarity;
		end loop;

		value := value / max_value;
		return value;
	end;

begin
	Calculon.Register_Callback("fBm",
			"(vector*3, real, real): real",
			Simplex.fBm'address);

	Calculon.Register_Callback("fBm3",
			"(vector*3, real, real, real, real): real",
			Simplex.fBm3'address);
end;

