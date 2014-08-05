with Ada.Text_IO;
with Calculon;

package body Simplex is
	type ivec3_t is
	record
		x, y, z: integer;
	end record;

	-- Permutation table.  The same list is repeated twice.
	perm: constant array(0..511) of integer :=
	(
		151,160,137,91,90,15,131,13,201,95,96,53,194,233,7,225,140,36,103,30,69,142,
		8,99,37,240,21,10,23,190,6,148,247,120,234,75,0,26,197,62,94,252,219,203,117,
		35,11,32,57,177,33,88,237,149,56,87,174,20,125,136,171,168,68,175,74,165,71,
		134,139,48,27,166,77,146,158,231,83,111,229,122,60,211,133,230,220,105,92,41,
		55,46,245,40,244,102,143,54,65,25,63,161,1,216,80,73,209,76,132,187,208, 89,
		18,169,200,196,135,130,116,188,159,86,164,100,109,198,173,186,3,64,52,217,226,
		250,124,123,5,202,38,147,118,126,255,82,85,212,207,206,59,227,47,16,58,17,182,
		189,28,42,223,183,170,213,119,248,152,2,44,154,163,70,221,153,101,155,167,43,
		172,9,129,22,39,253,19,98,108,110,79,113,224,232,178,185,112,104,218,246,97,
		228,251,34,242,193,238,210,144,12,191,179,162,241,81,51,145,235,249,14,239,
		107,49,192,214,31,181,199,106,157,184,84,204,176,115,121,50,45,127,4,150,254,
		138,236,205,93,222,114,67,29,24,72,243,141,128,195,78,66,215,61,156,180,

		151,160,137,91,90,15,131,13,201,95,96,53,194,233,7,225,140,36,103,30,69,142,
		8,99,37,240,21,10,23,190,6,148,247,120,234,75,0,26,197,62,94,252,219,203,117,
		35,11,32,57,177,33,88,237,149,56,87,174,20,125,136,171,168,68,175,74,165,71,
		134,139,48,27,166,77,146,158,231,83,111,229,122,60,211,133,230,220,105,92,41,
		55,46,245,40,244,102,143,54,65,25,63,161,1,216,80,73,209,76,132,187,208, 89,
		18,169,200,196,135,130,116,188,159,86,164,100,109,198,173,186,3,64,52,217,226,
		250,124,123,5,202,38,147,118,126,255,82,85,212,207,206,59,227,47,16,58,17,182,
		189,28,42,223,183,170,213,119,248,152,2,44,154,163,70,221,153,101,155,167,43,
		172,9,129,22,39,253,19,98,108,110,79,113,224,232,178,185,112,104,218,246,97,
		228,251,34,242,193,238,210,144,12,191,179,162,241,81,51,145,235,249,14,239,
		107,49,192,214,31,181,199,106,157,184,84,204,176,115,121,50,45,127,4,150,254,
		138,236,205,93,222,114,67,29,24,72,243,141,128,195,78,66,215,61,156,180
	);

	grad: constant array(0..11) of vec3_t :=
	(
		(1.0,1.0,0.0), (-1.0,1.0,0.0), (1.0,-1.0,0.0), (-1.0,-1.0,0.0),
		(1.0,0.0,1.0), (-1.0,0.0,1.0), (1.0,0.0,-1.0), (-1.0,0.0,-1.0),
		(0.0,1.0,1.0), (0.0,-1.0,1.0), (0.0,1.0,-1.0), (0.0,-1.0,-1.0)
	);

	function Fast_Floor(n: number) return integer is
		(integer(n-0.5));

	function Raw_Noise_3D(xyz: vec3_t) return number is
		s, t: number;
		i0, i1, i2, i3: ivec3_t;
		v0, v1, v2, v3: vec3_t;
		gi0, gi1, gi2, gi3: integer;
		n0, n1, n2, n3: number;

		F3: constant number := 1.0/3.0;
		G3: constant number := 1.0/6.0;

		function Calculate_Contribution(v: vec3_t; g: integer) return number is
			t: number := 0.6 - Dot(v, v);
		begin
			if (t < 0.0) then
				return 0.0;
			else
				t := t*t;
				return t*t*Dot(grad(g), v);
			end if;
		end;

	begin
		s := (xyz.x + xyz.y + xyz.z) * F3;
		i0 := (Fast_Floor(xyz.x+s), Fast_Floor(xyz.y+s), Fast_Floor(xyz.z+s));

		t := number(i0.x+i0.y+i0.z) * G3;
		v0 := xyz - (number(i0.x), number(i0.y), number(i0.z)) + t;

		if (v0.x >= v0.y) then
			if (v0.y >= v0.z) then
				-- XYZ order
				i1 := (1, 0, 0);
				i2 := (1, 1, 0);
			elsif (v0.x >= v0.z) then
				-- XZY order
				i1 := (1, 0, 0);
				i2 := (1, 0, 1);
			else
				-- ZXY order
				i1 := (0, 0, 1);
				i2 := (1, 0, 1);
			end if;
		else
			if (v0.y < v0.z) then
				-- ZYX order
				i1 := (0, 0, 1);
				i2 := (0, 1, 1);
			elsif (v0.x < v0.z) then
				-- YZX order
				i1 := (0, 1, 0);
				i2 := (0, 1, 1);
			else
				-- YXZ order
				i1 := (0, 1, 0);
				i2 := (1, 1, 0);
			end if;
		end if;
				
		-- A step of (1,0,0) in i means a step of (1-c,-c,-c) in v0,
    	-- a step of (0,1,0) in i means a step of (-c,1-c,-c) in v0, and
     	-- a step of (0,0,1) in i means a step of (-c,-c,1-c) in v0, where
     	-- c = 1/6.
		
		v1 := v0 - (number(i1.x), number(i1.y), number(i1.z)) + G3;
		v2 := v0 - (number(i2.x), number(i2.y), number(i2.z)) + 2.0*G3;
		v3 := v0 - (1.0, 1.0, 1.0) + 3.0*G3;

		-- Work out the hashed gradient indices of the four simplex corners.
		i3 := (i0.x mod 256, i0.y mod 256, i0.z mod 256);
		gi0 := perm(i3.x+     perm(i3.y+     perm(i3.z))) mod 12;
		gi1 := perm(i3.x+i1.x+perm(i3.y+i1.y+perm(i3.z+i1.z))) mod 12;
		gi2 := perm(i3.x+i2.x+perm(i3.y+i2.y+perm(i3.z+i2.z))) mod 12;
		gi3 := perm(i3.x+1+   perm(i3.y+1+   perm(i3.z+1))) mod 12;

		 -- Calculate the contribution from the four corners

		n0 := Calculate_Contribution(v0, gi0);
		n1 := Calculate_Contribution(v1, gi1);
		n2 := Calculate_Contribution(v2, gi2);
		n3 := Calculate_Contribution(v3, gi3);

		return 32.0 * (n0 + n1 + n2 + n3);
	end;

-- 3D Multi-octave Simplex noise.
--
	-- For each octave, a higher frequency/lower amplitude function will be added to the original.
	-- The higher the persistence [0-1], the more of each succeeding octave will be added.
	function Octave_Noise_3D(xyz: vec3_t;
			octaves, persistence, scale: number) return number is
		total: number := 0.0;
		frequency: number := scale;
		amplitude: number := 1.0;

		-- We have to keep track of the largest possible amplitude,
		-- because each octave adds more, and we need a value in [-1, 1].
		maxAmplitude: number := 0.0;
	begin
		for i in 1..(integer(octaves)) loop
			total := Raw_Noise_3D(xyz * frequency) * amplitude;

			frequency := frequency * 2.0;
			maxAmplitude := maxAmplitude + amplitude;
			amplitude := amplitude * persistence;
		end loop;

		return total / maxAmplitude;
	end;

begin
	Calculon.Register_Callback("noise",
			"(vector*3, real, real, real): real",
			Simplex.Octave_Noise_3D'address);
end;

