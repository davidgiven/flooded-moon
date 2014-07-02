package body Matrices is
	function "*" (m: mat4_t; n: number) return mat4_t is
	begin
		return r: mat4_t do
			for x in mat4_index_t loop
				for y in mat4_index_t loop
					r(x, y) := m(x, y) * n;
				end loop;
			end loop;
		end return;
	end;

	function "*" (m: mat4_t; v: vec3_t) return vec3_t is
		x: number := m(0,0)*v.x + m(0,1)*v.y + m(0,2)*v.z + m(0,3);
		y: number := m(1,0)*v.x + m(1,1)*v.y + m(1,2)*v.z + m(1,3);
		z: number := m(2,0)*v.x + m(2,1)*v.y + m(2,2)*v.z + m(2,3);
		w: number := m(3,0)*v.x + m(3,1)*v.y + m(3,2)*v.z + m(3,3);
	begin
		return (x/w, y/w, z/w);
	end;

	function "*" (m: mat4_t; o: mat4_t) return mat4_t is
		sum: number;
	begin
		return r: mat4_t do
			for i in mat4_index_t loop
				for j in mat4_index_t loop
					sum := 0.0;
					for k in mat4_index_t loop
						sum := sum + m(i,k)*o(k,j);
					end loop;
					r(i,j) := sum;
				end loop;
			end loop;
		end return;
	end;

	function Invert(m: mat4_t) return mat4_t is
		s0: number := m(0,0) * m(1,1) - m(1,0) * m(0,1);
		s1: number := m(0,0) * m(1,2) - m(1,0) * m(0,2);
		s2: number := m(0,0) * m(1,3) - m(1,0) * m(0,3);
		s3: number := m(0,1) * m(1,2) - m(1,1) * m(0,2);
		s4: number := m(0,1) * m(1,3) - m(1,1) * m(0,3);
		s5: number := m(0,2) * m(1,3) - m(1,2) * m(0,3);

		c5: number := m(2,2) * m(3,3) - m(3,2) * m(2,3);
		c4: number := m(2,1) * m(3,3) - m(3,1) * m(2,3);
		c3: number := m(2,1) * m(3,2) - m(3,1) * m(2,2);
		c2: number := m(2,0) * m(3,3) - m(3,0) * m(2,3);
		c1: number := m(2,0) * m(3,2) - m(3,0) * m(2,2);
		c0: number := m(2,0) * m(3,1) - m(3,0) * m(2,1);

		idet: number := 1.0 / (s0*c5 - s1*c4 + s2*c3 + s3*c2 - s4*c1 + s5*c0);
	begin
		return (
			(
				(m(1,1) * c5 - m(1,2) * c4 + m(1,3) * c3) * idet,
				(-m(0,1) * c5 + m(0,2) * c4 - m(0,3) * c3) * idet,
				(m(3,1) * s5 - m(3,2) * s4 + m(3,3) * s3) * idet,
				(-m(2,1) * s5 + m(2,2) * s4 - m(2,3) * s3) * idet
			), (
				(-m(1,0) * c5 + m(1,2) * c2 - m(1,3) * c1) * idet,
				(m(0,0) * c5 - m(0,2) * c2 + m(0,3) * c1) * idet,
				(-m(3,0) * s5 + m(3,2) * s2 - m(3,3) * s1) * idet,
				(m(2,0) * s5 - m(2,2) * s2 + m(2,3) * s1) * idet
			), (
				(m(1,0) * c4 - m(1,1) * c2 + m(1,3) * c0) * idet,
				(-m(0,0) * c4 + m(0,1) * c2 - m(0,3) * c0) * idet,
				(m(3,0) * s4 - m(3,1) * s2 + m(3,3) * s0) * idet,
				(-m(2,0) * s4 + m(2,1) * s2 - m(2,3) * s0) * idet
			), (
				(-m(1,0) * c3 + m(1,1) * c1 - m(1,2) * c0) * idet,
				(m(0,0) * c3 - m(0,1) * c1 + m(0,2) * c0) * idet,
				(-m(3,0) * s3 + m(3,1) * s1 - m(3,2) * s0) * idet,
				(m(2,0) * s3 - m(2,1) * s1 + m(2,2) * s0) * idet
			));
	end;

	function Transpose(m: mat4_t) return mat4_t is
	begin
		return r: mat4_t do
			for x in mat4_index_t loop
				for y in mat4_index_t loop
					r(x,y) := m(y,x);
				end loop;
			end loop;
		end return;
	end;

	function ToString(m: mat4_t) return string is
		function cols(x, y: mat4_index_t) return string is
			(m(x, y)'img &
				(if (x < mat4_index_t'last) then (',' & cols(x+1, y)) else ""));

		function rows(y: mat4_index_t) return string is
			(' ' & cols(mat4_index_t'first, y) & LF &
				(if (y < mat4_index_t'last) then rows(y+1) else ""));
	begin
		return '(' & LF & rows(mat4_index_t'first) & ')';
	end;
end;

