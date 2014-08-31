with Ada.Text_IO;
with Ada.Strings;
with Ada.Finalization;
with Config;
with Matrices;
with Vectors;
with Colours;
with Utils;
with ConfigFiles;
with GenericLists;
with Transforms;
with BigFiles;
with System.Storage_Elements;
with World.Universe;

use Ada.Text_IO;
use Ada.Strings;
use Config;
use Colours;
use Vectors;
use Matrices;
use ConfigFiles;
use Transforms;
use BigFiles;
use System.Storage_Elements;
use Utils;

procedure Tests is
	LF: character := ASCII.LF;

	function CompareWS(s1, s2: string) return boolean is
		i1: integer := s1'first;
		i2: integer := s2'first;
		c1, c2: character;

		function isws(c: character) return boolean is
			((c = LF) or (c = ' '));
	begin
		while (i1 <= s1'last) and (i2 <= s2'last) loop
			c1 := s1(i1);
			c2 := s2(i2);

			if not isws(c1) and not isws(c2) then
				if (c1 /= c2) then
					return false;
				end if;
				i1 := i1 + 1;
				i2 := i2 + 1;
			else
				if isws(c1) then
					i1 := i1 + 1;
				end if;
				if isws(c2) then
					i2 := i2 + 1;
				end if;
			end if;
		end loop;

		-- One string has reached the end. If the other string has
		-- non-whitespace chars, it's a fail.
		while (i1 <= s1'last) loop
			if not isws(s1(i1)) then
				return false;
			end if;
			i1 := i1 + 1;
		end loop;

		while (i2 <= s2'last) loop
			if not isws(s2(i2)) then
				return false;
			end if;
			i2 := i2 + 1;
		end loop;

		return true;
	end;

	procedure Check(b: boolean; msg: string) is
	begin
		if not b then
			Put_Line(Standard_Error, "Test failed: " & msg);
			Utils.Error("Test failure");
		end if;
	end;

	procedure MatrixAndVectorSizes is
	begin
		Check(vec3_t'alignment = 4*8,
			"MatrixAndVectorSizes fail: vec3_t alignment is"
				& vec3_t'alignment & " not" & 4*8);
	end;
			
	procedure MatrixInversion is
		-- mat4_t literals can't be loaded directly into a mat4_t
		-- (the storage order is wrong). They need to be transposed
		-- first.
		m: mat4_t := Transpose(((3.0, 2.0, 4.0, 1.0),
					            (2.0,-3.0, 1.0, 1.0),
					            (1.0, 1.0, 2.0, 1.0),
					            (3.0, 2.0, 1.0, 1.0)));

		r: mat4_t := Transpose((( 5.0, 3.0, -15.0, 7.0),
		                        (-1.0,-6.0,  3.0, 4.0),
					            ( 9.0,-0.0, -0.0, -9.0),
					            (-22.0, 3.0, 39.0, 7.0))) / 27.0;
	begin
		m := Invert(m);
		Check(
			CompareWS(ToString(m), ToString(r)),
			"MatrixInversion fail; should be:" & LF & r & 
				LF & "but was:" & LF & m);
	end;

	procedure MatrixMultiplyByVector is
		m: mat4_t := Transpose(((1.0, 2.0, 3.0, 4.0),
		                        (5.0, 6.0, 7.0, 8.0),
								(9.0, 10.0, 11.0, 12.0),
								(13.0, 14.0, 15.0, 16.0)));

		v: vec3_t := (0.0, 1.0, 2.0);
		r: vec3_t := (4.5, 5.0, 5.5) / 6.0;
	begin
		v := m * v;
		Check(
			CompareWS(ToString(v), ToString(r)),
			"MatrixMultiplyByVector fail; should be:" & LF & r &
				LF & "but was:" & LF & v);
	end;

	procedure ConfigTest is
		cf: node_t := ConfigFiles.Create;
	begin
		cf.Load("testdata/testconfig.tm");

		Check(
			CompareWS(cf("value").Value, "Hello, world!"),
			"ConfigTest fail (simple value 1)");
	end;
		          
	procedure ConfigMissingTest is
		cf: node_t := ConfigFiles.Create;
	begin
		cf.Load("testdata/testconfig.tm");

		Check(
			not cf.Exists("doesnotexist"),
			"ConfigMissingTest fail (value existed!)");
	end;
		          
	procedure ListsTest is
		count: integer := 0;

		package P is
			type TestObject is new Ada.Finalization.Limited_Controlled
				with null record;
			procedure Initialize(o: in out TestObject);
			procedure Finalize(o: in out TestObject);
			procedure Method(o: in out TestObject);

			package Lists is new GenericLists(TestObject);
			subtype List is Lists.List;
		end;

		package body P is
			procedure Initialize(o: in out TestObject) is
			begin
				count := count + 1;
			end;

			procedure Finalize(o: in out TestObject) is
			begin
				count := count - 1;
			end;

			procedure Method(o: in out TestObject) is
			begin
				null;
			end;
		end;

		use P;
	begin
		declare
			v: P.List;
		begin
			for i in 1..10 loop
				v.Add;
			end loop;
			v.Add.Method;
			v(0).Method;
			v(3).Method;
		end;

		Check(
			count = 0,
			"ListsTest fail (refcount is " & count'img & ")");
	end;

	procedure MapTest is
		bf: bigfile_t;
	begin
		bf.Open("testdata/maptest.dat");

		declare
			s: string(1..integer(bf.size));
			for s'address use bf.address;
		begin
			Check(
				s = "Hello, world!",
				"MapTest fail");
		end;
	end;

	procedure ClampTest is
	begin
		Check(
			clamp(-1.0, 0.0, 1.0) = 0.0,
			"clamp -1 fail");

		Check(
			clamp(0.0, 0.0, 1.0) = 0.0,
			"clamp 0 fail");

		Check(
			clamp(0.5, 0.0, 1.0) = 0.5,
			"clamp 0.5 fail");

		Check(
			clamp(1.0, 0.0, 1.0) = 1.0,
			"clamp 1.0 fail");

		Check(
			clamp(2.0, 0.0, 1.0) = 1.0,
			"clamp 2.0 fail");
	end;

	procedure WorldTest
	is
	begin
		World.Universe.Init;
	end;

begin
	MatrixAndVectorSizes;
	MatrixInversion;
	MatrixMultiplyByVector;
	ConfigTest;
	ConfigMissingTest;
	ListsTest;
	MapTest;
	ClampTest;
	WorldTest;
end;

