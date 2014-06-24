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
with Calculon;
with BigFiles;
with System.Storage_Elements;

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
		Check(Vector2'length = 2,
			"MatrixAndVectorSizes fail: Vector2 is not of length 2");
		Check(Vector3'length = 3,
			"MatrixAndVectorSizes fail: Vector3 is not of length 3");
		Check(Vector4'length = 4,
			"MatrixAndVectorSizes fail: Vector4 is not of length 4");
		Check(Matrix2'length(1) = 2,
			"MatrixAndVectorSizes fail: Matrix2 is not of length 2");
		Check(Matrix3'length(1) = 3,
			"MatrixAndVectorSizes fail: Matrix3 is not of length 3");
		Check(Matrix4'length(1) = 4,
			"MatrixAndVectorSizes fail: Matrix4 is not of length 4");
	end;
			
	procedure MatrixInversion is
		m: Matrix3 := ((3.0, 2.0, 4.0),
					   (2.0,-3.0, 1.0),
					   (1.0, 1.0, 2.0));
	begin
		m := Invert(m);
		Check(
			CompareWS(ToString(m),
				"( 1.00000000000000E+00, 0.00000000000000E+00,-2.00000000000000E+00 4.28571428571428E-01,-2.85714285714286E-01,-7.14285714285714E-01 -7.14285714285714E-01, 1.42857142857143E-01, 1.85714285714286E+00)"),
			"MatrixInversion fail:" & LF & ToString(m));
	end;

	procedure MatrixMultiplyByVector is
		m: Matrix3 := ((1.0, 2.0, 3.0),
		               (5.0, 6.0, 7.0),
					   (9.0, 10.0, 11.0));
		v: Vector3 := (1.0, 2.0, 3.0);
	begin
		v := m * v;
		Check(
			CompareWS(ToString(v),
				"( 1.40000000000000E+01, 3.80000000000000E+01, 6.20000000000000E+01)"),
			"MatrixMultiplyByVector fail:" & LF & ToString(v));
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

	procedure CalculonTest is
		type TestFunc is access procedure(i: number; r: out number);
		package TestCalculon is new Calculon(TestFunc);
		use TestCalculon;
		func: TestCalculon.Func;
		n: number;
	begin
		Initialise(func, "let i=i+1 in return", "(i: real): (i: real)");
		func.Call.all(1.0, n);
		Check(
			n = 2.0,
			"CalculonTest fail");
	end;

	procedure CalculonVectorTest is
		type TestFunc is access procedure(i: number; r1, r2: out Vector3);
		package TestCalculon is new Calculon(TestFunc);
		use TestCalculon;
		func: TestCalculon.Func;
		n1, n2: Vector3;
	begin
		Initialise(func, "let r1=[1,1,1] in let r2=[2,2,2] in return",
			"(i: real): (r1: vector*3, r2: vector*3)");
		func.Call.all(1.0, n1, n2);
		Check(
			(n1(0) = 1.0) and (n1(1) = 1.0) and (n1(2) = 1.0) and
			(n2(0) = 2.0) and (n2(1) = 2.0) and (n2(2) = 2.0),
			"CalculonVectorTest fail; n1=" & ToString(n1) & " n2=" & ToString(n2));
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

begin
	MatrixAndVectorSizes;
	MatrixInversion;
	MatrixMultiplyByVector;
	ConfigTest;
	ConfigMissingTest;
	ListsTest;
	CalculonTest;
	CalculonVectorTest;
	MapTest;
end;



