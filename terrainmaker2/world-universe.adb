with World.Sun;
with World.Moon;

package body World.Universe is
	function Make_Sun return access World.Sun.Object
	is
		s: access World.Sun.Object;
	begin
		s := World.Sun.Create;
		s.colour := (1.0, 1.0, 1.0);
		s.location := (149597871.0e3, 0.0, 0.0);
		s.nominal_radius := 695500.0e3;
		return s;
	end;
	
	function Make_Moon return access World.Moon.Object
	is
		p: access World.Moon.Object;
	begin
		p := World.Moon.Create;
		p.location := (0.0, 0.0, 0.0);
		p.nominal_radius := 1737.0e3;
		p.atmospheric_depth := 100.0e3;
		return p;
	end;
	
	procedure Init
	is
	begin
		Sun := Make_Sun;
		Bodies := new Planet_Array'(
			0 => Planet_Ref(Make_Moon)
		);

		Sun.Init;
		for object of Bodies.all loop
			object.Init;
		end loop;
	end;
end;

