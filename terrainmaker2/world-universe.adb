with World.Sun;
with World.Moon;

package body World.Universe is
	bodies_array: aliased Array_Of_Bodies := (
		-- The sun must always go first.
		
		World.Sun.Create
			.Set_Location((149597871.0e3, 0.0, 0.0))
			.Set_Radius(695500.0e3),

		World.Moon.Create
			.Set_Atmospheric_Depth(100.0e3)
			.Set_Location((0.0, 0.0, 0.0))
			.Set_Radius(1737.0e3)
	);

	procedure Init
	is
	begin
		Bodies := bodies_array'access;

		for p of bodies_array loop
			p.Init;
		end loop;
	end;
end;

