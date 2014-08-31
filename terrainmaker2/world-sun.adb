package body World.Sun is
	procedure Init(self: in out Class)
	is
	begin
		Super(self).Init;
	end;

	procedure Sample_Surface_P(self: Class;
			xyz_rel_planet: vec3_t;
			camera_direction, sun_direction, surface_normal: vec3_t;
			sun_colour: colour_t;
			ambient_colour: colour_t;
			emission: out colour_t)
	is
	begin
		emission := (1.0, 1.0, 1.0);
	end;
end;

