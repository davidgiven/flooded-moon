with World.Universe;
with Scene;

package body Atmospheric_Planet is
	procedure Init(self: in out Class)
	is
	begin
		Super(self).Init;
	end;

	procedure Sample_Atmosphere(self: Object;
			xyz: vec3_t;
			camera_direction, sun_direction: vec3_t;
			sun_colour: colour_t;
			extinction, emission: out colour_t)
	is
	begin
		self.Sample_Atmosphere_P(xyz - self.location,
			camera_direction, sun_direction,
			sun_colour,
			extinction, emission);
	end;

	-- Coordinates here are *world* based.
	procedure Render_Atmosphere_For_Segment(object: Class;
			segment_start, segment_end: vec3_t;
			emission, transmittance: in out colour_t) is
		here: vec3_t;
		camera_dir, sun_dir: vec3_t;
		sunlight: colour_t;
		extinction_here, emission_here, transmittance_here: colour_t;
		segment_length: number;
	begin
		-- Sampling is done for the middle of our segment.
		here := (segment_start + segment_end) / 2.0;

		-- Calculate sunlight at this point.
		sun_dir := Normalise(World.Universe.Sun.location - here);
		sunlight := object.Sunlight_From_Point(here, sun_dir,
			World.Universe.Sun.colour,
			allow_self_shadowing => false);

		-- Sample the atmosphere here.
		camera_dir := Normalise(Scene.camera_location - here);
		Atmospheric_Planet.Object(object).Sample_Atmosphere(here,
				camera_dir, sun_dir, sunlight,
				extinction_here, emission_here);

		-- Calculate transmittance for this segment (0 means opaque,
		-- 1.0 means transparent).
		segment_length := Length(segment_end - segment_start);
		transmittance_here := exp(-extinction_here * segment_length);

		-- Calculate cumulative transmittance and emission.
		transmittance := transmittance * transmittance_here;

		-- colour_t of this pixel is the colour_t accumulated so far, plus
		-- the colour_t of the new segment attenuated 
		emission := emission +
			transmittance*emission_here*segment_length*sunlight;
	end;
end;

