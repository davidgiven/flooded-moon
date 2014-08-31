with Utils;
with Noise;
with Config;
with Ada.Numerics;

use Utils;
use Config;
use Config.Number_Functions;
use Ada.Numerics;

package body World.Moon is
	-- Maximum height of mountains above sea level.
	max_mountain_height: constant number := 1000.0; -- metres

	-- Sample distance in the upper atmosphere.
	default_sample_distance: constant number := 10000.0; -- metres
	-- Sample distance inside mountains.
	mountain_sample_distance: constant number := 100.0; -- metres

	procedure Init(self: in out Class)
	is
	begin
		Super(self).Init;
	end;

	function Get_Sample_Distance_P(self: Class;
				xyz_rel_planet: vec3_t)
			return number
	is
		altitude: number := length(xyz_rel_planet) - self.nominal_radius;
	begin
		-- Inside mountains.
		if (altitude < max_mountain_height) then
			-- We're going to assume a maximum gradient of 1. That means
			-- that we know that there's going to be ground no further away
			-- than our immediate radar altitude, in any direction.
			declare
				radar_altitude: number := self.Get_Actual_Radius_P(xyz_rel_planet);
			begin
				return clamp(altitude - radar_altitude,
					mountain_sample_distance, max_mountain_height - altitude);
			end;
		end if;

		return clamp(altitude - max_mountain_height,
			mountain_sample_distance, default_sample_distance);
	end;

	function Get_Actual_Radius_P(self: Class;
				xyz_rel_planet: vec3_t)
			return number
	is
		-- up is a unit vector pointing away from the planet's core.
		up: vec3_t := xyz_rel_planet / length(xyz_rel_planet);

		-- xyz_rel_planet normalised to the lunar surface (i.e. at altitude 0)
		xyzn: vec3_t := up * self.nominal_radius;

		scale: constant number := 20.0e3;

		d: number := Noise.fBm3(xyzn/scale, 2.0, 5.0, 1.0, 1.0);

		function Stretch(value, min: number)
			return number
				is ((clamp(value, min, 1.0) - min) * (1.0 / (1.0 - min)));
	begin
		d := Stretch(d, 0.5);
		return self.nominal_radius + d*max_mountain_height;
	end;

	procedure Sample_Atmosphere_P(self: Class;
			xyz_rel_planet: vec3_t;
			camera_direction, sun_direction: vec3_t;
			sun_colour: colour_t;
			extinction, emission: out colour_t)
	is
		-- Okay, Rayleigh scattering. For the scattering and extinction
		-- factors:
		--
		--                       8 * pi^3 * (n^2 - 1)^2       -h
		--  betas_r(h, lambda) = ---------------------- * exp(--)
		--                            3*N*lambda^4            Hr
		--
		--  betae_r(h, lambda) = betas_r(h, lambda)
		--
		-- h: height above sea level
		-- Hr: scale height
		-- lambda: wavelength of light
		-- n: index of refraction of air
		-- N: density at sea level
		-- 
		-- And for the phase:
		-- 
		--              3
		--  P_r(mu) = ----- * (1 + mu^2)
		--            16*pi
		--
		-- mu: cos of angle between sun and camera.
		--
		-- The final emitted light and extinction factors are:
		--
		--  (input * betas_r * P_r)
		--  betae_r
		--
		-- We simplify this to:
		--
		--  input * expfactor * betas_r_without_exp * P_r
		--  expfactor * betae_r_without_exp
		--
		-- beta_r_without_exp and beta_m_without_exp end up being constants.
		--
		-- Easy, right? Ha.

		-- Arbitrary scaling factor for atmospheric thickness.
		density: constant number := 0.5;
		scale_altitude: constant number := 25.0e3; -- metres

		betas_r_without_exp: colour_t := (5.5e-6, 13.0e-6, 22.4e-6); -- metres^-1
		betae_r_without_exp: colour_t := betas_r_without_exp;
		mu: number := dot(camera_direction, sun_direction);
		altitude: number := length(xyz_rel_planet) - self.nominal_radius;

		expfactor: number := density * exp(-altitude / scale_altitude);

		phase_r: number := (3.0/16.0*PI) * (1.0 + mu*mu);
	begin
		emission := sun_colour * betas_r_without_exp*phase_r * expfactor;
		extinction := betae_r_without_exp * expfactor;
	end;

	procedure Sample_Surface_P(self: Class;
			xyz_rel_planet: vec3_t;
			camera_direction, sun_direction, surface_normal: vec3_t;
			sun_colour: colour_t;
			ambient_colour: colour_t;
			emission: out colour_t)
	is
		-- up is a unit vector pointing away from the planet's core.
		up: vec3_t := xyz_rel_planet / length(xyz_rel_planet);

		-- xyz_rel_planet normalised to the lunar surface (i.e. at altitude 0)
		xyzn: vec3_t := up * self.nominal_radius;

		c: number := clamp(dot(surface_normal, sun_direction), 0.0, 1.0);
	begin
		emission := (sun_colour + ambient_colour) * c;
	end;
end;


