with Vectors;
with Config;
with Rays;
with Colours;

use Vectors;
use Config;
use Config.Number_Functions;
use Rays;
use Colours;

package Planet is
	type Class is abstract tagged record
		-- Assigned properties
		location: vec3_t := (0.0, 0.0, 0.0);
		nominal_radius: number := 0.0;
		atmospheric_depth: number := 0.0;

		-- Calculated properties
		bounding_radius: number;
	end record;

	subtype Object is Class'class;

	procedure Init(self: in out Class);

	-- Methods which are intended to be overridden by implementations of each
	-- kind of body. _P methods take planet coordinates. Everything else takes
	-- world coordinates.
	
	-- Returns the radius of the planet at the point directly under the
	-- coordinate given (planet coordinates).
	function Get_Actual_Radius_P(self: Class;
				xyz_rel_planet: vec3_t)
			return number;

	-- Returns the colour of the surface at the point directly under the
	-- coordinate given (planet coordinates).
	procedure Sample_Surface_P(self: Class;
			xyz_rel_planet: vec3_t;
			camera_direction, sun_direction, surface_normal: vec3_t;
			sun_colour: colour_t;
			ambient_colour: colour_t;
			emission: out colour_t) is abstract;

	-- Returns the sample distance at a point near the planet (planet
	-- coordinates).
	function Get_Sample_Distance_P(self: Class;
				xyz_rel_planet: vec3_t)
			return number;

	-- Calculate the emission and transmittance for a segment of atmosphere.
	procedure Render_Atmosphere_For_Segment(object: Class;
			segment_start, segment_end: vec3_t;
			emission, transmittance: in out colour_t);

	-- Non-overridable. These do the actual work.

	-- Tests to see whether the ray intersects this planet. Returns
	-- the entry and exit points.
	function Test_Intersection(self: Object;
				ray: ray_t;
				ray_entry, ray_exit: in out vec3_t;
				include_atmosphere: boolean := true)
			return boolean;
	
	-- Returns the radius of the planet at the point directly under the
	-- coordinate given (world coordinates).
	function Get_Actual_Radius(self: Object;
				xyz: vec3_t)
			return number is
				(self.Get_Actual_Radius_P(xyz - self.location));

	-- Returns the colour of the surface at the point directly under the
	-- coordinate given (world coordinates).
	procedure Sample_Surface(self: Object;
			xyz: vec3_t;
			camera_direction, sun_direction, surface_normal: vec3_t;
			sun_colour: colour_t;
			ambient_colour: colour_t;
			emission: out colour_t);

	-- Returns the sample distance at a point near the planet (world
	-- coordinates).
	function Get_Sample_Distance(self: Object;
				xyz: vec3_t)
			return number is
				(self.Get_Sample_Distance_P(xyz - self.location));

	-- Returns the coordinates of the point on the surface directly under
	-- the coordinate given (planet coordinates).
	function Normalise_To_Surface_P(self: Object;
				xyz_rel_planet: vec3_t)
			return vec3_t;

	-- Returns the coordinates of the point on the surface directly under
	-- the coordinate given (world coordinates).
	function Normalise_To_Surface(self: Object;
				xyz: vec3_t)
			return vec3_t is
				(self.Normalise_To_Surface_P(xyz - self.location));

	-- Returns the normal vector of the point on the surface directly under
	-- the coordinate given (world coordinates).
	function Get_Surface_Normal(self: Object;
				xyz: vec3_t)
			return vec3_t;

	-- Tests whether the supplied point is under the terrain.
	function Is_Point_Underground(self: Object;
				xyz: vec3_t)
			return boolean;

	-- Given a segment, trims it at the point where it intersects with
	-- the terrain. Returns true if the segment actually meets the
	-- terrain, false otherwise.
	function Find_Intersection_With_Terrain(self: Object;
			segment_start: vec3_t; segment_end: in out vec3_t)
				return boolean;

	-- Calculate the amount of sunlight visible from a particular point
	-- near the planet.
	function Sunlight_From_Point(self: Object;
			here: vec3_t; sun_dir: vec3_t; sunlight: colour_t;
			allow_self_shadowing: boolean := false) return colour_t;

	-- Calculates the lit colour of a piece of terrain at the point
	-- given.
	procedure Light_Terrain(self: Object;
			here: vec3_t;
			emission, transmittance: in out colour_t);

	-- Updates the emission and transmittance given a ray passing
	-- through a planet's sphere of influence.
	procedure Render_Planet(self: Object;
			ray_start, ray_end: vec3_t;
			emission, transmittance: in out colour_t);

	-- Simplified version of Render_Planet used for calculating lights.
	-- Considers only transmittance.
	function Compute_Sunlight(self: Object;
			ray_start, ray_end: vec3_t)
			return colour_t;
end;
