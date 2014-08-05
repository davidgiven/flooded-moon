# All units are in metres.

camera:
{
	hfov: 60
	aspect: 1.777
	location: (0, -1750e3, 0)
	forward: (1, 0, 0)
	up: (0, 1, 0)
	pitch: -10
}

planets: 
{
	sun:
	{
		nominal_radius: 695500e3
		atmospheric_depth: 0
		location: (149597871e3, 0, 0)
		colour: (1, 1, 1)

		terrain_radius_func: "scripts/sun-terrain.cal"
		terrain_surface_func: "scripts/sun-surface.cal"
		atmosphere_func: "scripts/moon-atmosphere.cal"
	}

	moon:
	{
		nominal_radius: 1737e3
		atmospheric_depth: 100e3
		location: (0, 0, 0)

		transform:
		{
			tilt: 1.5424
		}

		terrain_radius_func: "scripts/moon-terrain.cal"
		terrain_surface_func: "scripts/moon-surface.cal"
		atmosphere_func: "scripts/moon-atmosphere.cal"
	}
}
