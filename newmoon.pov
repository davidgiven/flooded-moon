#version 3.7;
#include "consts.inc"
#include "colors.inc"
#include "stones.inc"
#include "textures.inc"
#include "functions.inc"
#include "transforms.inc"
#include "rad_def.inc"

#declare km                     = 1;
#declare Lunar_Sphere           = 1750.000 * km; // enclosing diameter of terrain object
#declare Nominal_Terrain_Radius = 1737.400 * km;
#declare Atmosphere_Base        = -2 * km;
#declare Atmospheric_Depth      = 100 * km;
#declare Atmospheric_Scale      = 30 * km;

global_settings
{
	assumed_gamma 1.0

	/*
	radiosity
	{
		Rad_Settings(Radiosity_Normal, off, on)
	}
	*/
}

#default
{
	texture
	{
		finish
		{
			ambient 0
		}
	}
}

#include "/tmp/camera.inc"

camera
{
    angle CameraFieldOfView
	up z
	right -x*image_width/image_height
	scale km

	location CameraLocation
	sky CameraSky
	look_at CameraLookAt
}

background
{
    rgb <0, 0, 0>
}

// =======================================================================
//                                 MOON   
// =======================================================================

#include "newmoon.inc"

object { Moon_Object }

// ==========================================================================
//                                  EARTH
// ==========================================================================

#include "earth.inc"

object
{
	Earth
	scale km

	translate <0, -384000*km, 0>
	rotate x*5.14
}

light_source
{
	<0, -300000*km, 0>
	color <0.8, 0.8, 1.0>*0.2
	spotlight
	point_at <0, 0, 0>

	rotate x*5.14
}

// =======================================================================
//                                  SUN   
// =======================================================================

#local Sun_Closeness = 30;
#local Sun_Brightness = 10;
light_source
{
	<0, 0, 0>
	color White

	looks_like
	{
		sphere
		{
			<0, 0, 0>, 695500*km/Sun_Closeness

			texture
			{
				pigment
				{
					White
				}
				finish
				{
					emission Sun_Brightness
					ambient 1
				}
			}
		}
	}

	translate <-149600000*km/Sun_Closeness, 0, 0>
	rotate z*(Time_Of_Day*360/24)
}

// ==========================================================================
//                                   SKY
// ==========================================================================

sky_sphere
{
	pigment
	{
		#if (CameraFieldOfView < 50)
			color rgb <0, 0, 0>
		#else
			image_map
			{
				png "nightsky/phot-32a-09-fullres.png"
				map_type 1
				interpolate 4
				once
			}
			rotate 90*x
		#end
	}
	emission 0.2
}


