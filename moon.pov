#version 3.7;
#include "consts.inc"
#include "colors.inc"
#include "stones.inc"
#include "textures.inc"
#include "functions.inc"
#include "transforms.inc"
#include "rad_def.inc"

#declare Lunar_Sphere           = 1750.000; // enclosing diameter of model
#declare Nominal_Terrain_Radius = 1737.400; // 0 level for sea
#declare Sea_Level              = -6.55;//-7.65;
#declare Atmospheric_Depth      = 60;
#declare Atmospheric_Scale      = 60;

#declare Cloud_Base             = 5;
#declare Cloud_Height           = 10;

#declare Time_Of_Day            = 15;
#declare Altitude               = 0.3;
#declare Latitude               = -7.80;
#declare Longitude              = 86.34;
#declare Pitch                  = -10;
#declare Bearing                = -95;
#declare Displacement           = 0;
#declare Field_Of_View          = 50;

#declare Contours               = .1;

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
			ambient 0.0
		}
	}
}

#include "moon.inc"
#include "earth.inc"

camera
{
    angle Field_Of_View
    location <0, 0, 0>
    sky <0, -1, 0>
    look_at <0, 0, 1>

    Place_On_Surface(Longitude, Latitude, Bearing, Pitch, Altitude)
	translate Displacement * <-1, 1, 1>
}

sky_sphere
{
	pigment
	{
		image_map
		{
			png "nightsky/phot-32a-09-fullres.png"
			map_type 1
			interpolate 4
			once
		}
		rotate 90*x
	}
	emission 0.2
}

object { Moon }

object
{
	Earth

	translate <0, -384000, 0>
	rotate x*5.14
}

light_source
{
	<0, -300000, 0>
	color <0.8, 0.8, 1.0>*0.1
	spotlight
	point_at <0, 0, 0>

	rotate x*5.14
}

//box
//{
//    <-0.5, -0.5, -0.5>, <0.5, 0.5, 0.5>
//    pigment { colour Red }
//    Place_On_Surface(-3.25, 19.9, 0, 0, 0)
//}

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
			<0, 0, 0>, 695500/Sun_Closeness

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

	translate <-149600000/Sun_Closeness, 0, 0>
	rotate z*(Time_Of_Day*360/24)
}

