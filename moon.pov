#include "consts.inc"
#include "colors.inc"
#include "stones.inc"
#include "textures.inc"
#include "functions.inc"

#declare Lunar_Sphere           = 1750.000; // enclosing diameter of model
#declare Nominal_Terrain_Radius = 1737.400; // 0 level for sea
#declare Sea_Level              = -2;
#declare Atmospheric_Depth      = 100;
#declare Atmospheric_Scale      = 100;

#declare Cloud_Base             = 5;
#declare Cloud_Height           = 10;

#declare Time_Of_Day            = 6;
#declare Altitude               = 2;
#declare Latitude               = 19.9;
#declare Longitude              = -3.35;
#declare Pitch                  = 0;
#declare Bearing                = 45;
#declare Displacement           = 0;
#declare Field_Of_View          = 60;

#declare Contours               = .1;

global_settings
{
}

#default
{
	texture
	{
		finish
		{
			ambient 0.00
		}
	}
}

#include "moon.inc"

camera
{
    angle Field_Of_View
    location <0, 0, 0>
    sky <0, -1, 0>
    look_at <0, 0, 1>
    translate Displacement
    Place_On_Surface(Longitude, Latitude, Bearing, Pitch, Altitude)
}

background
{
    rgb <0, 0, 0>
}

object { Moon }

//box
//{
//    <-0.5, -0.5, -0.5>, <0.5, 0.5, 0.5>
//    pigment { colour Red }
//    Place_On_Surface(-3.25, 19.9, 0, 0, 0)
//}

light_source
{
	<0, 0, 0>
	colour rgb <1, 1, 1>
	looks_like
	{
		sphere
		{
			<0, 0, 0>, 1000
			pigment
			{
				colour rgb <1.0, 1.0, 1.0>
			}
			finish
			{
				ambient 1
			}
		}
	}

	translate <-100000, 0, 0>
	rotate z*(Time_Of_Day*360/24)
}
