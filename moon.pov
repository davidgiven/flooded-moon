#include "consts.inc"
#include "colors.inc"
#include "stones.inc"
#include "textures.inc"
#include "functions.inc"
#include "transforms.inc"

#declare Lunar_Sphere           = 1750.000; // enclosing diameter of model
#declare Nominal_Terrain_Radius = 1737.400; // 0 level for sea
#declare Sea_Level              = -7.65;
#declare Atmospheric_Depth      = 100;
#declare Atmospheric_Scale      = 30;

#declare Cloud_Base             = 5;
#declare Cloud_Height           = 10;

#declare Time_Of_Day            = 4;
#declare Altitude               = 5;
#declare Latitude               = 20;
#declare Longitude              = -3.5;
#declare Pitch                  = -10;
#declare Bearing                = 40;
#declare Displacement           = 0;
#declare Field_Of_View          = 50;

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
#include "animationpath.inc"

camera
{
    angle Field_Of_View
    location <0, 0, 0>
    sky <0, -1, 0>
    look_at <0, 0, 1>

/*    
    Place_Camera(
        spline
        {
            natural_spline
            0.00, <0, 0>
            0.01, <0, 0>
            0.10, <0, 10>
            0.20, <-20, 30>
            0.30, <-20, 35>
            0.40, <55, 40>
            0.60, <60, 50>
            0.70, <55, 45>
            0.80, <0, 50>
            0.90, <-20, 30>
            0.99, <0, 0>
            1.00, <0, 0>
        }
    )
    */
    
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
