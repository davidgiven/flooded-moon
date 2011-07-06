#include "consts.inc"
#include "colors.inc"
#include "stones.inc"
#include "textures.inc"
#include "functions.inc"

#declare Lunar_Sphere           = 1750.000; // enclosing diameter of model
#declare Nominal_Terrain_Radius = 1737.400; // 0 level for sea
#declare Sea_Level              = -1;
#declare Geoid_Altitude_Delta   = 0;
#declare Atmospheric_Depth      = 100;
#declare Atmospheric_Scale      = 50;

#declare Cloud_Base             = 5;
#declare Cloud_Height           = 10;

#declare Time_Of_Day            = 6; //24*clock;
#declare Altitude               = 0.002;
#declare Latitude               = -43.31; //20;
#declare Longitude              = -11.36; //-6.8;
#declare Pitch                  = 0;
#declare Bearing                = 0; //120;
#declare Displacement           = 0;
#declare Field_Of_View          = 20;

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

camera
{
	angle Field_Of_View
	location <0, 0, 0>
	sky <0, -1, 0>
	look_at <0, 0, 1>
	translate Displacement
	rotate x * Pitch
	rotate y * -Bearing
	translate <0, -(Nominal_Terrain_Radius + Sea_Level + Altitude), 0> 
	rotate <-Latitude, 0, -Longitude>
//	rotate <-90, -0, 0>
//	translate <1, -4000, 0>	
}

background
{
	rgb <0, 0, 0>
}

#include "moon.inc"
object { Moon }

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
