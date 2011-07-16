#include "consts.inc"
#include "colors.inc"
#include "stones.inc"
#include "textures.inc"
#include "functions.inc"
#include "transforms.inc"

#declare Lunar_Sphere           = 1750.000; // enclosing diameter of model
#declare Nominal_Terrain_Radius = 1737.400; // 0 level for sea
#declare Sea_Level              = -2;
#declare Atmospheric_Depth      = 1000;
#declare Atmospheric_Scale      = 1000;

#declare Cloud_Base             = 5;
#declare Cloud_Height           = 10;

#declare Time_Of_Day            = 6;
#declare Altitude               = 5000;
#declare Latitude               = 0;
#declare Longitude              = 180;
#declare Field_Of_View          = 360;

#declare Contours               = .5;

global_settings
{
}

#default
{
	texture
	{
		finish
		{
			ambient 1.00
		}
	}
}

#include "moon.inc"
#include "animationpath.inc"

camera
{
    spherical
    angle Field_Of_View Field_Of_View/2
	location <0, 0, 0>
	up <1, 0, 0>
	look_at <0, 1, 0>
	rotate <0, 0, 180>
    rotate <-Latitude, 0, -Longitude>
}

background
{
	rgb <0, 0, 0>
}

/*
box
{
    <-0.5, -0.5, -0.5>, <0.5, 0.5, 0.5>
    pigment { colour Red }
    Place_On_Surface(-3.25, 19.9, 0, 0, 0)
}
*/

#local sealevel = Nominal_Terrain_Radius + Sea_Level;
#local km = 1 / Lunar_Sphere;
   
#local Distance_Function =
    function(x, y, z)
    {
        sqrt(x*x + y*y + z*z) * km
    };
    
#local Slopemap_Shading_Pigment =
    pigment
    {
        slope
        {
            point_at <0, 0, 0>, 0.8, 1.0
        }
        pigment_map
        {
            [0.0 colour rgbt<1, 1, 1, 0.0>]
            [1.0 colour rgbt<0, 0, 0, 0.0>]
        }
    }

object
{
    Moon_Object

    pigment { Slopemap_Shading_Pigment }
}    
