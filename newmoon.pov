#version 3.7;
#include "consts.inc"
#include "colors.inc"
#include "stones.inc"
#include "textures.inc"
#include "functions.inc"
#include "transforms.inc"

#declare km                     = 1;
#declare Lunar_Sphere           = 1750.000 * km; // enclosing diameter of terrain object
#declare Nominal_Terrain_Radius = 1737.400 * km;
#declare Atmosphere_Base        = -2 * km;
#declare Atmospheric_Depth      = 100 * km;
#declare Atmospheric_Scale      = 30 * km;
#declare Time_Of_Day            = 5.5 + 12;

global_settings
{
	assumed_gamma 1.0
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
    angle 50
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

#declare Terrain_Mesh =
	object
	{
		mesh2
		{
			#include "/tmp/moon.inc"
		}

		scale km
	}

#declare Terrain_Object =
	object
	{
		mesh2
		{
			#include "/tmp/moon.inc"
		}
		scale km

		uv_mapping texture
		{
			function { x }

			texture_map
			{
				[0.0 pigment { Red }]
				[0.2 pigment { Green }]
				[0.4 pigment { Blue }]
				[0.6 pigment { Yellow }]
				[0.8 pigment { White }]
			}
		}
	}

// =======================================================================
//                                  SEA   
// =======================================================================

//#local Sea_Colour = rgb <57/256, 63/256, 86/256>;
#local Sea_Colour = rgbt <0, 0.2, 0.4, 0>;

#local Sea_Texture =
    texture
    {
        pigment {
            colour rgbt 1
            quick_colour rgbt <0, 0, 1, 0>
        }

        normal {
            wrinkles 0.2
            scale 0.01*km
        }      
        //normal {
        //    bozo 0.8
        //    scale 0.002 * km
        //} 
        
        finish
		{
            specular 0.8
            roughness 0.03
            diffuse 0.3
            reflection
			{
                0, 1.0
                falloff 2
                fresnel on
            }
            conserve_energy
        }
    }
    
#declare Sea_Object =
	object
	{
		mesh2
		{
			#include "/tmp/sea.inc"
		}

		scale km

	    texture {
	        Sea_Texture
	    }
	
        interior {
            ior 1.34
            fade_distance 0.01*km
            fade_power 2
            fade_color Sea_Colour
        }
	}

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
//                              ATMOSPHERE
// =======================================================================

// Note! Media units are all in kilometres!

#local Bottom_Of_Atmosphere = Nominal_Terrain_Radius + Atmosphere_Base;
#local Top_Of_Atmosphere = Bottom_Of_Atmosphere
	+ Atmospheric_Depth;

#local Base_Rayleigh_Power = 6.7;
#local Rayleigh_Factor = 1 / Atmospheric_Scale;
#local Rayleigh_Scale = 0.5;
#local Rayleigh_Power = Base_Rayleigh_Power * Rayleigh_Factor;

#local Rayleigh_Density =
	density
	{
        #local height_from_centre =
	        function(x, y, z) {
	            sqrt(x*x + y*y + z*z)
	        }
	        
	    #local height_from_surface =
	        function(x, y, z) {
	            height_from_centre(x, y, z) - Bottom_Of_Atmosphere
	        }
	   
	    #local gravity_factor =
	        function(x, y, z) {
	            (1/3) * Bottom_Of_Atmosphere / height_from_centre(x, y, z)
	        }
	        
		function
		{
			Rayleigh_Scale * exp(-Rayleigh_Power * height_from_surface(x, y, z)
				// disable the gravity factor for now
			    // * gravity_factor(x, y, z)
            )
            //* (Atmospheric_Scale / Atmospheric_Depth)
		}
	}

#local Rayleigh_Colour = rgb <0.2061, 0.3933, 1.0>;

#local Rayleigh_Media =
	media
	{
		method 3
		//samples 31
		//jitter 0.1
		samples 31
		scattering
		{
			RAYLEIGH_SCATTERING
			color 2.3 * Rayleigh_Colour / Atmospheric_Scale
			extinction 1
		}
		density
		{
			Rayleigh_Density
		}
	}

#local Sky_Object =
	difference {
		sphere {
			<0, 0, 0>, Top_Of_Atmosphere
		}
		sphere {
			<0, 0, 0>, Bottom_Of_Atmosphere
		}
		
		pigment
		{
			rgbt 1
		}

		hollow
		
		interior
		{
			media
			{
				Rayleigh_Media
			}
		}
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


object { Terrain_Object }
object { Sea_Object }
//object { Sky_Object }

