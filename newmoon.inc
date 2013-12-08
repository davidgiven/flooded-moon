#declare Lunar_Sphere           = 1750.000 * km; // enclosing diameter of terrain object
#declare Nominal_Terrain_Radius = 1737.400 * km;
#declare Atmosphere_Base        = -2 * km;
#declare Atmospheric_Depth      = 100 * km;
#declare Atmospheric_Scale      = 30 * km;

// =======================================================================
//                                TERRAIN 
// =======================================================================

#local P_Desert = 
    pigment
	{
        crackle solid
        turbulence 0.3
        lambda 3
		scale km
        
        color_map
		{
            [0 color rgb <132,134,118>*0.7/255]
            [0.5 color rgb <80,93,93>*0.7/255]
            [1 color rgb <131,132,118>*0.5/255]
        }            
	}               
          
#local SC=1.5;
#local P_Vegetation1 =
    pigment
	{
        wrinkles
        turbulence 0.3
        lambda 3
		scale km
        color_map
		{
            [0 color rgb <95,120,82>*SC/255]
            [0.3 color rgb <60,85,50>*SC/255]
            [0.6 color rgb <133,142,49>/255]
            [1 color rgb <52,70,68>*SC/255]
        }
    }
    
#local SC=0.5;
#local P_Vegetation2 =
    pigment{
        crackle solid
        turbulence 0.3
        lambda 3
		scale km
        color_map
		{
            [0 color rgb <62,85,40>*0.6*SC/255]
            [0.5 color rgb <92,120,82>*0.5*SC/255]
            [1 color rgb <84,110,57>*0.8*SC/255]
        }
    }
    
#local SC=0.6;
#local P_Vegetation3 =
    pigment {
        crackle solid
        turbulence 0.3
        lambda 3
        scale km
        color_map
		{
            [0 color rgb <28,44,42>*0.5*SC/255]
            [0.5 color rgb <74,110,67>*0.8*SC/255]
            [1 color rgb <52,85,50>*0.6*SC/255]
        }
    }


#declare Yellow_Texture = texture { pigment { Yellow } }
#declare Red_Texture = texture { pigment { Red } }
#declare Green_Texture = texture { pigment { Green } }
#declare Blue_Texture = texture { pigment { Blue } }

#local Rock_Texture =
    texture
    {
		pigment
		{
			cells
			turbulence .5 lambda 8
			color_map
			{
				[0.0 rgb <.6, .6, .6>]
				[0.5 rgb <.8, .8, .8>]
				[1.0 rgb <.4, .4, .4>]
			}
			scale 0.30
			//scale <1,.1,1>*.25
			warp {turbulence .2}
			rotate 30*x
		}

        normal
        {
            slope
            {
                point_at CameraSky, 0.8, 1.0
            }
            normal_map {
              [0.0 granite 0.0]
//              [0.5 granite -0.8]
              [1.0 granite 0.5]
            }
            scale .2
            //scale .5*y

//            warp {turbulence .2}
            rotate 30*x
        }
    }
        
#local Snow_Texture =
	texture
	{
		pigment
		{
			color White
		}

		finish
		{
			diffuse 1
		}
	}

#local SandLow_Texture =
	texture
	{
		pigment
		{
			color rgb <270,279,161>/255
		}
	}

#local SandHigh_Texture =
	texture
	{
		pigment
		{
			color rgb <200,210,161>/255
		}
	}

#local ShorefrontHigh_Texture =
	texture
	{
		pigment
		{
			color Khaki
		}
	}
    
#local ShorefrontLow_Texture =
	texture
	{
		pigment
		{
			color MediumForestGreen
		}
	}

#local ScrubLow_Texture =
	texture
	{
		pigment
		{
			cells
			turbulence .5 lambda 8
			pigment_map
			{
				[0.0 P_Vegetation1]
				[0.3 P_Vegetation2]
				[0.6 P_Vegetation3]
			}
			scale 0.1
			//scale <1,.1,1>*.25
			warp {turbulence .2}
			rotate 30*x
		}
	}
    
#local ScrubHigh_Texture =
	texture
	{
		pigment
		{
			cells
			turbulence .5 lambda 8
			pigment_map
			{
				[0.0 P_Vegetation3]
				[0.3 P_Vegetation2]
				[0.6 P_Vegetation1]
			}
			scale 0.1
			//scale <1,.1,1>*.25
			warp {turbulence .2}
			rotate 30*x
		}
	}

#local Fields_Texture =
	texture
	{
		pigment
		{
			pigment_pattern
			{
				crackle
				solid
			} 
			scale 0.01*km

			pigment_map
			{
				[0 P_Desert]
				[0.21 P_Vegetation2]
				[1 P_Vegetation1]
			}
		}
	}


/* Really flat. Sandy shore, fields, barren rockland. */

#local Flat_Surface =
    texture
    {
		uv_mapping function { x }
	
		texture_map
		{
			[-0.020 Rock_Texture]
			[ 0.002 SandLow_Texture]
			[ 0.005 SandHigh_Texture]
			[ 0.006 ShorefrontLow_Texture]
			[ 0.008 ShorefrontHigh_Texture]
			[ 0.010 Fields_Texture]
			[ 2.000 ScrubLow_Texture]
			[ 3.000 ScrubHigh_Texture]
			[ 4.000 Rock_Texture]
			[10.000 Snow_Texture]
			/*
			[ 0.005 SandHigh_Texture ]
			[ 0.006 ShorefrontLow]
			[ 0.008 ShorefrontHigh]
			[ 0.010 Fields_Texture]
			[ 1.000 P_Vegetation3]
			[ 2.000 P_Vegetation3]
			[ 10.000)*km pigment { color White }]
			*/
		}
	}
    
/* Flattish. Less sand, no fields. */

#local Flattish_Surface =
    texture
    {
		uv_mapping function { x }
	
		texture_map
		{
			[-0.020 Rock_Texture]
			[ 0.002 SandLow_Texture]
			[ 0.004 ShorefrontLow_Texture]
			[ 0.008 ShorefrontHigh_Texture]
			[ 0.015 ScrubLow_Texture]
			[ 2.000 ScrubHigh_Texture]
			[ 3.000 Rock_Texture]
			[10.000 Snow_Texture]
		}
	}
    
/* Steepish. More rocks, less vegetation. */

#local Steepish_Surface =
    texture
    {
		uv_mapping function { x }
	
		texture_map
		{
			[-0.020 Rock_Texture]
			[ 0.020 Rock_Texture]
			[ 0.030 Rock_Texture]
			[ 1.500 ScrubHigh_Texture]
			[ 3.000 Rock_Texture]
			[10.000 Snow_Texture]
		}
	}
    
/* Really steep. Rock face. */

#local Steep_Surface = Rock_Texture;

#local Terrain_Object =
	object
	{
		mesh2
		{
			#include "/tmp/moon.inc"
		}
		scale km

		texture
		{
			/* u is height, in kilometres, above sealevel.
			 * v is the slope (0 = horizontal, 1 = steep).
			 */
			 
			uv_mapping function { y }
			turbulence 0.1

			texture_map
			{
				[0.05 Flat_Surface ]
				[0.1 Flattish_Surface ]
				[0.2 Steepish_Surface ]
				[0.4 Steep_Surface ]
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
    
#local Sea_Object =
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

// ==========================================================================
//                                 OVERALL
// ==========================================================================

#declare Moon_Object =
	union
	{
		object { Terrain_Object }
		object { Sea_Object }
		object { Sky_Object }
	}
