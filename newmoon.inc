#declare Lunar_Sphere           = 1750.000 * km; // enclosing diameter of terrain object
#declare Nominal_Terrain_Radius = 1737.400 * km;
#declare Atmosphere_Base        = -2 * km;
#declare Atmospheric_Depth      = 200 * km;
#declare Atmospheric_Scale      = (Atmospheric_Depth * 2);

#local map =
	function(x, lo, hi)
	{
	  (clip(x, lo, hi)-lo) / (hi-lo)
	}

#local unmap =
	function(x, lo, hi)
	{
		x*(hi-lo) + lo
	}

// =======================================================================
//                                TERRAIN 
// =======================================================================

#local Fields_Texture =
	texture
	{
		pigment
		{
			bozo
			color_map
			{
				[0 color <0.60, 0.75, 0.30>]
				[1 color <0.20, 0.50, 0.10>]
			}
			scale 0.010*km
		}

		normal
		{
			bumps 0.25
			scale 0.100*km
		}
	}

/* Other */

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
		scale 0.3*km
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
		scale 0.3*km
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
        scale 0.3*km
        color_map
		{
            [0 color rgb <28,44,42>*0.5*SC/255]
            [0.5 color rgb <74,110,67>*0.8*SC/255]
            [1 color rgb <52,85,50>*0.6*SC/255]
        }
    }

#macro MakeGrass(col)
	texture
	{
		pigment
		{
			color rgb col
		}
		normal
		{
			wrinkles 0.3
			scale 0.001*km
		}
		finish
		{
			diffuse 0.6
			brilliance 1.5
			roughness 0.5
		}
	}
	texture
	{
		pigment
		{
			color rgbt 1
		}
		normal
		{
			bumps 0.3
			scale 1*km
			warp
			{
				turbulence 0.1
			}
		}
	}
#end

#local Real_Grass_Texture = MakeGrass(<0.3, 0.4, 0.12>)
#local Bushes_Texture = MakeGrass(<0.2, 0.3, 0.12>)

#local Grass_Texture =
	texture
	{
		pigment_pattern
		{
			uv_mapping function { y }
			pigment_map
			{
				[0 rgb <0, 0, 0>]
				[1 rgb <1, 1, 1>]
			}
		}
		turbulence 0.2

		texture_map
		{
			[0.0 Real_Grass_Texture ]
			[0.1 Bushes_Texture ]
		}
	}

#declare Yellow_Texture = texture { pigment { Yellow } }
#declare Red_Texture = texture { pigment { Red } }
#declare Green_Texture = texture { pigment { Green } }
#declare Blue_Texture = texture { pigment { Blue } }

#if (0)
#local LowRock_Texture =
	texture
	{
		pigment
		{
			bozo
			turbulence 0.04
			omega 0.75
			color_map
			{
				[0.0 rgb <.7, .6, .3>]
				[0.6 rgb <.8, .6, .4>]
				[1.0 rgb <.4, .4, .4>]
			}
			scale 0.100*km
			//scale <1,.1,1>*.25
			scale CameraSky*2
			warp {turbulence .2}
		}

/*
		normal
		{
			bumps 0.2
			turbulence 0.1
			omega 1
			scale 0.2*km
		}
	*/
	}
	texture
	{
		pigment
		{
			slope
			{
				point_at CameraSky, 0, 1.0
			}
			color_map
			{
				[0.0 rgbt <1, 1, 1, 1>]
				[0.3 rgbt <0.5, 0.5, 0.5, 0>]
			}
			scale 1
		}
	}
	texture
	{
		pigment
		{
			crackle
			color_map
			{
				[0    rgbt 0.3]
				[0.3  rgbt 1]
			}
			scale 0.010*km
		}
	}
#end

#macro MakeRock(c, s, g, bs)
	texture
	{
		pigment
		{
			granite
			color_map
			{
				[ 0.02  color c * 1.3 ]
				[ 0.20  color c * 1.3 ]
				[ 0.85  color c * 1.0 ]
				[ 1.00  color c * 0.5 ]
			}
			scale 0.05 * s
		}
		normal
		{
			granite
			bump_size 0.5 * g
			translate 0
			scale 0.05 * bs
			rotate 0
		}
		finish
		{
			ambient 0
			diffuse 0.6 
			brilliance 1.0
			phong 0.0 
			phong_size 40
			specular 0.0 
			roughness 0.15 
			metallic 0.0 
			reflection 0
			reflection_exponent 1.0 
		}
	}
#end

#local LowRock_Texture =
	MakeRock(<0.7, 0.6, 0.3>, 2, 2, 0.7)

#local HighRock_Texture =
	MakeRock(<0.5, 0.5, 0.5>, 2, 2, 1.5)

#if (0)
	texture
	{
		pigment
		{
			slope
			{
				point_at CameraSky, 0, 1.0
			}
			color_map
			{
				[0.0 rgbt <1, 1, 1, 1>]
				[0.3 rgbt <0, 0, 0, 0>]
			}
			scale 1
		}
	}
#end

#local Cracked_Rock_Texture =
    texture
    {
		#local granite1_density = 1.0;

		#local granite_1_pigment =
			pigment
			{
				crackle
				pigment_map
				{
					[0.0 rgb <0.4,0.23,0.15>]
					[0.035 rgb <0.7,0.6,0.55>]
					[0.05 rgbt <0.9,0.73,0.65,1.0>]
					[1.0 rgbt 1 ] //rgb <1,0.83,0.75>]
				}
				warp
				{
					turbulence 0.2
				}
				scale 0.010*km
			}

		#local granite_1_grain_pigment =
			pigment
			{
				granite
				color_map
				{
					[0.0 rgb granite1_density*0.1*<0.9,0.73,0.65>]
					[0.0 rgb granite1_density*0.3*<0.9,0.73,0.65>]
					[0.6 rgb granite1_density*<0.9,0.73,0.65> ]
					[0.7 rgb granite1_density*<0.9,0.73,0.65> ]
					[1.0 Black]
				}
				scale 0.02*km
			}

		pigment {granite_1_pigment}
		pigment {granite_1_grain_pigment}
		normal
		{
			pigment_pattern {granite_1_pigment} 1
		}
        scale 0.5
	}

#local Uncracked_Rock_Texture =
	texture
	{
		#local granite_density = 0.7;

		pigment
		{
			wrinkles
			pigment_map
			{
				[0.0 rgb granite_density*0.2*<0.4,0.23,0.15>]
				[0.05 rgb granite_density*0.4*<0.7,0.7,0.55>]
				[0.8 rgb granite_density*0.6*<0.9,0.73,0.65>]
				[1.0 rgb granite_density*0.8*<1,0.83,0.75>]
			}
			warp
			{
				turbulence 0.5
			}
		}
		scale 0.001*km
	}

#local Rock_Texture =
	texture
	{
		pigment_pattern
		{
			uv_mapping function { y }
		}
		texture_map
		{
			[0.0 Grass_Texture]
			[0.05 Bushes_Texture]
			[0.1 Uncracked_Rock_Texture]
		}
	}

/*
	}0		pigment_pattern
		{
			uv_mapping function { map(x, -20, 20) }
			pigment_map
			{
				[0 rgb <0, 0, 0>]
				[1 rgb <1, 1, 1>]
			}
		}
	
		texture_map
		{
			[map( 0.000, -20, 20) LowRock_Texture]
			[map( 5.000, -20, 20) HighRock_Texture]
		}
	}
	*/
        
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
			bozo
			pigment_map
			{
				[0.0 P_Vegetation1]
				[0.3 P_Vegetation2]
				[0.6 P_Vegetation3]
			}
			scale 0.1*km
			//scale <1,.1,1>*.25
			warp
			{
				turbulence .5
				lambda 8
			}
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

/* Really flat. Sandy shore, fields, barren rockland. */

#local Flat_Surface =
    texture
    {
		pigment_pattern
		{
			uv_mapping function { map(x, -20, 20) }
			pigment_map
			{
				[0 rgb <0, 0, 0>]
				[1 rgb <1, 1, 1>]
			}
		}
	
		texture_map
		{
			[map(-0.020, -20, 20) Rock_Texture]
			[map( 0.002, -20, 20) SandLow_Texture]
			[map( 0.005, -20, 20) SandHigh_Texture]
			[map( 0.006, -20, 20) ShorefrontLow_Texture]
			[map( 0.008, -20, 20) ShorefrontHigh_Texture]
			[map( 0.010, -20, 20) Grass_Texture]
			[map( 3.000, -20, 20) Bushes_Texture]
			[map( 4.000, -20, 20) Rock_Texture]
			[map(10.000, -20, 20) Snow_Texture]
		}
	}
    
/* Flattish. Less sand, no fields. */

#local Flattish_Surface =
    texture
    {
		pigment_pattern
		{
			uv_mapping function { map(x, -20, 20) }
			pigment_map
			{
				[0 rgb <0, 0, 0>]
				[1 rgb <1, 1, 1>]
			}
		}
		warp
		{
			turbulence 0.1
		}
	
		texture_map
		{
			[map(-0.020, -20, 20) Rock_Texture]
			[map(-0.002, -20, 20) SandLow_Texture]
			[map( 0.002, -20, 20) SandHigh_Texture]
			[map( 0.004, -20, 20) ShorefrontLow_Texture]
			[map( 0.008, -20, 20) ShorefrontHigh_Texture]
			[map( 0.015, -20, 20) Grass_Texture]
			[map( 2.000, -20, 20) Bushes_Texture]
			[map( 3.000, -20, 20) Rock_Texture]
			[map(10.000, -20, 20) Snow_Texture]
		}
	}
    
/* Steepish. More rocks, less vegetation. */

#local Steepish_Surface =
    texture
    {
		pigment_pattern
		{
			uv_mapping function { map(x, -20, 20) }
			pigment_map
			{
				[0 rgb <0, 0, 0>]
				[1 rgb <1, 1, 1>]
			}
		}
	
		texture_map
		{
			[map(-0.020, -20, 20) Rock_Texture]
			[map( 0.020, -20, 20) Rock_Texture]
			[map( 0.030, -20, 20) Grass_Texture]
			[map( 1.500, -20, 20) Bushes_Texture]
			[map( 3.000, -20, 20) Rock_Texture]
			[map(10.000, -20, 20) Snow_Texture]
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
			 
			pigment_pattern
			{
				uv_mapping function { y }
				pigment_map
				{
					[0 rgb <0, 0, 0>]
					[1 rgb <1, 1, 1>]
				}
			}
			turbulence 0.2

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

#local WaterColour = <0.60, 0.85, 0.85>;
#local WaterClarity = 0.4;
#local WaterFrequency = 1300;
#local WaterReflection = 0.3;

#local Sea_Material =
	material
	{
		texture
		{
			pigment
			{
				color rgb WaterColour transmit WaterClarity
			}

			normal
			{
				waves 0.10
				scale 0.002*km
				translate <0, Nominal_Terrain_Radius*2, 0>
			}
			finish
			{
				brilliance 0.9
				specular 0.4
				reflection WaterReflection
				diffuse 0.3
				roughness 0.01
				metallic 0.0
			}
		}
		texture
		{
			pigment
			{
				color rgbt 1
			}

			normal
			{
				bozo 0.5
				scale 0.001*km
			}
			finish
			{
				brilliance 0.9
				specular 0.4
				reflection WaterReflection
				diffuse 0.3
				roughness 0.01
				metallic 0.0
			}
		}
		texture
		{
			#local Seashore =
				function
				{
					pattern
					{
						object
						{
							object
							{
								Terrain_Object
								scale (Nominal_Terrain_Radius+0.010) / Nominal_Terrain_Radius
							}
						}
					}
				}

			#local Waves_Pigment =
				pigment
				{
					crackle
					scale 0.05*km

					pigment_map
					{
						[0.00 rgbt <1, 1, 1, 0>]
						[0.07 rgbt <1, 1, 1, 0>] 
						[0.10 rgbt <1, 1, 1, 1>]
					}
				}

			pigment
			{
				function
				{
					Seashore(x, y, z)
				}

				pigment_map
				{
					[0 rgbt <1, 1, 1, 1>]
					[1 Waves_Pigment]
				}

				warp
				{
					turbulence 0.001
					omega 1
					lambda 4
				}
			}

			finish
			{
				diffuse 1
				ambient 0.05
			}
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

	    material {
	        Sea_Material
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
		#local g = 1.622; // m/s^2 --- note METRES!
		#local L = 0.0065/6; // K/m --- temperature lapse rate. Adjusted, crudely
		#local M = 0.0289644; // kg/mol --- molar mass of dry air
		#local T0 = 293; // kelvin --- STP
		#local R = 8.3144; // joules / mole kelvin --- gas constant
		#local gM_RL = (g*M) / (R*L);
		#local L_T0 = L / T0;

		#local dscale = 1.0; // from atmospheres to whatever Povray uses

        #local height_from_centre =
	        function(x, y, z) {
	            sqrt(x*x + y*y + z*z)
	        }
	        
	    #local height_from_surface =
	        function(x, y, z) {
	            height_from_centre(x, y, z) - Bottom_Of_Atmosphere
	        }
	   
		// Calculates air pressure at a given height, in atmospheres (1 at
		// ground level). Heights are in metres.

		#local pressure = 
			function(h)
			{
				pow((1 - L_T0*h), gM_RL)
			}

		#warning concat("Pressure at   0km: ", str(pressure(0), 0, 8))
		#warning concat("Pressure at   5km: ", str(pressure(5000), 0, 8))
		#warning concat("Pressure at  10km: ", str(pressure(10000), 0, 8))
		#warning concat("Pressure at  15km: ", str(pressure(15000), 0, 8))
		#warning concat("Pressure at  50km: ", str(pressure(50000), 0, 8))
		#warning concat("Pressure at 100km: ", str(pressure(100000), 0, 8))

		function
		{
			dscale * pressure(height_from_surface(x, y, z)*1000)
		}
	}

#local Rayleigh_Colour = rgb <0.2061, 0.3933, 1.0>;

#local Rayleigh_Media =
	media
	{
		method 3
		//samples 31
		//jitter 0.1
		samples 60
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
//                                  TREES 
// ==========================================================================

#if (0)
#include "black_tupelo.inc"

#local Tree_Object =
	union { 
		object
		{
			black_tupelo_13_stems
			pigment
			{
				color rgb <0.5, 0.4, 0>
			}
		}
		object
		{
			black_tupelo_13_leaves
			texture
			{
				pigment
				{
					color rgb <17, 69, 17>/255
				} 
				finish
				{
				}
			}
		}

		scale 1/black_tupelo_13_height
	}
#else

#include "maple-mesh.inc"
//#include "tomtree/TOMTREE.inc"
#local Tree_Object =
	object {
		TREE
	}
#end

#macro Tree(pos, upv, w, h, r)
	object
	{
		Tree_Object
		rotate r*y
		scale h
		Reorient_Trans(y, upv)
		translate pos
	}
#end

#local Trees_Object =
	union
	{
		#include "/tmp/props.inc"
	}

// ==========================================================================
//                                 OVERALL
// ==========================================================================

#declare Moon_Object =
	union
	{
		object { Terrain_Object }
		object { Trees_Object }
		object { Sea_Object }
		object { Sky_Object }
	}
