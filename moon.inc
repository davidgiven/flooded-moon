#declare Horizon_Distance_2     = Altitude * 
		(2 * (Nominal_Terrain_Radius + Sea_Level) + Altitude);
#declare Horizon_Distance       = sqrt(Horizon_Distance_2);

#local km = 1 / (Nominal_Terrain_Radius + Sea_Level);
        
#warning concat("Current horizon distance is: ", str(Horizon_Distance, 0, 1), "\n")

#declare Topography_Table =
	function { pigment { image_map {
		png "KaguyaTopographyDataLowRes.png"
		//png "KaguyaTopographyData.png"
		map_type 1 // spherical
		interpolate 2 // bilinear
	}}}

// returns altitude in kilometres
#declare Topography_Altitude =
	function(x, y, z)
	{
		(Topography_Table(x, y, z).gray - 0.5) * 22
			+ 1737.400
	}

#declare Geoid_Pigment =
    pigment {
        image_map {
            png "GeoidData.png"
            map_type 1 // spherical
            interpolate 2 // bilinear
        }
        rotate y*180
    }

#declare Geoid_Table = function { pigment { Geoid_Pigment } }

// The geoid data has range 1081.1 and offset 525.3. Povray reads it out of
// the file as unit-scaled values, so we actually get the elevation with:
//
//   elevation = (sample * 1081.1) - 525.3
//
// The units are in metres.
//
// Note, however, that the geoid imagemap is from -180 to +179 longitude on
// the X axis, while the topography map is from 0 to +359 longitude. 

// returns delta in kilometres
#declare Geoid_Altitude =
	function(x, y, z)
	{
		(Geoid_Table(x, y, z).gray * 1.0811) - 0.5253
	}

#local Surface_Noise =
    function {
        pigment {
            bozo
            turbulence 0.5
            color_map {
                [0 color rgb 0]
                [1 color rgb 1]
            }
        }
    }
    
#declare Moon_Altitude = 
	function(x, y, z)
	{
		Topography_Altitude(x, y, z) - Geoid_Altitude(x, y, z)
//			+ Surface_Noise(x*3e3, y*3e3, z*3e3).gray * 0.01
	}

#declare Moon_Mesh =
	isosurface
	{
		function
		{
			#local scaling_factor = 1.0 / Lunar_Sphere;
			f_sphere(x, y, z, 1) +
				(Lunar_Sphere - Moon_Altitude(x, y, z))
					*scaling_factor

		}
		contained_by
		{
			sphere
			{
				<0, 0, 0>, 1
			}
		}

        max_gradient 1.5
		accuracy 0.00001
	}


#local Geoid_Texture =
    texture
    {
        Geoid_Pigment
    }
    
#local Moon_Texture =
	texture
	{
        pigment {
            #local SandHigh_Colour = rgb <200,210,161>/255;
            #local SandLow_Colour = rgb <270,279,161>/255;
            
            #local P_Desert = 
                pigment {
                    crackle solid
                    turbulence 0.3
                    lambda 3
                    scale <1,30,1> //* 0.1 * km
                    
                    color_map {
                        [0 color rgb <132,134,118>*0.7/255]
                        [0.5 color rgb <80,93,93>*0.7/255]
                        [1 color rgb <131,132,118>*0.5/255]
                    }            
            }               
                      
            #local ShorefrontHigh =
                pigment {
                    color Khaki
                }
                
            #local ShorefrontLow =
                pigment {
                    color MediumForestGreen
                }
                
            #local SC=1.5;
            #local P_Vegetation1 =
                pigment {
                    wrinkles
                    turbulence 0.3
                    lambda 3
                    scale <1,30,1> //* 0.3 * km
                    color_map{
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
                    scale <1,30,1> //* 0.1 * km
                    color_map{
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
                    scale <1,30,1> * 0.1 * km
                    color_map{
                        [0 color rgb <28,44,42>*0.5*SC/255]
                        [0.5 color rgb <74,110,67>*0.8*SC/255]
                        [1 color rgb <52,85,50>*0.6*SC/255]
                    }
                }

            onion
            turbulence 0
            lambda 3
            omega 3
            pigment_map
            {
                #local sealevel = (Nominal_Terrain_Radius + Sea_Level) /
                    Lunar_Sphere;
               
                [sealevel-0.100*km color White]
                [sealevel+0.000*km color SandLow_Colour]
                [sealevel+0.005*km color SandHigh_Colour]
                [sealevel+0.005*km ShorefrontLow]
                [sealevel+0.010*km ShorefrontHigh]
                [sealevel+0.010*km
                    pigment_pattern
                    {
                        crackle
                        solid
                        scale km * 1.0
                    } 
                    pigment_map
                    {
                        [0. P_Desert]
                        [0.21 P_Vegetation2]
                        [1 P_Vegetation1]
                    }
                ]
                [sealevel+1.000*km P_Vegetation3]
                [sealevel+2.000*km P_Vegetation3]
                [sealevel+10.000*km color White]
            }
        }
        
//        pigment
//        {
//            onion
//            pigment_map
//            {
//                [0 colour Black]
//                [0.1 colour Black]
//                [0.2 colour Red]
//                [0.3 colour Red]
//                [0.4 colour White]
//            }
//            scale Contours * km
//        }
//        normal {
//            average
//            normal_map {
//                [ 1 
//                   pigment_pattern
//                    {
//                        agate
//                        color_map {
//                            [0.99 White]
//                            [1 White*0.7]
//                        } 
//                    }
//                    bump_size 0.5
//                ]
//                [ 1
//                    granite 0.3
//                ]
//            }
//            scale <1, 30, 1> * 0.1
//        }
        finish {
            diffuse 1
            specular 0
        }
	}

#local Moon_Object =
	object
	{
		object {
			Moon_Mesh
		}

		texture {
		    Moon_Texture
		}

        scale Lunar_Sphere
        rotate x*90
        rotate z*-90
		//rotate <0, 0, 1>
		translate <0, 0, 0>
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
        }

        // sea swell        
        normal { 
//            ripples 0.5
//            frequency 100
//            scale 0.01 * km
//            translate <0, 1, 0>

            waves 0.1
            scale 0.001 * km
        }
        
        // local waves
//        normal { 
//            ripples 1
//            translate <-0.5, -0.5, -0.5>
//            scale 0.001 * km
//            translate <0, 1, 0>
//        }
        
        finish {
            ambient 0
            specular 1
            diffuse 0.3
            reflection {
                0, 0.5
                fresnel on
            }
            conserve_energy
        }
    }
    
#local Sea_Object =
	sphere
	{
		<0, 0, 0>, 1

	    texture {
	        Sea_Texture
	    }
	
        interior {
            ior 1.34
            fade_distance 0.01
            fade_power 2
            fade_color Sea_Colour
        }

	    scale Nominal_Terrain_Radius + Sea_Level
	}

// =======================================================================
//                              ATMOSPHERE
// =======================================================================

// Note! Media units are all in kilometres!

#local Bottom_Of_Atmosphere = Nominal_Terrain_Radius + Sea_Level
    + 0.001;
	//+ Cloud_Base + Cloud_Height + 0.001;
#local Top_Of_Atmosphere = Bottom_Of_Atmosphere
	+ Atmospheric_Depth;

#local Base_Rayleigh_Power = 6.7;
#local Rayleigh_Factor = 1 / Atmospheric_Scale;
#local Rayleigh_Scale = 1;
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
		intervals 3
		samples 3
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
//                                 CLOUDS 
// =======================================================================

#local Cloud_Pigment =
	pigment
	{
		average
		pigment_map
		{
			[1 planar]
			[1 granite scale 100]
		}
	}

// Note: media units are in kilometres!

#local Cloud_Bottom = Cloud_Base + Nominal_Terrain_Radius + Sea_Level;
#local Cloud_Top = Cloud_Bottom + Cloud_Height;
#local Clouds_Media =
	media
	{
		samples 3
//		jitter 0.1
		scattering
		{
			1 1
		}
		
	    density
	    {
		    spherical
		    scale Cloud_Top
		    density_map
		    {
		    	#local p = Cloud_Height / Cloud_Top;
			    [p*(0/8) rgb 0]
			    [p*(4/8) rgb 0.2]
			    [p*(7/8) rgb 1]
			    [p*(8/8) rgb 0]
		    }
	    }
	
	    density
	    {
	        bumps
	        warp
	        {
	        	turbulence 1
	        	octaves 4
	        }
	        
	        scale Cloud_Height
	        density_map
	        {
	            [0 rgb 0]
	            [0.6 rgb 0]
	            [1 rgb .8]
	        }
	    }
	    
	    density
	    {
	        agate
	        //scale x*10
	        rotate y*90
	        scale y*10
	        //scale x*3
	        //rotate z*90
	        density_map
	        {
	            [0 rgb 1]
	            [0.3 rgb 1]
	            [0.4 rgb 0]
	            [1 rgb 0]
	        }
	        scale 150
	    }
	}

#local Clouds_Object =
	difference
	{
		sphere
		{
			<0, 0, 0>, Cloud_Top
		}

		sphere
		{
			<0, 0, 0>, Cloud_Bottom
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
				Clouds_Media
			}
		}
	}		

#declare Moon =
	union
	{
		object { Moon_Object }
		object { Sea_Object }
		object { Sky_Object }
//		object { Clouds_Object }
	}

