#local Radius = 6371;

#local Bottom_Of_Atmosphere = Radius+0;
#local Top_Of_Atmosphere = Bottom_Of_Atmosphere+400;

#local Atmospheric_Scale = 240;

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

#local EarthColours =
	pigment
	{
		image_map
		{
			jpeg "earth/Color Map.jpg"
			map_type 1
			interpolate 4
			once
		}
		rotate x*-90
	}

#local Sea =
	texture
	{
		pigment { EarthColours }

		normal
		{
			wrinkles 0.1
			scale 0.01
		}
		finish
		{
			ambient 0
			diffuse 0
			specular 0.4 roughness 0.02
		}
	}

#local Land =
	texture
	{
		pigment { EarthColours }

		normal
		{
			bump_map
			{
				jpeg "earth/Bump.jpg"
				map_type 1
				interpolate 4
				once
			}
			rotate x*-90
		}

		finish
		{
			diffuse 1.0
		}
	}

#local Clouds =
	texture
	{
		pigment { White }

		finish
		{
			diffuse 1.0
		}
	}

#declare Earth =
	union
	{
		/* Land */
		sphere
		{
			<0, 0, 0>, Radius
			texture
			{
				image_pattern
				{
					png "earth/Spec Mask.png"
					map_type 1
					interpolate 4
					once
				}

				texture_map
				{
					[0 Sea]
					[1 Land]
				}
			}
				
		}

		/* Clouds */
		sphere
		{
			<0, 0, 0>, Radius+10
			hollow

			texture
			{
				pigment_pattern
				{
					image_map
					{
						png "earth/Clouds.png"
						map_type 1
						interpolate 4
						once
					}
					rotate x*-90
				}
				texture_map
				{
					[0.0 pigment { rgbf <1, 1, 1, 1> }]
					[1.0 Clouds]
				}
			}
		}

		/* Sky */
		sphere
		{
			<0, 0, 0>, Top_Of_Atmosphere
		
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

		/* Industry ring */
		/*
		torus
		{
			35786, 10
			pigment {
				Red
			}
				finish { ambient 1 }

			rotate x*90
		}
		*/

		/* Axial tilt */
		rotate y*23.4
	}
