#version 3.7;
#include "consts.inc"
#include "colors.inc"
#include "stones.inc"
#include "textures.inc"
#include "functions.inc"
#include "transforms.inc"

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

camera
{
    angle 50
	right -x
	#include "/tmp/camera.inc"
}

background
{
    rgb <0, 0, 0>
}

object
{
	#include "/tmp/moon.inc"

	pigment
	{
		colour rgb <1, 1, 1>
	}
}

sphere
{
	<0, 0, 0>, 1500

	pigment
	{
		colour rgb <1, 1, 1>
	}
}

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
	rotate z*(6*360/24)
}

