#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../povray/include/povray_plugin.h"

#define km 1000.0
#define Nominal_Sphere (10000.0*km)

static double terrain(double x, double y, double z)
{
	return 1437.4*km;
}

extern "C" povray_vector_fn terrain_isosurface;
void terrain_isosurface(double data[])
{
	double x = data[0];
	double y = data[1];
	double z = data[2];
	double t = terrain(x, y, z) / Nominal_Sphere;
	data[0] = x*x + y*y + z*z - t*t;
}

