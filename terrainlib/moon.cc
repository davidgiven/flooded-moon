#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <libnoise/noise.h>
#include <stdexcept>
#include <cmath>
#include <string>
#include <map>
#include <deque>
#include <iostream>
#include <boost/format.hpp>
#include <boost/algorithm/string/split.hpp>
#include <boost/algorithm/string/predicate.hpp>
#include <boost/algorithm/string/classification.hpp>
#include <boost/iostreams/device/mapped_file.hpp>
#include "utils.h"
#include "matrix.h"
#include "map.h"
#include "pdslabel.h"
#include "pds.h"
#include "pdsset.h"
#include "../../povray/include/povray_plugin.h"

using std::min;
using std::max;

#define km                1000.0

#define SPHERE          (10000.000*km)
#define AVERAGE_TERRAIN  (1737.400*km)
#define SEALEVEL           (-1.850*km)

#define ATMOSPHERE_BASE    (-2.000*km)
#define ATMOSPHERE_DEPTH  (200.000*km)
#define ATMOSPHERE_SCALE  (ATMOSPHERE_DEPTH * 2.0)

static PDSSet geoidpds = {
	"geoid/LDGM_4_GLGM-3_L60.LBL:geoid/LDGM_4_GLGM-3_L60.IMG"
};

static PDSSet terrainpds = {
	"lroc/WAC_GLD100_E300N0450_256P.IMG::OFFSET=1737400",
	"lroc/WAC_GLD100_E300S0450_256P.IMG::OFFSET=1737400",
	"lroc/WAC_GLD100_E300N1350_256P.IMG::OFFSET=1737400",
	"lroc/WAC_GLD100_E300S1350_256P.IMG::OFFSET=1737400",
	"lroc/WAC_GLD100_E300N2250_256P.IMG::OFFSET=1737400",
	"lroc/WAC_GLD100_E300S2250_256P.IMG::OFFSET=1737400",
	"lroc/WAC_GLD100_E300N3150_256P.IMG::OFFSET=1737400",
	"lroc/WAC_GLD100_E300S3150_256P.IMG::OFFSET=1737400",
	"lroc/WAC_GLD100_P900N0000_256P.IMG::OFFSET=1737400",
	"lroc/WAC_GLD100_P900S0000_256P.IMG::OFFSET=1737400",
};

static double perlin(double x, double y, double z)
{
	static noise::module::Perlin noise;
	return noise.GetValue(x, y, z);
}

static double terrain_radius(const Point& p)
{
	return terrainpds.at(p);
}

static double sea_radius(const Point& p)
{
	return geoidpds.at(p) + SEALEVEL;
}

const double bottom_of_atmosphere = AVERAGE_TERRAIN + ATMOSPHERE_BASE;
const double top_of_atmosphere = bottom_of_atmosphere + ATMOSPHERE_DEPTH;

const double g = 1.622;     // m/s^2 --- note METRES
const double L = 0.0065/6;  // K/m --- temperature lapse rate. Adjusted, crudely
const double M = 0.0289644; // kg/mol --- molar mass of dry air
const double T0 = 293.0;    // kelvin --- STP
const double R = 8.3144;    // joules / mole kelvin --- gas constant
const double gM_RL = (g*M) / (R*L);
const double L_T0 = L / T0;

const double dscale = 1.0;  // from atmospheres to whatever Povray uses

static double pressure(double height)
{
	return pow((1 - L_T0*height), gM_RL);
}

static double rayleigh_density(const Point& p)
{
	const double height_from_centre = p.length();
	const double height_from_bottom = height_from_centre - bottom_of_atmosphere;

	return dscale * pressure(height_from_bottom);
}

static double isosurface(double* data, double (*callback)(const Point&))
{
	Point unscaled = {data[0], data[1], data[2]};
	Point scaled = {data[0]*SPHERE, data[1]*SPHERE, data[2]*SPHERE};

	double t = callback(scaled) / SPHERE;

	return unscaled.lengthSquared() - t*t;
}

extern "C" povray_scalar_fn povray_scalar_function_terrain_isosurface;
double povray_scalar_function_terrain_isosurface(double* data)
{
	return isosurface(data, terrain_radius);
}

extern "C" povray_scalar_fn povray_scalar_function_sea_isosurface;
double povray_scalar_function_sea_isosurface(double* data)
{
	return isosurface(data, sea_radius);
}

extern "C" povray_scalar_fn povray_scalar_function_terrain_altitude;
double povray_scalar_function_terrain_altitude(double* data)
{
	Point scaled = {data[0]*SPHERE, data[1]*SPHERE, data[2]*SPHERE};
	return (terrain_radius(scaled) - sea_radius(scaled)) / km;
}

extern "C" povray_scalar_fn povray_scalar_function_terrain_gradient;
double povray_scalar_function_terrain_gradient(double* data)
{
	Point scaled = {data[0]*SPHERE, data[1]*SPHERE, data[2]*SPHERE};
	return terrainpds.slope(scaled);
}

extern "C" povray_scalar_fn povray_scalar_function_rayleigh_density;
double povray_scalar_function_rayleigh_density(double* data)
{
	Point scaled = {data[0]*km, data[1]*km, data[2]*km};
	return rayleigh_density(scaled);
}

