#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
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
#include "noise.h"

using std::min;
using std::max;

#define SPHERE          (10000.000)
#define AVERAGE_TERRAIN  (1737.400)
#define SEALEVEL           (-1.850)

#define ATMOSPHERE_BASE    (-2.000)
#define ATMOSPHERE_DEPTH  (200.000)
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

static double sea_radius(const Point& p)
{
	return geoidpds.at(p)/1000.0 + SEALEVEL; // km
}

static double terrain_radius(const Point& p)
{
	double radius = terrainpds.at(p) / 1000.0; // km
	Point onsurface = terrainpds.mapToSphere(p, radius);
	double slope = terrainpds.slope(onsurface);

	return radius
		+ multifractal(onsurface*3.0, 1.0, 2.0, 6)*0.010
		+ multifractal(onsurface/2.0, 1.0, 2.0, int(6.0*slope))*0.200 *
			blend0lo(slope, 0.00, 1.0)
	;
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
	Point unscaled(data);
	Point scaled = unscaled * SPHERE;

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
	Point scaled = Point(data)*SPHERE;
	return terrain_radius(scaled) - sea_radius(scaled);
}

extern "C" povray_scalar_fn povray_scalar_function_terrain_gradient;
double povray_scalar_function_terrain_gradient(double* data)
{
	Point scaled = Point(data)*SPHERE;
	return terrainpds.slope(scaled);
}

extern "C" povray_scalar_fn povray_scalar_function_rayleigh_density;
double povray_scalar_function_rayleigh_density(double* data)
{
	Point scaled = Point(data) / 1000.0;
	return rayleigh_density(scaled);
}

