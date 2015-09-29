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

double slope_of_terrain(const Point& p)
{
	/* Important --- mapToSphere() returns METRES! */

	/* Direction vector scaled to ground level. */

	Vector location(p.x, p.y, p.z);

	/* Construct a magic base vector based on the element with the
	 * lowest magnitude. */

	double ax = fabs(location.x);
	double ay = fabs(location.y);
	double az = fabs(location.z);
	Vector base(0, 0, 0);
	if ((ax < ay) && (ax < az))
		base.x = 1;
	else if ((ay < ax) && (ay < az))
		base.y = 1;
	else
		base.z = 1;

	/* The cross product of this and the direction vector will be
	 * orthogonal to the direction vector. */

	Vector vp = base.cross(location).normalise();

	/* Use this to construct a point which is a certain distance away
	 * from the original point (in a random direction).
	 * (Remember that location is in kilometres. */

	Point p1 = Point(vp*0.100) + location;

	/* Create two more points based on this, equally spaced around
	 * the rotation vector. */

	Transform t = Transform().rotate(location, 60);
	Point p2 = t.transform(p1);
	Point p3 = t.transform(p2);

	/* Map them onto the terrain. */

	p1 = terrainpds.mapToSphere(p1);
	p2 = terrainpds.mapToSphere(p2);
	p3 = terrainpds.mapToSphere(p3);

	/* Now calculate the normal of this triangle. */

	Vector normal = (p2-p3).normalise().cross((p1-p3).normalise());

	/* The dot product against the direction vector will be our slope (1
	 * horizontal, 0 vertical). */

	double slope = normal.normalise().dot(location.normalise());

	/* Adjust to something a little easier for scripts to handle. */

	double aslope = (1.0 - fabs(slope)) * 5.0;
	return max(min(aslope, 1.0), 0.0);
}

static double terrain_radius(const Point& p)
{
	double t = terrainpds.at(p);
	if (t < 0.050*km)
		return 0.050*km;
	return t;
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

static void isosurface(double data[], double (*callback)(const Point&))
{
	Point unscaled = {data[0], data[1], data[2]};
	Point scaled = {data[0]*SPHERE, data[1]*SPHERE, data[2]*SPHERE};

	double t = callback(scaled) / SPHERE;

	data[0] = unscaled.lengthSquared() - t*t;
}

extern "C" povray_vector_fn terrain_isosurface;
void terrain_isosurface(double data[])
{
	isosurface(data, terrain_radius);
}

extern "C" povray_vector_fn sea_isosurface;
void sea_isosurface(double data[])
{
	isosurface(data, sea_radius);
}

extern "C" povray_vector_fn terrain_altitude;
void terrain_altitude(double data[])
{
	Point scaled = {data[0]*SPHERE, data[1]*SPHERE, data[2]*SPHERE};

	data[0] = (terrain_radius(scaled) - sea_radius(scaled)) / km;
}

extern "C" povray_vector_fn terrain_gradient;
void terrain_gradient(double data[])
{
	Point scaled = {data[0]*SPHERE, data[1]*SPHERE, data[2]*SPHERE};
	data[0] = slope_of_terrain(scaled);
}

extern "C" povray_vector_fn rayleigh_density;
void rayleigh_density(double data[])
{
	Point unscaled = {data[0]*km, data[1]*km, data[2]*km};
	data[0] = rayleigh_density(unscaled);
}

