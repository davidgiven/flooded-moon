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
#include <fstream>
#include <unordered_map>
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

const double MAXHEIGHT = 22; // maximum height of any object on the surface

PDSSet geoidpds;
PDSSet terrainpds;

#include "variables.h"

class PluginVariables : public Variables
{
public:
	PluginVariables()
	{
		parse(0, NULL);
		for (auto s : terrain)
			terrainpds.add(s);

		for (auto s : geoid)
			geoidpds.add(s);
	}
};

PluginVariables vars;

#include "functions.h"
#include "texture.h"
#include "writer.h"
#include "sphericalroam.h"
#include "povpluginwriter.h"
#include "terrain.h"

class Initialiser
{
public:
	Initialiser()
	{
		initCalculon();
	}
};

Terrain terrain;
Initialiser init;

#define SPHERE          (10000.000)
#define AVERAGE_TERRAIN  (1737.400)
#define SEALEVEL           (-1.850)

#define ATMOSPHERE_BASE    (-2.000)
#define ATMOSPHERE_DEPTH  (200.000)
#define ATMOSPHERE_SCALE  (ATMOSPHERE_DEPTH * 2.0)

static double sea_radius(const Point& p)
{
	return geoidpds.at(p)/1000.0 + SEALEVEL; // km
}

static double terrain_radius(const Point& p)
{
	return terrain.at(p) / 1000.0; // km
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
	return terrain.at(scaled) - sea_radius(scaled);
}

extern "C" povray_scalar_fn povray_scalar_function_terrain_gradient;
double povray_scalar_function_terrain_gradient(double* data)
{
	Point scaled = Point(data)*SPHERE;
	return terrain.slope(scaled);
}

extern "C" povray_scalar_fn povray_scalar_function_rayleigh_density;
double povray_scalar_function_rayleigh_density(double* data)
{
	Point scaled = Point(data) / 1000.0;
	return rayleigh_density(scaled);
}

extern "C" povray_mesh_fn povray_mesh_function_moon;
struct povray_mesh* povray_mesh_function_moon(void)
{
	Transform world;

	/* Set up the camera position. */

	world = world.lookAt(Point::ORIGIN, Vector::Y, Vector::Z);
	world = world.rotate(Vector::Y, -vars.longitude);
	world = world.rotate(Vector::X, -vars.latitude);
	world = world.rotate(Vector::X, -90);
	world = world.translate(Vector(0, vars.radius, 0));
	
	/* world is now relative to a point on a normalised sphere. Find out
	 * what sealevel is at this point and adjust the camera so it's
	 * relative to that. */

	Point camera = world.untransform(Point::ORIGIN);
	double sealevel = sea_radius(camera);

	std::cerr << "sealevel at camera is " << sealevel << "km\n";

	world = world.translate(Vector(0, sealevel-vars.radius+vars.altitude, 0));

	/* Adjust for pointing direction. */

	world = world.rotate(Vector::Y, -vars.bearing);
	world = world.rotate(Vector::X, 90 + vars.azimuth);

	PovPluginWriter writer;
	std::cerr << "creating mesh\n";
	SphericalRoam(terrain, world, sealevel, vars.fov / vars.shmixels).writeTo(writer);
	std::cerr << "calculating textures\n";
	//writer.applyTextureData(texture);
	std::cerr << "calculating normals\n";
	writer.calculateNormals();

	return writer.writeToMesh(world);
}

