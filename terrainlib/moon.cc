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
#include "sea.h"

class Initialiser
{
public:
	Initialiser()
	{
		initCalculon();
	}
};

Sea sea;
Terrain terrain;
Initialiser init;

#define SPHERE          (10000.000)
#define AVERAGE_TERRAIN  (1737.400)
#define SEALEVEL           (-1.850)

#define ATMOSPHERE_BASE    (-2.000)
#define ATMOSPHERE_DEPTH  (200.000)
#define ATMOSPHERE_SCALE  (ATMOSPHERE_DEPTH * 2.0)

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

extern "C" povray_scalar_fn povray_scalar_function_terrain_altitude;
double povray_scalar_function_terrain_altitude(double* data)
{
	Point p = Point(data);
	return terrain.at(p) - sea.at(p);
}

extern "C" povray_scalar_fn povray_scalar_function_terrain_gradient;
double povray_scalar_function_terrain_gradient(double* data)
{
	Point p = Point(data);
	return terrain.slope(p);
}

extern "C" povray_scalar_fn povray_scalar_function_rayleigh_density;
double povray_scalar_function_rayleigh_density(double* data)
{
	Point scaled = Point(data);
	return rayleigh_density(scaled);
}

struct povray_mesh* mesh_function(XYZMap& shape)
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
	double sealevel = sea.at(camera);

	std::cerr << "sealevel at camera is " << sealevel << "km\n";

	world = world.translate(Vector(0, sealevel-vars.radius+vars.altitude, 0));

	/* Adjust for pointing direction. */

	world = world.rotate(Vector::Y, -vars.bearing);
	world = world.rotate(Vector::X, 90 + vars.azimuth);

	PovPluginWriter writer;
	SphericalRoam(shape, world, sealevel, vars.fov / vars.shmixels).writeTo(writer);
	//writer.applyTextureData(texture);
	writer.calculateNormals();

	return writer.writeToMesh(world);
}

extern "C" povray_mesh_fn povray_mesh_function_moon;
struct povray_mesh* povray_mesh_function_moon(void)
{
	return mesh_function(terrain);
}

extern "C" povray_mesh_fn povray_mesh_function_sea;
struct povray_mesh* povray_mesh_function_sea(void)
{
	return mesh_function(sea);
}

