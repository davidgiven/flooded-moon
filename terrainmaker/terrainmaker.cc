/* TerrainMaker
 * © 2013 David Given
 *
 * This software is licensed under the Simplified BSD license. See COPYING
 * for the full text.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <assert.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <vector>
#include <map>
#include <list>
#include <set>
#include <iostream>
#include <fstream>
#include <sstream>
#include <boost/program_options.hpp>
#include <libnoise/noise.h>

const double RADIUS = 1737.400;
const double MAXHEIGHT = 22; // maximum height of any object on the surface
const double SEALEVEL = -2;
const double ATMOSPHERE = 20;
const double FOV = 50;

double latitude;
double longitude;
double altitude;
double azimuth;
double bearing;
std::string cameraf;
std::string topof;
std::string propsf;
double shmixels;

using std::min;
using std::max;

namespace po = boost::program_options;

#include "calculon.h"
typedef Calculon::Instance<Calculon::RealIsDouble> Compiler;
Compiler::StandardSymbolTable calculonSymbols;

#include "utils.h"
#include "matrix.h"
#include "spheremap.h"
#include "terrain.h"
#include "writer.h"
#include "povwriter.h"
#include "camerawriter.h"
#include "sphericalroam.h"
#include "propmaster.h"

static Point mapToTerrain(const Terrain& terrain, const Point& p)
{
	/* p is at the surface of our nominal sphere. */

	double f = terrain.terrain(p) / p.length();
	return Point(p.x*f, p.y*f, p.z*f);
}

int main(int argc, const char* argv[])
{
    po::options_description options("Allowed options");
    options.add_options()
		("help,h",
				"produce help message")
		("lat", po::value<double>()->default_value(20),
				"latitude")
		("lon", po::value<double>()->default_value(-3.5),
				"longitude")
		("altitude", po::value<double>()->default_value(5),
				"altitude (above sea level)")
		("azimuth", po::value<double>()->default_value(-10),
				"azimuth (0 looks straight ahead; negative looks down)")
		("bearing", po::value<double>()->default_value(40),
				"bearing")
		("shmixels", po::value<double>()->default_value(100),
				"terrain quality")
		("camera", po::value<std::string>()->default_value("/tmp/camera.inc"),
				"file to write camera information to")
		("topo", po::value<std::string>()->default_value("/tmp/moon.inc"),
				"file to write topographic information to")
		("props", po::value<std::string>()->default_value(""),
				"file to write prop information to")
	;

    po::variables_map vm;
    po::store(po::parse_command_line(argc, argv, options), vm);
    po::notify(vm);

    if (vm.count("help"))
    {
    	std::cout << "terrainmaker: makes terrain.\n"
    			  << options;
    	exit(1);
    }

    latitude = vm["lat"].as<double>();
    longitude = vm["lon"].as<double>();
    altitude = vm["altitude"].as<double>() + SEALEVEL;
    azimuth = vm["azimuth"].as<double>();
    bearing = vm["bearing"].as<double>();
	shmixels = vm["shmixels"].as<double>();
	cameraf = vm["camera"].as<std::string>();
	topof = vm["topo"].as<std::string>();
	propsf = vm["props"].as<std::string>();

    calculonSymbols.add("LATITUDE", latitude);
    calculonSymbols.add("LONGITUDE", longitude);
    calculonSymbols.add("ALTITUDE", altitude);
    calculonSymbols.add("AZIMUTH", azimuth);
    calculonSymbols.add("BEARING", bearing);
    calculonSymbols.add("SEALEVEL", SEALEVEL + RADIUS);

	try
	{
		Terrain terrain("topography.pgm", "geoid.pgm");

		Transform view;
		view = view.lookAt(Point::ORIGIN, Vector::Y, Vector::Z);
		view = view.rotate(Vector::Y, -longitude);
		view = view.rotate(Vector::X, -latitude);
		view = view.rotate(Vector::X, -90);
		view = view.translate(Vector(0, RADIUS+altitude, 0));
		view = view.rotate(Vector::Y, -bearing);
		view = view.rotate(Vector::X, 90 + azimuth);

		if (!cameraf.empty())
		{
			std::cerr << "writing camera information to: "
			          << cameraf
					  << "\n";

			Point camera = view.untransform(Point::ORIGIN);
			CameraWriter().writePov(cameraf.c_str(), view, altitude);

			std::cerr << "height of terrain at camera is "
					<< (terrain.terrain(camera) - RADIUS - SEALEVEL)
					<< "\n";
		}

		if (!topof.empty())
		{
			std::cerr << "writing topographic information to: "
			          << cameraf
					  << "\n";

			PovWriter writer;
			SphericalRoam(view, terrain, FOV / shmixels).writeTo(writer);
			writer.writeTo(topof.c_str());
		}

		if (!propsf.empty())
		{
			std::cerr << "writing props information to: "
			          << cameraf
					  << "\n";

			PovWriter writer;
			Propmaster(view, terrain, 13, 60).writeTo(writer);
			writer.writeTo(propsf.c_str());
		}
	}
	catch (const char* e)
	{
		std::cerr << "Error: " << e << "\n";
	}

#if 0
	Vector camera = ORIGIN;
	Vector wcamera = view*camera;
	printf("%f %f %f\n", wcamera.x, wcamera.y, wcamera.z);
#endif

#if 0
	double t = intersect(ray);
	printf("t=%f\n", t);

	Vector p = ray.project(t);
	printf("@ %f %f %f\n", p.x, p.y, p.z);
#endif

	return 0;
}
