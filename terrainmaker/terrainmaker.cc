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
#include <deque>
#include <iostream>
#include <fstream>
#include <sstream>
#include <memory>
#include <boost/program_options.hpp>
#include <boost/algorithm/string/predicate.hpp>
#include <boost/iostreams/device/mapped_file.hpp>
#include <boost/iostreams/device/array.hpp>
#include <boost/gil/gil_all.hpp>
#include <boost/gil/extension/io/png_dynamic_io.hpp>
#include <boost/ptr_container/ptr_list.hpp>
#include <boost/format.hpp>
#include <boost/algorithm/string.hpp>
#include <libnoise/noise.h>

const double RADIUS = 1737.400;
const double MAXHEIGHT = 22; // maximum height of any object on the surface
const double SEALEVEL = -2;
const double ATMOSPHERE = 20;
const double FOV = 50;

unsigned width, height;
double latitude;
double longitude;
double altitude;
double azimuth;
double bearing;
std::string cameraf;
std::string topof;
std::string propsf;
std::string heightmapf;
double shmixels;

using std::min;
using std::max;
using std::auto_ptr;

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
#include "plywriter.h"
#include "camerawriter.h"
#include "sphericalroam.h"
#include "propmaster.h"
#include "pdslabel.h"
#include "pds.h"
#include "pdsset.h"

static Point mapToTerrain(const Terrain& terrain, const Point& p)
{
	/* p is at the surface of our nominal sphere. */

	double f = terrain.terrain(p) / p.length();
	return Point(p.x*f, p.y*f, p.z*f);
}

static Writer* create_writer(const std::string& filename)
{
	if (boost::algorithm::ends_with(filename, ".inc"))
		return new PovWriter(filename);
	if (boost::algorithm::ends_with(filename, ".ply"))
		return new PlyWriter(filename);

	std::cout << "terrainmaker: couldn't figure out the type of '"
	          << filename << "'\n";
	exit(1);
}

static void pixel_to_lat_lon(int x, int y, double& lon, double& lat,
		unsigned width, unsigned height)
{
	lon = 360.0 * (double)x / (double)width;
	lat = 90.0 - 180.0 * (double)y / (double)height;
}

static void write_image_map(Map& map, const std::string& filename,
		unsigned width, unsigned height)
{
	boost::gil::gray16_image_t image(width, height);
	boost::gil::gray16_image_t::view_t view = boost::gil::view(image);

	double min = +std::numeric_limits<double>::infinity();
	double max = -std::numeric_limits<double>::infinity();

	for (int y=0; y<view.height(); y++)
		for (int x=0; x<view.width(); x++)
		{
			double dx, dy;
			pixel_to_lat_lon(x, y, dx, dy, width, height);
			double v = map.at(dx, dy);
			if (map.contains(dx, dy))
			{
				min = std::min(min, v);
				max = std::max(max, v);
			}
		}

	std::cerr << "minimum sample: " << min << "\n"
	          << "maximum sample: " << max << "\n";

	double median = (min+max)/2;
	double range = (max-min);

	std::cerr << "median sample:  " << median << "\n"
	          << "sample range:   " << range << "\n";

	for (int y=0; y<view.height(); y++)
		for (int x=0; x<view.width(); x++)
		{
			double dx, dy;
			pixel_to_lat_lon(x, y, dx, dy, width, height);

			if (map.contains(dx, dy))
			{
				double v = map.at(dx, dy);

				double sample = 0.5 + (v - median) / range;
				assert(sample >= 0.0);
				assert(sample <= 1.0);

				view(x, y) = 65535 * sample;
			}
		}

	boost::gil::png_write_view(filename, view);
}

int main(int argc, const char* argv[])
{
	std::cerr.precision(10);

    po::options_description options("Allowed options");
    options.add_options()
		("help,h",
				"produce help message")
		("config", po::value<std::vector<std::string>>()->composing(),
				"read additional options from config file")
		("width", po::value<unsigned>(&width)->default_value(2048),
				"width of output image")
		("height", po::value<unsigned>(&height)->default_value(2048),
				"height of output image")
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
		("geoid", po::value<std::vector<std::string>>()->composing(),
				"add a geoid PDS file (repeatable)")
		("terrain", po::value<std::vector<std::string>>()->composing(),
				"add a terrain PDS file (repeatable)")
		("camera", po::value<std::string>()->default_value(""),
				"write camera info to specified file")
		("topo", po::value<std::string>()->default_value(""),
				"generate topography and write to specified file")
		("props", po::value<std::string>()->default_value(""),
				"generate props and write to specified file")
		("heightmap", po::value<std::string>(&heightmapf)->default_value(""),
				"generate cylindrical heightmap and write to specified file")
	;

	po::positional_options_description posoptions;
	posoptions.add("config", -1);

    po::variables_map vm;
    po::store(
		po::command_line_parser(argc, argv)
			.options(options)
			.positional(posoptions)
			.run(),
		vm);
    po::notify(vm);

    if (vm.count("help"))
    {
    	std::cout << "terrainmaker: makes terrain.\n"
    			  << options;
    	exit(1);
    }

	if (vm.count("config") > 0)
	{
		for (auto s : vm["config"].as<std::vector<std::string>>())
		{
			std::ifstream stream(s);
			po::store(po::parse_config_file(stream, options), vm);
		}
		po::notify(vm);
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
		PDSSet terrainpds;
		if (vm.count("terrain") > 0)
		{
			for (auto s : vm["terrain"].as<std::vector<std::string>>())
			{
				PDS* pds = PDS::LoadFromSpec(s);
				terrainpds.add(pds);
			}
		}

		PDSSet geoidpds;
		if (vm.count("geoid") > 0)
		{
			for (auto s : vm["geoid"].as<std::vector<std::string>>())
			{
				PDS* pds = PDS::LoadFromSpec(s);
				geoidpds.add(pds);
			}
		}

		Terrain terrain(terrainpds, geoidpds);

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
			if (boost::algorithm::ends_with(cameraf, ".xml"))
				CameraWriter().writeMitsuba(cameraf.c_str(), "mitsuba/camera.tmpl.xml", view, altitude);
			else if (boost::algorithm::ends_with(cameraf, ".py"))
				CameraWriter().writeBlender(cameraf.c_str(), view);
			else
				CameraWriter().writePov(cameraf.c_str(), view, altitude);

#if 0
			std::cerr << "height of terrain at camera is "
					<< (terrain.terrain(camera) - RADIUS - SEALEVEL)
					<< "\n";
#endif
		}

		if (!topof.empty())
		{
			std::cerr << "writing topographic information to: "
			          << cameraf
					  << "\n";

			auto_ptr<Writer> writer(create_writer(topof));
			SphericalRoam(view, terrain, FOV / shmixels).writeTo(*writer);
			writer->writeToFile();
		}

		if (!propsf.empty())
		{
			std::cerr << "writing props information to: "
			          << cameraf
					  << "\n";

			auto_ptr<Writer> writer(create_writer(propsf));
			Propmaster(view, terrain, 13, 60).writeTo(*writer);
			writer->writeToFile();
		}

		if (!heightmapf.empty())
		{
			std::cerr << "writing heightmap to: "
			          << heightmapf
					  << "\n";
			write_image_map(terrain, heightmapf, width, height);
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
