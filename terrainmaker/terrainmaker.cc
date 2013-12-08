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
#include <unordered_map>
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

const double MAXHEIGHT = 22; // maximum height of any object on the surface
const double ATMOSPHERE = 20;
const double FOV = 50;

unsigned width, height;
double latitude = 20.0;
double longitude = -3.5;
double altitude = 5.0;
double azimuth = -10.0;
double bearing = 40.0;
double radius;
double sealevel;
std::string cameraf;
std::string topof;
std::string seatopof;
std::string propsf;
std::string heightmapf;
std::string seafuncf = "scripts/waterlevel.cal";
std::string terrainfuncf = "scripts/terrain.cal";
std::string texturefuncf = "scripts/texture.cal";
double shmixels = 100.0;

using std::min;
using std::max;
using std::auto_ptr;

namespace po = boost::program_options;

#include "utils.h"
#include "matrix.h"

Transform world;

#include "map.h"
#include "pdslabel.h"
#include "pds.h"
#include "pdsset.h"

PDSSet terrainpds;
PDSSet geoidpds;

#include "functions.h"
#include "terrain.h"
#include "sea.h"
#include "texture.h"
#include "writer.h"
#include "povwriter.h"
#include "plywriter.h"
#include "camerawriter.h"
#include "sphericalroam.h"
#include "propmaster.h"

static Point mapToTerrain(const Terrain& terrain, const Point& p)
{
	/* p is at the surface of our nominal sphere. */

	double f = terrain.at(p) / p.length();
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
	boost::gil::fill_pixels(view, 0x8000);

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
				sample = std::max(0.0, sample);
				sample = std::min(1.0, sample);

				view(x, y) = 65535 * sample;
			}
		}

	boost::gil::png_write_view(filename, view);

	std::cerr << "image function:\n"
              << "    v' = (v - 0.5) * "
			  << range << " + " << median
			  << "\n";
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
		("latitude", po::value<double>(&latitude),
				"latitude")
		("longitude", po::value<double>(&longitude),
				"longitude")
		("altitude", po::value<double>(&altitude),
				"altitude (above sea level)")
		("azimuth", po::value<double>(&azimuth),
				"azimuth (0 looks straight ahead; negative looks down)")
		("bearing", po::value<double>(&bearing),
				"bearing")
		("shmixels", po::value<double>(&shmixels),
				"terrain quality")
		("radius", po::value<double>(&radius),
				"average planetary radius")
		("sealevel", po::value<double>(&sealevel),
				"sealevel (relative to radius)")
		("geoid", po::value<std::vector<std::string>>()->composing(),
				"add a geoid PDS file (repeatable)")
		("terrain", po::value<std::vector<std::string>>()->composing(),
				"add a terrain PDS file (repeatable)")
		("seafunc", po::value(&seafuncf),
				"filename of Calculon script for calculating sea")
		("terrainfunc", po::value(&terrainfuncf),
				"filename of Calculon script for calculating terrain")
		("texturefunc", po::value(&texturefuncf),
				"filename of Calculon script for calculating texture")
		("camera", po::value<std::string>(&cameraf),
				"write camera info to specified file")
		("topo", po::value<std::string>(&topof),
				"generate land topography and write to specified file")
		("seatopo", po::value<std::string>(&seatopof),
				"generate ocean topography and write to specified file")
		("props", po::value<std::string>(&propsf),
				"generate props and write to specified file")
		("heightmap", po::value<std::string>(&heightmapf),
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

	initCalculon();

	try
	{
		if (vm.count("terrain") > 0)
		{
			for (auto s : vm["terrain"].as<std::vector<std::string>>())
				terrainpds.add(s);
		}

		if (vm.count("geoid") > 0)
		{
			for (auto s : vm["geoid"].as<std::vector<std::string>>())
				geoidpds.add(s);
		}

		Terrain terrain;
		Sea sea;
		Texture texture;

		/* Set up the camera position. */

		world = world.lookAt(Point::ORIGIN, Vector::Y, Vector::Z);
		world = world.rotate(Vector::Y, -longitude);
		world = world.rotate(Vector::X, -latitude);
		world = world.rotate(Vector::X, -90);
		world = world.translate(Vector(0, radius, 0));
		
		/* world is now relative to a point on a normalised sphere. Find out
		 * what sealevel is at this point and adjust the camera so it's
		 * relative to that. */

		{
			Point camera = world.untransform(Point::ORIGIN);
			double s = sea.at(camera);

			std::cerr << "sealevel at camera is " << s << "km\n";

			world = world.translate(Vector(0, s-radius+altitude, 0));
		}

		/* Adjust for pointing direction. */

		world = world.rotate(Vector::Y, -bearing);
		world = world.rotate(Vector::X, 90 + azimuth);

		if (!cameraf.empty())
		{
			std::cerr << "writing camera information to: "
			          << cameraf
					  << "\n";

			Point camera = world.untransform(Point::ORIGIN);
			if (boost::algorithm::ends_with(cameraf, ".xml"))
				CameraWriter().writeMitsuba(cameraf.c_str(), "mitsuba/camera.tmpl.xml");
			else if (boost::algorithm::ends_with(cameraf, ".py"))
				CameraWriter().writeBlender(cameraf.c_str());
			else
				CameraWriter().writePov(cameraf.c_str());

			double sea_at_camera = sea.at(camera);
			double height_of_camera = camera.length();

			std::cerr << "height of terrain at camera is "
					<< (height_of_camera - sea_at_camera)
					<< "\n";
		}

		if (!topof.empty())
		{
			std::cerr << "writing land topographic information to: "
			          << topof
					  << "\n";

			auto_ptr<Writer> writer(create_writer(topof));
			SphericalRoam(terrain, FOV / shmixels).writeTo(*writer);
			std::cerr << "calculating textures\n";
			writer->applyTextureData(texture);
			std::cerr << "calculating normals\n";
			writer->calculateNormals();
			std::cerr << "writing to file\n";
			writer->writeToFile();
		}

		if (!seatopof.empty())
		{
			std::cerr << "writing ocean topographic information to: "
			          << topof
					  << "\n";

			auto_ptr<Writer> writer(create_writer(seatopof));
			SphericalRoam(sea, FOV / (shmixels/10)).writeTo(*writer);
			std::cerr << "calculating normals\n";
			writer->calculateNormals();
			std::cerr << "writing to file\n";
			writer->writeToFile();
		}

		if (!propsf.empty())
		{
			std::cerr << "writing props information to: "
			          << cameraf
					  << "\n";

			auto_ptr<Writer> writer(create_writer(propsf));
			Propmaster(terrain, 13, 60).writeTo(*writer);
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
