/* TerrainMaker
 * © 2013 David Given
 *
 * This software is licensed under the Simplified BSD license. See COPYING
 * for the full text.
 */

#include <assert.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <boost/algorithm/string/predicate.hpp>
#include <boost/iostreams/device/mapped_file.hpp>
#include <boost/iostreams/device/array.hpp>
#include "globals.h"
#include "functions.h"

/* Work around GIL bug where they haven't kept up to date with libpng
 * API changes. */

#include <png.h>
#if !defined(png_infopp_NULL)
	#define png_infopp_NULL (png_infopp)NULL
#endif
#if !defined(int_p_NULL)
	#define int_p_NULL (int*)NULL
#endif

#include <boost/gil/gil_all.hpp>
#include <boost/gil/extension/io/png_dynamic_io.hpp>
#include <boost/ptr_container/ptr_list.hpp>
#include <boost/format.hpp>
#include <boost/algorithm/string.hpp>

#include "utils.h"
#include "matrix.h"
#include "map.h"
#include "pdslabel.h"
#include "pds.h"
#include "pdsset.h"

#include "variables.h"
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
		return new PovWriter();
	if (boost::algorithm::ends_with(filename, ".ply"))
		return new PlyWriter();

	std::cout << "terrainmaker: couldn't figure out the type of '"
	          << filename << "'\n";
	exit(1);
}

static void pixel_to_lat_lon(int x, int y, double& lon, double& lat,
		unsigned width, unsigned height)
{
	lon = vars.heightmapleft + (double)(vars.heightmapright - vars.heightmapleft) * (double)x / (double)width;
	lat = vars.heightmaptop - (double)(vars.heightmaptop - vars.heightmapbottom) * (double)y / (double)height;
}

static void write_image_map(Map& map, const std::string& filename,
		unsigned width, unsigned height)
{
	boost::gil::gray16_image_t image(width, height);
	boost::gil::gray16_image_t::view_t view = boost::gil::view(image);
	boost::gil::fill_pixels(view, 0x8000);

	double min = +std::numeric_limits<double>::infinity();
	double max = -std::numeric_limits<double>::infinity();

	double data[height][width];
	bool found = false;
	int progress = 0;
	for (int y=0; y<height; y++)
	{
		int newprogress = (y*100)/height;
		if (newprogress != progress)
		{
			std::cerr << '\r' << newprogress << "%";
			progress = newprogress;
		}

		for (int x=0; x<width; x++)
		{
			double dx, dy;
			pixel_to_lat_lon(x, y, dx, dy, width, height);
			if (map.contains(dx, dy))
			{
				found = true;
				double v = map.at(dx, dy);
				min = std::min(min, v);
				max = std::max(max, v);
				data[y][x] = v;
			}
		}
	}
	std::cerr << '\n';

	if (!found)
		fatalError("area of coverage for heightmap contains no data, giving up");

	std::cerr << "minimum sample: " << min << "\n"
	          << "maximum sample: " << max << "\n";

	double median = (min+max)/2;
	double range = (max-min);

	std::cerr << "median sample:  " << median << "\n"
	          << "sample range:   " << range << "\n";

	for (int y=0; y<height; y++)
		for (int x=0; x<width; x++)
		{
			double dx, dy;
			pixel_to_lat_lon(x, y, dx, dy, width, height);

			if (map.contains(dx, dy))
			{
				double v = data[y][x];
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

	vars.parse(argc, argv);

	initCalculon();

	try
	{
		for (auto s : vars.terrain)
			terrainpds.add(s);

		for (auto s : vars.geoid)
			geoidpds.add(s);

		Terrain terrain;
		Sea sea;
		Texture texture;

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

		if (!vars.cameraf.empty())
		{
			std::cerr << "writing camera information to: "
			          << vars.cameraf
					  << "\n";

			Point camera = world.untransform(Point::ORIGIN);
			if (boost::algorithm::ends_with(vars.cameraf, ".xml"))
				CameraWriter().writeMitsuba(vars.cameraf.c_str(), "mitsuba/camera.tmpl.xml");
			else if (boost::algorithm::ends_with(vars.cameraf, ".py"))
				CameraWriter().writeBlender(vars.cameraf.c_str());
			else
				CameraWriter().writePov(vars.cameraf.c_str());

			double sea_at_camera = sea.at(camera);
			double terrain_at_camera = terrain.at(camera);
			double height_of_camera = camera.length();

			std::cerr << "height of terrain at camera is "
					<< (height_of_camera - sea_at_camera) << "km\n"
					<< "height above ground level is "
					<< (height_of_camera - terrain_at_camera) << "km\n"
				;
		}

		if (!vars.topof.empty())
		{
			std::cerr << "writing land topographic information to: "
			          << vars.topof
					  << "\n";

			unique_ptr<Writer> writer(create_writer(vars.topof));
			SphericalRoam(terrain, world, sealevel, vars.fov / vars.shmixels).writeTo(*writer);
			std::cerr << "calculating textures\n";
			writer->applyTextureData(texture);
			std::cerr << "calculating normals\n";
			writer->calculateNormals();
			std::cerr << "writing to file\n";
			writer->writeTo(vars.topof);
		}

		if (!vars.seatopof.empty())
		{
			std::cerr << "writing ocean topographic information to: "
			          << vars.topof
					  << "\n";

			unique_ptr<Writer> writer(create_writer(vars.seatopof));
			SphericalRoam(sea, world, sealevel, vars.fov / vars.shmixels).writeTo(*writer);
			std::cerr << "calculating normals\n";
			writer->calculateNormals();
			std::cerr << "writing to file\n";
			writer->writeTo(vars.seatopof);
		}

		if (!vars.propsf.empty())
		{
			std::cerr << "writing props information to: "
			          << vars.propsf
					  << "\n";

			std::ofstream of(vars.propsf);
			Propmaster(terrain, 13, vars.maxpropdistance).writeTo(of);
		}

		if (!vars.heightmapf.empty())
		{
			std::cerr << "writing heightmap to: "
			          << vars.heightmapf
					  << "\n";
			write_image_map(terrain, vars.heightmapf, vars.width, vars.height);
		}
	}
	catch (const std::exception& e)
	{
		std::cerr << "Error: " << e.what() << "\n";
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
