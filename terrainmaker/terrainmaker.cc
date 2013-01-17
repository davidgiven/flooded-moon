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
#include <libnoise/noise.h>

const double RADIUS = 1737.400;
const double MAXHEIGHT = 22; // maximum height of any object on the surface
const double SEALEVEL = -2;
const double ATMOSPHERE = 20;
const double SHMIXELS = 150;
const double FOV = 50;

double latitude = 20.75;
double longitude = -2.2;//-2.8;
double altitude = SEALEVEL + 0.9;
double azimuth = -20;
double bearing = 50;

using std::min;
using std::max;

#include "utils.h"
#include "matrix.h"
#include "spheremap.h"
#include "terrain.h"
#include "meshwriter.h"
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

		Point camera = view.untransform(Point::ORIGIN);
		CameraWriter().write("mitsuba/camera.xml", "mitsuba/camera.tmpl.xml",
				view, altitude);

		std::cerr << "height of terrain at camera is "
				<< (terrain.terrain(camera) - RADIUS - SEALEVEL)
				<< "\n";

		SphericalRoam(view, terrain, FOV / SHMIXELS).writeTo("/tmp/moon.ply");
		Propmaster(view, terrain, 10, 60).writeTo("/tmp/props.ply");
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
