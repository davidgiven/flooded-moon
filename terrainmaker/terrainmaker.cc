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
const double SHMIXELS = 50; //150;
const double FOV = 50;

double latitude = 20.73;
double longitude = -3.2;
double altitude = 0.5;
double azimuth = -30;
double bearing = 70;

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

class Generator
{
public:
	Generator(MeshWriter& writer, const Transform& view, const Terrain& terrain):
		_writer(writer),
		_view(view),
		_terrain(terrain),
		_minangle(degToRad(FOV / SHMIXELS))
	{
	}

	void commitFacet(const Point& va, const Point& vb, const Point& vc)
	{
		_writer.addFace(vc, vb, va);
	}

	void subdivide(const Point& va, const Point& vb, const Point& vc)
	{
		Point vab = Vector(va.x+vb.x, va.y+vb.y, va.z+vb.z).normalise() * RADIUS;
		Point vbc = Vector(vb.x+vc.x, vb.y+vc.y, vb.z+vc.z).normalise() * RADIUS;
		Point vca = Vector(vc.x+va.x, vc.y+va.y, vc.z+va.z).normalise() * RADIUS;

		facet(va, vab, vca);
		facet(vb, vbc, vab);
		facet(vc, vca, vbc);
		facet(vab, vbc, vca);
	}

	void facet(const Point& va, const Point& vb, const Point& vc)
	{
		Point ta = mapToTerrain(_terrain, va);
		Point tb = mapToTerrain(_terrain, vb);
		Point tc = mapToTerrain(_terrain, vc);

		Vector ca = _view.transform(ta).toVector().normalise();
		Vector cb = _view.transform(tb).toVector().normalise();
		Vector cc = _view.transform(tc).toVector().normalise();

		double dab = ca.dot(cb);
		double dac = ca.dot(cc);
		double dbc = cb.dot(cc);

		double a = min(acos(dab), min(acos(dac), acos(dbc)));

		if (a < _minangle)
			commitFacet(ta, tb, tc);
		else
			subdivide(ta, tb, tc);
	}

	void icosahedron()
	{
		double x = 0.525731112119133606 * RADIUS;
		double z = 0.850650808352039932 * RADIUS;

		Point v0(-x, 0, z);
		Point v1(x, 0, z);
		Point v2(-x, 0, -z);
		Point v3(x, 0, -z);
		Point v4(0, z, x);
		Point v5(0, z, -x);
		Point v6(0, -z, x);
		Point v7(0, -z, -x);
		Point v8(z, x, 0);
		Point v9(-z, x, 0);
		Point va(z, -x, 0);
		Point vb(-z, -x, 0);

		facet(v0, v4, v1);
		facet(v0, v9, v4);
		facet(v9, v5, v4);
		facet(v4, v5, v8);
		facet(v4, v8, v1);

		facet(v8, va, v1);
		facet(v8, v3, va);
		facet(v5, v3, v8);
		facet(v5, v2, v3);
		facet(v2, v7, v3);

		facet(v7, va, v3);
		facet(v7, v6, va);
		facet(v7, vb, v6);
		facet(vb, v0, v6);
		facet(v0, v1, v6);

		facet(v6, v1, va);
		facet(v9, v0, vb);
		facet(v9, vb, v2);
		facet(v9, v2, v5);
		facet(v7, v2, vb);
	}

private:
	MeshWriter& _writer;
	const Transform& _view;
	const Terrain& _terrain;
	double _minangle;
};

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

		MeshWriter writer;
		SphericalRoam(view, terrain, FOV / SHMIXELS).writeTo(writer);
		writer.writeTo("/tmp/moon.ply");

		Propmaster(view, terrain, 5, 5);
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
