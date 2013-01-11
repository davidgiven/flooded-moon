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
#include <iostream>
#include <fstream>
#include <sstream>

const double RADIUS = 1737.400;
const double SEALEVEL = -2;
const double ATMOSPHERE = 20;
const double FOV = 50;
const int SHMIXELS = 512;
const double SCALE = 1;

using std::min;
using std::max;

#include "utils.h"
#include "matrix.h"
#include "spheremap.h"
#include "terrain.h"
#include "meshwriter.h"
#include "camerawriter.h"

const double SHMIXELSTEP = FOV / (double)SHMIXELS;

static Vector* points;

static Vector& pointAt(int x, int y)
{
	return points[y*SHMIXELS + x];
}

static double intersect(const Ray& ray)
{
	/* d = -(l.D) +/- sqrt((l.D)^2 - DxD + r^2)
	 *
	 * where:
	 *
	 *   D = o - c
	 *   o = start of ray
	 *   l = direction of ray (normalised)
	 *   c = centre of sphere
	 *   r = radius of sphere
	 *
	 */

	const Vector& D = ray.point;
	double l_dot_D = ray.direction.dot(D);
	double D_squared = D.dot(D);

	double r = RADIUS;
	double sqrt_term = l_dot_D*l_dot_D - D_squared + r*r;
	double t0 = -l_dot_D + sqrt(sqrt_term);
	double t1 = -l_dot_D - sqrt(sqrt_term);

	if (t0 < 0)
		t0 = NAN;
	if (t1 < 0)
		t1 = NAN;

	if (t0 < t1)
		return t0;
	return t1;
}

static void write_terrain(std::ostream& s, int height)
{
	/* Each point may have a polygon below it and a polygon to the right of it:
	 *
	 *   A -- B
	 *   | \  |
	 *   |  \ |
	 *   C -- D
	 *
	 * ...except the last row and the last column. Therefore, there must be at
	 * most (SHMIXELS-1)^2 polygons.
	 */

	std::map<const Vector*, int> pointtoindex;
	std::map<int, const Vector*> indextopoint;
	for (int y=0; y<height; y++)
	{
		for (int x=0; x<SHMIXELS; x++)
		{
			const Vector& A = pointAt(x, y);
			if (A.isValid())
			{
				int i = pointtoindex.size();
				pointtoindex[&A] = i;
				indextopoint[i] = &A;
			}
		}
	}

	std::vector<const Vector*> facelist;
	for (int y=0; y<height-1; y++)
	{
		for (int x=0; x<SHMIXELS-1; x++)
		{
			const Vector& A = pointAt(x  , y  );
			const Vector& B = pointAt(x+1, y  );
			const Vector& C = pointAt(x  , y+1);
			const Vector& D = pointAt(x+1, y+1);

			if (A.isValid() && B.isValid() && D.isValid())
			{
				facelist.push_back(&A);
				facelist.push_back(&B);
				facelist.push_back(&D);
			}

			if (A.isValid() && D.isValid() && C.isValid())
			{
				facelist.push_back(&A);
				facelist.push_back(&D);
				facelist.push_back(&C);
			}
		}
	}

	MeshWriter writer;


	s << "ply\n"
		 "format ascii 1.0\n"
		 "element vertex " << indextopoint.size() << "\n"
		 "property float x\n"
		 "property float y\n"
		 "property float z\n"
		 "element face " << facelist.size()/3 << "\n"
		 "property list uchar int vertex_indices\n"
		 "end_header\n";

	/* Write out the vertices. */

	for (int i=0; i<indextopoint.size(); i++)
	{
		const Vector& v = *indextopoint[i];
		s << v.x*SCALE << " " << v.y*SCALE << " " << v.z*SCALE << "\n";

		writer.addPoint(v);
	}

	/* Write out the faces. */

	for (int i=0; i<facelist.size(); i+=3)
	{
		int a = pointtoindex[facelist[i+0]];
		int b = pointtoindex[facelist[i+1]];
		int c = pointtoindex[facelist[i+2]];

		s << "3 " << a << " " << b << " " << c << "\n";

		writer.addFace(a, b, c);
	}

	writer.writeTo("/tmp/moon.serialized");
}

static Vector mapToTerrain(const Terrain& terrain, const Vector& p)
{
	/* p is at the surface of our nominal sphere. */

	Vector np = p.normalise();
	double altitude = terrain.terrain(np);

	return np*altitude;
}

int main(int argc, const char* argv[])
{
	try
	{
		Terrain terrain("topography.pgm", "geoid.pgm");

		double latitude = 20.73;
		double longitude = -3.2;
		double altitude = SEALEVEL+5;
		double azimuth = -20;
		double bearing = 70;

		Transform view;
		view = view.lookAt(Vector::ORIGIN, Vector::Y, Vector::Z);
		view = view.rotate(Vector::Y, -longitude);
		view = view.rotate(Vector::X, -latitude);
		view = view.rotate(Vector::X, -90);
		view = view.translate(Vector(0, RADIUS+altitude, 0));
		view = view.rotate(Vector::Y, -bearing);
		view = view.rotate(Vector::X, 90 + azimuth);

		Vector camera = view.untransform(Vector::ORIGIN);
		CameraWriter().write("mitsuba/camera.xml", "mitsuba/camera.tmpl.xml",
				view, altitude);

		std::cerr << "height of terrain at camera is "
				<< terrain.terrain(camera.normalise()) - RADIUS
				<< "\n";

		double angle_to_bottom = azimuth + 90 + FOV/2;
		int shmixels_to_bottom = (int)(angle_to_bottom / SHMIXELSTEP);
		shmixels_to_bottom = max(shmixels_to_bottom, SHMIXELS);

		points = new Vector[SHMIXELS * shmixels_to_bottom];
		std::cerr << "outputing mesh of " << SHMIXELS << "x" << shmixels_to_bottom << " shmixels\n";

		Transform row = view.rotate(Vector::X, FOV/2 + SHMIXELSTEP/2);
		for (int y=0; y<shmixels_to_bottom; y++)
		{
			Transform m = row.rotate(Vector::Z, -FOV/2 + SHMIXELSTEP/2);

			for (int x=0; x<SHMIXELS; x++)
			{
				Vector p = m.untransform(Vector::Y);
				Vector dir = (p - camera).normalise();
				Ray ray(camera, dir);

				double t = intersect(ray);
				if (!isnan(t))
					pointAt(x, y) = mapToTerrain(terrain, ray.project(t));

	//			printf("dir for %dx%d: %f %f %f: %f %f %f\n", x, y, ray.direction.x, ray.direction.y, ray.direction.z, intersection.x, intersection.y, intersection.z);
				m = m.rotate(Vector::Z, SHMIXELSTEP);
			}

			std::cerr << y << "/" << shmixels_to_bottom << "\r";
			row = row.rotate(Vector::X, -SHMIXELSTEP);
		}
		std::cerr << "\n";

		write_terrain(std::cout, shmixels_to_bottom);
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
