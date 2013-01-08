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

const double RADIUS = 1737.400;
const double FOV = 60;
const int SHMIXELS = 256;
const double SCALE = 0.01;

#include "utils.h"
#include "matrix.h"
#include "terrain.h"

const double SHMIXELSTEP = FOV / (double)SHMIXELS;

static Vector points[SHMIXELS*SHMIXELS];

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

	double sqrt_term = l_dot_D*l_dot_D - D_squared + RADIUS*RADIUS;
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

static void write_terrain(std::ostream& s)
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
	for (int y=0; y<SHMIXELS; y++)
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
	for (int y=0; y<SHMIXELS-1; y++)
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


	s << "ply\n"
		 "format ascii 1.0\n"
		 "element vertex " << indextopoint.size() << "\n"
		 "property float x\n"
		 "property float y\n"
		 "property float z\n"
		 "element face " << facelist.size()/3 << "\n"
		 "property list uchar int vertex_index\n"
		 "end_header\n";

	/* Write out the vertices. */

	for (int i=0; i<indextopoint.size(); i++)
	{
		const Vector& v = *indextopoint[i];
		s << v.x*SCALE << " " << v.y*SCALE << " " << v.z*SCALE << "\n";
	}

	/* Write out the faces. */

	for (int i=0; i<facelist.size(); i+=3)
	{
		s << "3 "
		  << pointtoindex[facelist[i+0]] << " "
		  << pointtoindex[facelist[i+1]] << " "
		  << pointtoindex[facelist[i+2]] << "\n";
	}
}

static Vector mapToTerrain(const Terrain& terrain, const Vector& p)
{
	/* p is at the surface of our nominal sphere. */

	Vector np = p.normalise();
	double altitude = terrain.altitude(np);

	return np*altitude;
}

int main(int argc, const char* argv[])
{
	try
	{
		Terrain terrain("/home/dg/shared/workspace/flooded-moon/topography.pgm");

		double latitude = 20.73;
		double longitude = -3.8;
		double altitude = 1780;
		double azimuth = -10;
		double bearing = 90;

		Transform view;
		view = view.lookAt(Vector::ORIGIN, Vector::Y, Vector::Z);
		view = view.rotate(Vector::Y, -longitude);
		view = view.rotate(Vector::X, -latitude);
		view = view.rotate(Vector::X, -90);
		view = view.translate(Vector(0, altitude, 0));
		view = view.rotate(Vector::Y, -bearing);
		view = view.rotate(Vector::X, 90 + azimuth);

		Vector camera = view.untransform(Vector::ORIGIN);
		std::cerr << "camera at (" << camera.x << ", " << camera.y
				<< ", " << camera.z << ")\n";

		Vector forwards = (view.untransform(Vector::Y) - camera).normalise();
		std::cerr << "facing (" << forwards.x << ", " << forwards.y
				<< ", " << forwards.z << ")\n";

		Vector up = (view.untransform(Vector::Z) - camera).normalise();
		std::cerr << "up (" << up.x << ", " << up.y
				<< ", " << up.z << ")\n";

		Transform row = view.rotate(Vector::X, -FOV/2 + SHMIXELSTEP/2);
		for (int y=0; y<SHMIXELS; y++)
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

			std::cerr << y << "/" << SHMIXELS << "\r";
			row = row.rotate(Vector::X, SHMIXELSTEP);
		}
		std::cerr << "\n";

		write_terrain(std::cout);
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
