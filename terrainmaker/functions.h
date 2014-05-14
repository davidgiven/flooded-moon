/* TerrainMaker
 * © 2013 David Given
 *
 * This software is licensed under the Simplified BSD license. See COPYING
 * for the full text.
 */

#include "calculon.h"
typedef Calculon::Instance<Calculon::RealIsDouble> Compiler;
Compiler::StandardSymbolTable calculonSymbols;

typedef void MapFunc(Compiler::Vector<3>* xyz, double* height);
Compiler::Program<MapFunc>* seaFunc;
Compiler::Program<MapFunc>* terrainFunc;

typedef void UVFunc(Compiler::Vector<3>* pv, double* u, double* v);
Compiler::Program<UVFunc>* textureFunc;

Compiler::Program<MapFunc>* propsFunc;

extern "C" double lookup_sea(Compiler::Vector<3>* xyz)
{
	double height;
	(*seaFunc)(xyz, &height);
	return height;
}

extern "C" double lookup_terrain(Compiler::Vector<3>* xyz)
{
	Point p = {xyz->x, xyz->y, xyz->z};
	return terrainpds.at(p) / 1000.0;
}

extern "C" double lookup_geoid(Compiler::Vector<3>* xyz)
{
	Point p = {xyz->x, xyz->y, xyz->z};
	return geoidpds.at(p) / 1000.0;
}

extern "C" double lookup_perlin(Compiler::Vector<3>* xyz)
{
	static noise::module::Perlin noise;
	return noise.GetValue(xyz->x, xyz->y, xyz->z);
}

extern "C" double lookup_multifractal(Compiler::Vector<3>* xyz,
		double frequency, double lacunarity, double octaves)
{
	static noise::module::RidgedMulti noise;
	noise.SetFrequency(frequency);
	noise.SetLacunarity(lacunarity);
	noise.SetOctaveCount((int) octaves);
	return noise.GetValue(xyz->x, xyz->y, xyz->z);
}

extern "C" double lookup_slope(Compiler::Vector<3>* xyz)
{
	/* Important --- mapToSphere() returns METRES! */

	/* Direction vector scaled to ground level. */

	Vector location(xyz->x, xyz->y, xyz->z);

	/* Construct a magic base vector based on the element with the
	 * lowest magnitude. */

	double ax = fabs(location.x);
	double ay = fabs(location.y);
	double az = fabs(location.z);
	Vector base(0, 0, 0);
	if ((ax < ay) && (ax < az))
		base.x = 1;
	else if ((ay < ax) && (ay < az))
		base.y = 1;
	else
		base.z = 1;

	/* The cross product of this and the direction vector will be
	 * orthogonal to the direction vector. */

	Vector vp = base.cross(location).normalise();

	/* Use this to construct a point which is a certain distance away
	 * from the original point (in a random direction).
	 * (Remember that location is in kilometres. */

	Point p1 = Point(vp*0.100) + location;

	/* Create two more points based on this, equally spaced around
	 * the rotation vector. */

	Transform t = Transform().rotate(location, 60);
	Point p2 = t.transform(p1);
	Point p3 = t.transform(p2);

	/* Map them onto the terrain. */

	p1 = terrainpds.mapToSphere(p1);
	p2 = terrainpds.mapToSphere(p2);
	p3 = terrainpds.mapToSphere(p3);

	/* Now calculate the normal of this triangle. */

	Vector normal = (p2-p3).normalise().cross((p1-p3).normalise());

	/* The dot product against the direction vector will be our slope (1
	 * horizontal, 0 vertical). */

	double slope = normal.normalise().dot(location.normalise());

	/* Adjust to something a little easier for scripts to handle. */

	double aslope = (1.0 - fabs(slope)) * 5.0;
	return max(min(aslope, 1.0), 0.0);
}

void initCalculon(void)
{
    calculonSymbols.add("LATITUDE", latitude);
    calculonSymbols.add("LONGITUDE", longitude);
    calculonSymbols.add("ALTITUDE", altitude);
    calculonSymbols.add("AZIMUTH", azimuth);
    calculonSymbols.add("BEARING", bearing);
	calculonSymbols.add("RADIUS", radius);
    calculonSymbols.add("SEALEVEL", sealevel);

	calculonSymbols.add("SEA", "(vector*3): real", lookup_sea);
	calculonSymbols.add("TERRAIN", "(vector*3): real", lookup_terrain);
	calculonSymbols.add("GEOID", "(vector*3): real", lookup_geoid);
	calculonSymbols.add("SLOPE", "(vector*3): real", lookup_slope);
	calculonSymbols.add("PERLIN", "(vector*3): real", lookup_perlin);
	calculonSymbols.add("MULTIFRACTAL", "(vector*3, real, real, real): real", lookup_multifractal);

	{
		std::ifstream f;
		f.exceptions(std::ifstream::failbit);
		f.open(seafuncf);
		seaFunc = new Compiler::Program<MapFunc>(calculonSymbols, f,
			"(XYZ: vector*3): (HEIGHT: real)");
	}

	{
		std::ifstream f;
		f.exceptions(std::ifstream::failbit);
		f.open(terrainfuncf);
		terrainFunc = new Compiler::Program<MapFunc>(calculonSymbols, f,
			"(XYZ: vector*3): (HEIGHT: real)");
	}

	{
		std::ifstream f;
		f.exceptions(std::ifstream::failbit);
		f.open(texturefuncf);
		textureFunc = new Compiler::Program<UVFunc>(calculonSymbols, f,
			"(XYZ: vector*3): (U: real, V: real)");
	}

	{
		std::ifstream f;
		f.exceptions(std::ifstream::failbit);
		f.open(propsfuncf);
		propsFunc = new Compiler::Program<MapFunc>(calculonSymbols, f,
			"(XYZ: vector*3): (DENSITY: real)");
	}
}

