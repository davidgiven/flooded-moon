/* TerrainMaker
 * © 2013 David Given
 *
 * This software is licensed under the Simplified BSD license. See COPYING
 * for the full text.
 */

#include "globals.h"
#include "variables.h"
#include "functions.h"
#include "utils.h"
#include "matrix.h"
#include "map.h"
#include "pdslabel.h"
#include "pds.h"
#include "pdsset.h"
#include <libnoise/noise.h>

Compiler::StandardSymbolTable calculonSymbols;

Compiler::Program<MapFunc>* seaFunc;
Compiler::Program<MapFunc>* terrainFunc;
Compiler::Program<UVFunc>* textureFunc;
Compiler::Program<MapFunc>* propsFunc;

double lookup_sea(Compiler::Vector<3>* xyz)
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
	/* Direction vector scaled to ground level. */

	Vector location(xyz->x, xyz->y, xyz->z);
	return terrainpds.slope(location);
}

void initCalculon(void)
{
    calculonSymbols.add("LATITUDE", vars.latitude);
    calculonSymbols.add("LONGITUDE", vars.longitude);
    calculonSymbols.add("ALTITUDE", vars.altitude);
    calculonSymbols.add("AZIMUTH", vars.azimuth);
    calculonSymbols.add("BEARING", vars.bearing);
	calculonSymbols.add("RADIUS", vars.radius);
    calculonSymbols.add("SEALEVEL", vars.sealevel);

	calculonSymbols.add("SEA", "(vector*3): real", lookup_sea);
	calculonSymbols.add("TERRAIN", "(vector*3): real", lookup_terrain);
	calculonSymbols.add("GEOID", "(vector*3): real", lookup_geoid);
	calculonSymbols.add("SLOPE", "(vector*3): real", lookup_slope);
	calculonSymbols.add("PERLIN", "(vector*3): real", lookup_perlin);
	calculonSymbols.add("MULTIFRACTAL", "(vector*3, real, real, real): real", lookup_multifractal);

	{
		std::ifstream f;
		f.exceptions(std::ifstream::failbit);
		f.open(vars.seafuncf);
		seaFunc = new Compiler::Program<MapFunc>(calculonSymbols, f,
			"(XYZ: vector*3): (HEIGHT: real)");
	}

	{
		std::ifstream f;
		f.exceptions(std::ifstream::failbit);
		f.open(vars.terrainfuncf);
		terrainFunc = new Compiler::Program<MapFunc>(calculonSymbols, f,
			"(XYZ: vector*3): (HEIGHT: real)");
	}

	{
		std::ifstream f;
		f.exceptions(std::ifstream::failbit);
		f.open(vars.texturefuncf);
		textureFunc = new Compiler::Program<UVFunc>(calculonSymbols, f,
			"(XYZ: vector*3): (U: real, V: real)");
	}

	{
		std::ifstream f;
		f.exceptions(std::ifstream::failbit);
		f.open(vars.propsfuncf);
		propsFunc = new Compiler::Program<MapFunc>(calculonSymbols, f,
			"(XYZ: vector*3): (DENSITY: real)");
	}
}

