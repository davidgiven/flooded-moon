/* TerrainMaker
 * © 2013 David Given
 *
 * This software is licensed under the Simplified BSD license. See COPYING
 * for the full text.
 */

#include "calculon.h"
typedef Calculon::Instance<Calculon::RealIsDouble> Compiler;
Compiler::StandardSymbolTable calculonSymbols;

typedef double SeaFunc(Compiler::Vector<3>* xyz);
Compiler::Program<SeaFunc>* seaFunc;

typedef void TerrainFunc(Compiler::Vector<2>* result, Compiler::Vector<3>* xyz);
Compiler::Program<TerrainFunc>* terrainFunc;

extern "C" double lookup_sea(Compiler::Vector<3>* xyz)
{
	return (*seaFunc)(xyz);
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
	calculonSymbols.add("PERLIN", "(vector*3): real", lookup_perlin);
	calculonSymbols.add("MULTIFRACTAL", "(vector*3, real, real, real): real", lookup_multifractal);

	{
		std::ifstream f(seafuncf);
		seaFunc = new Compiler::Program<SeaFunc>(calculonSymbols, f,
			"(XYZ: vector*3): real");
	}

	{
		std::ifstream f(terrainfuncf);
		terrainFunc = new Compiler::Program<TerrainFunc>(calculonSymbols, f,
			"(XYZ: vector*3): vector*2");
		
		terrainFunc->dump();
	}
}

