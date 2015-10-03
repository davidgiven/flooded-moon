#ifndef TERRAINLIB_NOISE_H
#define TERRAINLIB_NOISE_H

#include <libnoise/noise.h>

double easecurve(double x)
{
	return (x*x) / (x*x + (1.0-x)*(1.0-x));
}

double blend0lo(double x, double lo, double hi)
{
	if (x < lo)
		return 0.0;
	if (x < hi)
		return easecurve((x-lo) / (hi-lo));
	return 1.0;
}

double blend0hi(double x, double lo, double hi)
{
	if (x < lo)
		return 1.0;
	if (x < hi)
		return 1.0 - easecurve((x-lo) / (hi-lo));
	return 0.0;
}

double blend2(double x, double lo1, double hi1, double hi2, double lo2)
{
	if (x < lo1)
		return 0.0;
	if (x < hi1)
		return easecurve((x-lo1) / (hi1-lo1));
	if (x > hi2)
		return easecurve((lo2-x) / (lo2-hi2));
	return 1.0;
}

double perlin(const Vector& p)
{
	static noise::module::Perlin noise;
	return noise.GetValue(p.x, p.y, p.z);
}

double multifractal(const Vector& p, double frequency, double lacunarity, int octaves)
{
	static noise::module::RidgedMulti noise;
	noise.SetFrequency(frequency);
	noise.SetLacunarity(lacunarity);
	noise.SetOctaveCount(octaves);
	return noise.GetValue(p.x, p.y, p.z);
}

#endif

