/* TerrainMaker
 * © 2013 David Given
 *
 * This software is licensed under the Simplified BSD license. See COPYING
 * for the full text.
 */

class Terrain : public XYZMap
{
public:
	Terrain()
	{
	}

private:
	noise::module::Perlin _noise;

	// Procedurally perturbs the terrain.

	double perturb(double altitude, const Point& p) const
	{
		double n = _noise.GetValue(p.x/1, p.y/1, p.z/1)*50;
		return n;
	}

public:
	bool contains(double lon, double lat) const
	{
		return terrainpds.contains(lon, lat) && geoidpds.contains(lon, lat);
	}

	/* Returns the altitude of the current perturbed location, in
	 * kilometres from core. */

	double at(const Point& p) const
	{
		Compiler::Vector<2> result;
		Compiler::Vector<3> v;
		v.x = p.x;
		v.y = p.y;
		v.z = p.z;

		(*terrainFunc)(&result, &v);
		return result.m[0];
	}

	using Map::at;
};
