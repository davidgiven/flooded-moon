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

public:
	bool contains(double lon, double lat) const
	{
		return terrainpds.contains(lon, lat) && geoidpds.contains(lon, lat);
	}

	/* Returns the altitude of the current perturbed location, in
	 * kilometres from core. */

	double at(const Point& p) const
	{
		Compiler::Vector<3> v;
		v.x = p.x;
		v.y = p.y;
		v.z = p.z;

		double h;
		(*terrainFunc)(&v, &h);
		return h;
	}

	using Map::at;
};
