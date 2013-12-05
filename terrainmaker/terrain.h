/* TerrainMaker
 * © 2013 David Given
 *
 * This software is licensed under the Simplified BSD license. See COPYING
 * for the full text.
 */

class Terrain : public Map
{
public:
	Terrain(const Map& terrain, const Map& geoid):
		_terrain(terrain),
		_geoid(geoid)
	{
	}

private:
	const Map& _terrain;
	const Map& _geoid;
	noise::module::Perlin _noise;

	// Returns the radius of the current geoid, in metres from core.

	double geoid(double lon, double lat) const
	{
		return _geoid.at(lon, lat);
	}

	// Returns the radius of a location, in metres from core.

	double terrain(double lon, double lat) const
	{
		return _terrain.at(lon, lat);
	}

	// Procedurally perturbs the terrain.

	double perturb(double altitude, const Point& p) const
	{
		double n = _noise.GetValue(p.x/1, p.y/1, p.z/1)*50;
		return n;
	}

public:
	/* Returns the radius of the current perturbed location,
	 * specified as a point on a sphere (radius irrelevant), in
	 * metres from core. */

	double terrain(const Point& v) const
	{
		static Transform t = Transform().rotate(Vector::X, -90);
		Point vv = t.transform(v);

		double r = vv.length();
		double lat = acos(vv.y / r);
		double lon = atan2(vv.z, vv.x);

		//lat -= M_PI_2;
		lon = M_PI_2 - lon;

		return at(radToDeg(lon), radToDeg(lat));
	}

	bool contains(double lon, double lat) const
	{
		return _terrain.contains(lon, lat);
	}

	/* Returns the altitude of the current perturbed location, in metres
	 * from core. */

	double at(double lon, double lat) const
	{
		if (!contains(lon, lat))
			return at(0, 0);

		double m = terrain(lon, lat);
//		m = 0;
//		double alt = m/1000.0 + RADIUS;
//		Point nv(vv.x*alt/r, vv.y*alt/r, vv.z*alt/r);

//		alt += perturb(m, nv) / 1000.0;
		return m;
	}

	Point mapToTerrain(const Point& p) const
	{
		return mapToTerrain(p, terrain(p));
	}

	Point mapToTerrain(const Point& p, double altitude) const
	{
		double f = altitude / p.length();
		return Point(p.x*f, p.y*f, p.z*f);
	}
};
