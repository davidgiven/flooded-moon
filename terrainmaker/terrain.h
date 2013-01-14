class Terrain
{
public:
	Terrain(const char* topography, const char* geoid):
		_terrain(topography),
		_geoid(geoid)
	{
	}

private:
	SphereMap _terrain;
	SphereMap _geoid;
	noise::module::Perlin _noise;

public:
	// The geoid data has range 1081.1 and offset 525.3. SphereMap reads it out of
	// the file as unit-scaled values, so we actually get the elevation with:
	//
	//   elevation = (sample * 1081.1) - 525.3
	//
	// The units are in metres.
	//
	// Note, however, that the geoid imagemap is from -180 to +179 longitude on
	// the X axis, while the topography map is from 0 to +359 longitude.

	double geoid(double lon, double lat) const
	{
		double d = _geoid.lookup(lon - M_PI, lat);
		return (d * 1081.1) - 525.3;
	}

	// Returns the altitude of a location on the moon, in metres.

	double altitude(double lon, double lat) const
	{
		double d = _terrain.lookup(lon, lat);
		return (d - 0.5) * 22000.0;
	}

	// Procedurally perturbs the terrain.

	double perturb(double altitude, const Point& p) const
	{
		double n = _noise.GetValue(p.x/1, p.y/1, p.z/1)*50;
		return n;
	}

	double terrain(const Point& v) const
	{
		static Transform t = Transform().rotate(Vector::X, -90);
		Point vv = t.transform(v);

		double r = vv.length();
		double lat = acos(vv.y / r);
		double lon = atan2(vv.z, vv.x);

		//lat -= M_PI_2;
		lon = M_PI_2 - lon;

		//return sin(lon*20)*50 + RADIUS;
		double m = altitude(lon, lat) - geoid(lon, lat);
//		m = 0;
		double alt = m/1000.0 + RADIUS;
		Point nv(vv.x*alt/r, vv.y*alt/r, vv.z*alt/r);

		alt += perturb(m, nv) / 1000.0;
		return alt;
	}

	Point mapToTerrain(const Point& p) const
	{
		double f = terrain(p) / p.length();
		return Point(p.x*f, p.y*f, p.z*f);
	}
};
