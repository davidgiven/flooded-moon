/* TerrainMaker
 * © 2013 David Given
 *
 * This software is licensed under the Simplified BSD license. See COPYING
 * for the full text.
 */

class Map
{
public:
	virtual ~Map() {}

	virtual bool contains(double lon, double lat) const = 0;
	virtual double at(double lon, double lat) const = 0;
	virtual double at(const Point& v) const = 0;

	Point mapToSphere(const Point& p) const
	{
		return mapToSphere(p, at(p));
	}

	Point mapToSphere(const Point& p, double radius) const
	{
		double f = radius / p.length();
		return Point(p.x*f, p.y*f, p.z*f);
	}
};

class LatLongMap : public Map
{
public:
	/* Returns the sample at a location specified via a point on
	 * a sphere (radius irrelevant). */

	double at(const Point& v) const
	{
		static Transform t = Transform().rotate(Vector::X, -90);
		Point vv = t.transform(v);

		double r = vv.length();
		double lat = radToDeg(acos(vv.y / r)) - 90.0;
		double lon = radToDeg(atan2(vv.z, vv.x));

		lat = -lat;
		lon = 90.0 - lon;

		return at(lon, lat);
	}

	using Map::at;
};

class XYZMap : public Map
{
public:
	double at(double lon, double lat) const
	{
		assert(false);
		return 0.0;
	}

	using Map::at;
};

