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

	double slope(const Point& p)
	{
		using std::min;
		using std::max;

		/* Important --- mapToSphere() returns METRES! */

		/* Direction vector scaled to ground level. */

		Vector location(p.x, p.y, p.z);

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

		p1 = mapToSphere(p1);
		p2 = mapToSphere(p2);
		p3 = mapToSphere(p3);

		/* Now calculate the normal of this triangle. */

		Vector normal = (p2-p3).normalise().cross((p1-p3).normalise());

		/* The dot product against the direction vector will be our slope (1
		 * horizontal, 0 vertical). */

		double slope = normal.normalise().dot(location.normalise());

		/* Adjust to something a little easier for scripts to handle. */

		double aslope = (1.0 - fabs(slope)) * 5.0;
		return max(min(aslope, 1.0), 0.0);
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
		Transform t;
		t = t.rotate(Vector::Y, -lat);
		t = t.rotate(Vector::Z, lon-90);
		Point p(1.0, 0.0, 0.0);
		p = t.transform(p);
		return at(p);
	}

	using Map::at;
};

