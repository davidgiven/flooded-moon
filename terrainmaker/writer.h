/* TerrainMaker
 * © 2013 David Given
 *
 * This software is licensed under the Simplified BSD license. See COPYING
 * for the full text.
 */

class Writer
{
public:
	Writer(const std::string& filename):
		_filename(filename)
	{
	}

	virtual ~Writer()
	{
	}

	void clear()
	{
		_pointMap.clear();
		_pointArray.clear();
		_faces.clear();
	}

	int addPoint(const Point& point)
	{
		PointMap::const_iterator i = _pointMap.find(point);
		if (i != _pointMap.end())
			return i->second;

		int index = _pointArray.size();
		_pointMap[point] = index;
		_pointArray.push_back(point);
		return index;
	}

	void addFace(const Point& va, const Point& vb, const Point& vc)
	{
		int pa = addPoint(va);
		int pb = addPoint(vb);
		int pc = addPoint(vc);

		Triangle t = {pa, pb, pc};
		_faces.push_back(t);
	}

	void writeToFile()
	{
		writeTo(_filename.c_str());
	}

	virtual void writeTo(const char* filename) = 0;

protected:
	struct Triangle
	{
		int a, b, c;
	};

	std::string _filename;
	typedef std::map<Point, int> PointMap;
	PointMap _pointMap;
	std::vector<Point> _pointArray;
	std::vector<Triangle> _faces;
};
