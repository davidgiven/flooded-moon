/* TerrainMaker
 * © 2013 David Given
 *
 * This software is licensed under the Simplified BSD license. See COPYING
 * for the full text.
 */

class MeshWriter
{
public:
	MeshWriter()
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

	void writeTo(const char* filename)
	{
		std::ofstream of;
		of.open(filename, std::ios::out);
		of.precision(10);
		of << std::scientific;

		of << "ply\n"
			 "format ascii 1.0\n"
			 "element vertex " << _pointArray.size() << "\n"
			 "property float x\n"
			 "property float y\n"
			 "property float z\n"
			 "element face " << _faces.size() << "\n"
			 "property list uchar int vertex_indices\n"
			 "end_header\n";

		std::cerr << _pointArray.size() << " vertices and " << _faces.size() << " faces\n";

		for (int i=0; i<_pointArray.size(); i++)
		{
			const Point& v = _pointArray[i];
			of << v.x << " " << v.y << " " << v.z << "\n";
		}

		for (int i=0; i<_faces.size(); i++)
		{
			const Triangle& t = _faces[i];
			of << "3 " << t.a << " " << t.b << " " << t.c << "\n";
		}
	}

private:
	struct Triangle
	{
		int a, b, c;
	};

	typedef std::map<Point, int> PointMap;
	PointMap _pointMap;
	std::vector<Point> _pointArray;
	std::vector<Triangle> _faces;
};
