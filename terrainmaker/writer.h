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
		_hasTextures(false),
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
		_pointArray.resize(index+1);
		_pointMap[point] = index;

		PointData& pd = _pointArray[index];
		pd.point = point;
		pd.index = index;
		pd.u = pd.v = 0.0;

		return index;
	}

	void addFace(const Point& va, const Point& vb, const Point& vc)
	{
		assert(va.isValid());
		assert(vb.isValid());
		assert(vc.isValid());
		assert(va != vb);
		assert(vb != vc);
		assert(va != vc);

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

	void applyTextureData(Texture& texture)
	{
		for (PointData& pd : _pointArray)
		{
			texture.at(pd.point, pd.u, pd.v);
		}
		_hasTextures = true;
	}

	void calculateNormals()
	{
		typedef std::unordered_multimap<int, int> Polymap;
		Polymap polymap;

		for (int i=0; i<_faces.size(); i++)
		{
			Triangle& t = _faces[i];
			polymap.insert(Polymap::value_type(t.a, i));
			polymap.insert(Polymap::value_type(t.b, i));
			polymap.insert(Polymap::value_type(t.c, i));

			Vector edge1 = (_pointArray[t.b].point - _pointArray[t.a].point).normalise();
			Vector edge2 = (_pointArray[t.c].point - _pointArray[t.a].point).normalise();
			t.normal = edge1.cross(edge2).normalise();
		}

		for (int i=0; i<_pointArray.size(); i++)
		{
			Vector n(0.0, 0.0, 0.0);

			auto its = polymap.equal_range(i);
			int trianglecount = 0;
			for (auto it = its.first; it != its.second; it++)
			{
				int triangleindex = it->second;
				n = n + _faces[triangleindex].normal;
				trianglecount++;
			}

			if (trianglecount > 0)
			{
				n = n / trianglecount;
				_pointArray[i].normal = n;
			}
			else
				_pointArray[i].normal = Vector(0, 0, 0);

			assert(_pointArray[i].normal.isValid());
		}
	}

	virtual void writeTo(const char* filename) = 0;

protected:
	struct Triangle
	{
		int a, b, c;
		Vector normal;
	};

	struct PointData
	{
		Point point;
		int index;
		double u, v;
		Vector normal;
	};

	bool _hasTextures;
	std::string _filename;
	typedef std::map<Point, int> PointMap;
	PointMap _pointMap;

	std::vector<PointData> _pointArray;
	std::vector<Triangle> _faces;
};
