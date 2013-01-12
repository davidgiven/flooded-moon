class MeshWriter
{
public:
	MeshWriter()
	{
	}

	void clear()
	{
		_points.clear();
		_faces.clear();
	}

	void addPoint(const Vector& v)
	{
		_points.push_back(v);
	}

	void addFace(int a, int b, int c)
	{
		Triangle t = {a, b, c};
		_faces.push_back(t);
	}

	void writeTo(const char* filename)
	{
		std::ofstream of;
		of.open(filename, std::ios::out);

		of << "ply\n"
			 "format ascii 1.0\n"
			 "element vertex " << _points.size() << "\n"
			 "property float x\n"
			 "property float y\n"
			 "property float z\n"
			 "element face " << _faces.size() << "\n"
			 "property list uchar int vertex_indices\n"
			 "end_header\n";

		std::cerr << _points.size() << " vertices and " << _faces.size() << " faces\n";

		for (int i=0; i<_points.size(); i++)
		{
			const Vector& v = _points[i];
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

	std::vector<Vector> _points;
	std::vector<Triangle> _faces;
};
