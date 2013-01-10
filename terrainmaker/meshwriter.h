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
		of.open(filename, std::ios::out | std::ios::binary);
		of.close();
	}

private:
	void write_u16(std::ofstream& of, u_int16_t v)
	{
		of << (u_int8_t) (v & 0xff);
		of << (u_int8_t) (v >> 8);
	}

	void write_u32(std::ofstream& of, u_int32_t v)
	{
		write_u16(of, v & 0xffff);
		write_u16(of, v >> 16);
	}

	void write_u64(std::ofstream& of, u_int64_t v)
	{
		write_u32(of, v & 0xffffffff);
		write_u32(of, v >> 32);
	}

	void write_double(std::ofstream& of, double d)
	{
		u_int8_t* p = (u_int8_t*) &d;

	}

private:
	struct Triangle
	{
		int a, b, c;
	};

	std::vector<Vector> _points;
	std::vector<Triangle> _faces;
};
