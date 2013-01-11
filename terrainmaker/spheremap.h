class SphereMap
{
public:
	SphereMap(const char* filename)
	{
		int fd = open(filename, O_RDONLY);
		if (fd == -1)
			throw "failed to open data file";

		struct stat st;
		fstat(fd, &st);

		const char* data = (const char*) mmap(NULL, st.st_size, PROT_READ, MAP_PRIVATE, fd, 0);
		if (data == MAP_FAILED)
			throw "failed to map data";

		if (sscanf(data, "P5 %d %d %d", &_width, &_height, &_depth) != 3)
			throw "malformed data file --- not a P5 PGM";
		std::cerr << filename << ": " << _width << "x" << _height << ", " << _depth << " shades of grey\n";

		_wscale = (double)_width / (2*M_PI);
		_hscale = (double)_height / M_PI;
		std::cerr << "(" << _wscale << "x" << _hscale << " pixels per radian)\n";

		_samples = (const unsigned char*) strchr(strchr(strchr(data, 10)+1, 10)+1, 10)+1;
	}

private:
	int _width, _height, _depth;
	double _wscale, _hscale;
	const unsigned char* _samples;

	double getsample(int x, int y) const
	{
		int i;
		if (_depth < 0x100)
		{
			const unsigned char* p = _samples + ((y*_width) + x);
			i = *p;
		}
		else
		{
			const unsigned char* p = _samples + ((y*_width) + x) * 2;
			i = (p[0] << 8) + p[1];
		}
		return (double)i / (double)_depth;
	}

public:
	double lookup(double lon, double lat) const
	{
		const double TWOPI = 2 * M_PI;

		if (lon < 0)
			lon += TWOPI;
		if (lon >= TWOPI)
			lon -= TWOPI;
		assert(lon >= 0);
		assert(lon < TWOPI);
		assert(lat >= 0);
		assert(lat < M_PI);

		double x = lon * _wscale;
		double y = lat * _hscale;

		assert(x >= 0);
		assert(x < _width);
		assert(y >= 0);
		assert(y < _height);

		double tl = getsample(floor(x), floor(y));
		double tr = getsample(ceil(x), floor(y));
		double bl = getsample(floor(x), ceil(y));
		double br = getsample(ceil(x), ceil(y));

		x = x - floor(x);
		y = y - floor(y);

		return ((1-x) * (1-y) * tl) +
				(x * (1-y) * tr) +
				((1-x) * y * bl) +
				(x * y * br);
	}
};
