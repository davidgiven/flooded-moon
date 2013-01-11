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

		int w = (int)(lon * _wscale);
		int h = (int)(lat * _hscale);

		assert(w >= 0);
		assert(w < _width);
		assert(h >= 0);
		assert(h < _height);

		int i;
		if (_depth < 0x100)
		{
			const unsigned char* p = _samples + ((h*_width) + w);
			i = *p;
		}
		else
		{
			const unsigned char* p = _samples + ((h*_width) + w) * 2;
			i = (p[0] << 8) + p[1];
		}
		return (double)i / (double)_depth;
	}
};