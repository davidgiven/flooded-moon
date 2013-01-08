double radToDeg(double value) { return value * (180.0f / M_PI); }
double degToRad(double value) { return value * (M_PI / 180.0f); }

template <class T> void swap(T& t1, T& t2)
{
	T t = t1;
	t1 = t2;
	t2 = t;
}

