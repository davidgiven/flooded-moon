/* TerrainMaker
 * © 2013 David Given
 *
 * This software is licensed under the Simplified BSD license. See COPYING
 * for the full text.
 */

double radToDeg(double value) { return value * (180.0f / M_PI); }
double degToRad(double value) { return value * (M_PI / 180.0f); }

template <class T> void swap(T& t1, T& t2)
{
	T t = t1;
	t1 = t2;
	t2 = t;
}

template <class T> static T min(const T& t1, const T& t2, const T& t3)
{
	return min(t1, min(t2, t3));
}

template <class T> static T max(const T& t1, const T& t2, const T& t3)
{
	return max(t1, max(t2, t3));
}

double area_of_triangle(double a, double b, double c)
{
	double p = (a + b + c) / 2;
	return sqrt(p * (p-a) * (p-b) * (p-c));
}

double randf()
{
	return (double)rand() / (double)RAND_MAX;
}
