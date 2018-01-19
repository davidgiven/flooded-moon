/* TerrainMaker
 * © 2013 David Given
 *
 * This software is licensed under the Simplified BSD license. See COPYING
 * for the full text.
 */

#ifndef UTILS_H
#define UTILS_H

static inline double radToDeg(double value) { return value * (180.0f / M_PI); }
static inline double degToRad(double value) { return value * (M_PI / 180.0f); }

template <class T> static inline void swap(T& t1, T& t2)
{
	T t = t1;
	t1 = t2;
	t2 = t;
}

using std::min;
using std::max;

template <class T> static inline T min(const T& t1, const T& t2, const T& t3)
{
	return min(t1, min(t2, t3));
}

template <class T> static inline T max(const T& t1, const T& t2, const T& t3)
{
	return max(t1, max(t2, t3));
}

static inline double wrapf(double max, double value)
{
    if (value == 0.0)
        return 0.0;

    double m = value - max*floor(value/max);

    if (max > 0)
    {
        if (m >= max)
            return 0;

        if (m < 0)
        {
            if (max+m == max)
                return 0;
            else
                return max+m;
        }
    }
    else
    {
        if (m <= max)
            return 0;

        if (m > 0)
        {
            if (max+m == max)
                return 0;
            else
                return max+m;
        }
    }

    return m;
}

static inline double wrapf(double min, double max, double value)
{
	return wrapf(max-min, value-min) + min;
}

static inline double area_of_triangle(double a, double b, double c)
{
	double p = (a + b + c) / 2;
	return sqrt(p * (p-a) * (p-b) * (p-c));
}

static inline double randf()
{
	return (double)rand() / (double)RAND_MAX;
}

#endif

