/* TerrainMaker
 * © 2013 David Given
 *
 * This software is licensed under the Simplified BSD license. See COPYING
 * for the full text.
 */

#ifndef MATRIX_H
#define MATRIX_H

#include "utils.h"

struct Vector
{
	static Vector X;
	static Vector Y;
	static Vector Z;

	double x, y, z;

	Vector():
		x(NAN), y(NAN), z(NAN)
	{}

	Vector(double xx, double yy, double zz):
		x(xx), y(yy), z(zz)
	{}

	Vector(const Vector& o):
		x(o.x), y(o.y), z(o.z)
	{}

	Vector& operator = (const Vector& o)
	{
		x = o.x;
		y = o.y;
		z = o.z;
		return *this;
	}

	bool operator< (const Vector& o) const
	{
		if (x < o.x) return true;
		if (x > o.x) return false;
		if (y < o.y) return true;
		if (y > o.y) return false;
		if (z < o.z) return true;
		if (z > o.z) return false;
		return false;
	}

	bool isValid() const
	{
		return !std::isnan(x);
	}

	bool operator == (const Vector& o) const
	{
		return (x == o.x) && (y == o.y) && (z == o.z);
	}

	bool operator != (const Vector& o) const
	{
		return (x != o.x) || (y != o.y) || (z != o.z);
	}

	Vector operator + (const Vector& o) const
	{
		return Vector(x + o.x, y + o.y, z + o.z);
	}

	Vector operator - (const Vector& o) const
	{
		return Vector(x - o.x, y - o.y, z - o.z);
	}

	Vector operator * (double f) const
	{
		return Vector(x*f, y*f, z*f);
	}

	Vector operator / (double f) const
	{
		return Vector(x/f, y/f, z/f);
	}

	double lengthSquared() const
	{
		return x*x + y*y + z*z;
	}

	double length() const
	{
		return sqrt(lengthSquared());
	}

	double dot(const Vector& o) const
	{
		return x*o.x + y*o.y + z*o.z;
	}

	double absDot(const Vector& o) const
	{
		return abs(this->dot(o));
	}

	Vector cross(const Vector& o) const
	{
		return Vector(
			y*o.z - z*o.y,
			z*o.x - x*o.z,
			x*o.y - y*o.x
		);
	}

	Vector normalise() const
	{
		double len = length();
		return Vector(x/len, y/len, z/len);
	}
};

struct Point
{
	static Point ORIGIN;

	double x, y, z;

	Point():
		x(NAN), y(NAN), z(NAN)
	{}

	Point(double xx, double yy, double zz):
		x(xx), y(yy), z(zz)
	{}

	Point(double* ptr):
		x(ptr[0]), y(ptr[1]), z(ptr[2])
	{}

	Point(const Vector& o):
		x(o.x), y(o.y), z(o.z)
	{}

	Point& operator = (const Vector& o)
	{
		x = o.x;
		y = o.y;
		z = o.z;
		return *this;
	}

	bool operator < (const Point& o) const
	{
		if (x < o.x) return true;
		if (x > o.x) return false;
		if (y < o.y) return true;
		if (y > o.y) return false;
		if (z < o.z) return true;
		if (z > o.z) return false;
		return false;
	}

	bool operator == (const Point& o) const
	{
		return (x == o.x) && (y == o.y) && (z == o.z);
	}

	bool operator != (const Point& o) const
	{
		return (x != o.x) || (y != o.y) || (z != o.z);
	}

	Vector toVector() const
	{
		return Vector(x, y, z);
	}

	bool isValid() const
	{
		return !std::isnan(x);
	}

	Point operator + (const Vector& o) const
	{
		return Point(x+o.x, y+o.y, z+o.z);
	}

	Point operator + (const Point& o) const
	{
		return Point(x+o.x, y+o.y, z+o.z);
	}

	Point operator - (const Vector& o) const
	{
		return Point(x-o.x, y-o.y, z-o.z);
	}

	Vector operator - (const Point& o) const
	{
		return Vector(x-o.x, y-o.y, z-o.z);
	}

	Vector operator * (double f) const
	{
		return Vector(x*f, y*f, z*f);
	}

	Vector operator / (double f) const
	{
		return Vector(x/f, y/f, z/f);
	}

	double lengthSquared() const
	{
		return x*x + y*y + z*z;
	}

	double length() const
	{
		return sqrt(lengthSquared());
	}
};

struct Matrix
{
private:
	Matrix() {}

public:
	double m[16];

	static Matrix ZERO;
	static Matrix IDENTITY;

	Matrix(double m00, double m10, double m20, double m30,
		   double m01, double m11, double m21, double m31,
		   double m02, double m12, double m22, double m32,
		   double m03, double m13, double m23, double m33)
	{
		m[ 0] = m00; m[ 1] = m10; m[ 2] = m20; m[ 3] = m30;
		m[ 4] = m01; m[ 5] = m11; m[ 6] = m21; m[ 7] = m31;
		m[ 8] = m02; m[ 9] = m12; m[10] = m22; m[11] = m32;
		m[12] = m03; m[13] = m13; m[14] = m23; m[15] = m33;
	}

	Matrix(double data[16])
	{
		memcpy(m, data, sizeof(m));
	}

	Matrix(const Matrix& o)
	{
		memcpy(m, o.m, sizeof(m));
	}

	Matrix& operator = (const Matrix& o)
	{
		memcpy(m, o.m, sizeof(m));
		return *this;
	}

	double& operator () (int y, int x)
	{
		return m[y*4 + x];
	}

	double operator () (int y, int x) const
	{
		return m[y*4 + x];
	}

    Vector operator * (const Vector& v) const
    {
    	const Matrix& s = *this;

		double x = s(0,0)*v.x + s(0,1)*v.y + s(0,2)*v.z + s(0,3);
		double y = s(1,0)*v.x + s(1,1)*v.y + s(1,2)*v.z + s(1,3);
		double z = s(2,0)*v.x + s(2,1)*v.y + s(2,2)*v.z + s(2,3);
		double w = s(3,0)*v.x + s(3,1)*v.y + s(3,2)*v.z + s(3,3);
		return Vector(x/w, y/w, z/w);
#if 0
		return Vector(
			s(0,0)*v.x + s(0,1)*v.y + s(0,2)*v.z + s(0,3)*v.w,
			s(1,0)*v.x + s(1,1)*v.y + s(1,2)*v.z + s(1,3)*v.w,
			s(2,0)*v.x + s(2,1)*v.y + s(2,2)*v.z + s(2,3)*v.w,
			s(3,0)*v.x + s(3,1)*v.y + s(3,2)*v.z + s(3,3)*v.w
		);
#endif
	}

    Matrix operator * (const Matrix& o) const
	{
    	Matrix result;
    	const Matrix& s = *this;

    	for (int i=0; i<4; i++)
    	{
    		for (int j=0; j<4; j++)
    		{
    			double sum = 0;
    			for (int k=0; k<4; k++)
    				sum += s(i,k) * o(k,j);
    			result(i,j) = sum;
    		}
    	}

    	return result;
	}

    Matrix transpose() const
    {
    	Matrix result = ZERO;

		for (int i=0; i<4; ++i)
			for (int j=0; j<4; ++j)
				result(i, j) = (*this)(j, i);

		return result;
    }

    Matrix invert() const
    {
    	double inv[16];

        inv[0] = m[5]  * m[10] * m[15] -
                 m[5]  * m[11] * m[14] -
                 m[9]  * m[6]  * m[15] +
                 m[9]  * m[7]  * m[14] +
                 m[13] * m[6]  * m[11] -
                 m[13] * m[7]  * m[10];

        inv[4] = -m[4]  * m[10] * m[15] +
                  m[4]  * m[11] * m[14] +
                  m[8]  * m[6]  * m[15] -
                  m[8]  * m[7]  * m[14] -
                  m[12] * m[6]  * m[11] +
                  m[12] * m[7]  * m[10];

        inv[8] = m[4]  * m[9] * m[15] -
                 m[4]  * m[11] * m[13] -
                 m[8]  * m[5] * m[15] +
                 m[8]  * m[7] * m[13] +
                 m[12] * m[5] * m[11] -
                 m[12] * m[7] * m[9];

        inv[12] = -m[4]  * m[9] * m[14] +
                   m[4]  * m[10] * m[13] +
                   m[8]  * m[5] * m[14] -
                   m[8]  * m[6] * m[13] -
                   m[12] * m[5] * m[10] +
                   m[12] * m[6] * m[9];

        inv[1] = -m[1]  * m[10] * m[15] +
                  m[1]  * m[11] * m[14] +
                  m[9]  * m[2] * m[15] -
                  m[9]  * m[3] * m[14] -
                  m[13] * m[2] * m[11] +
                  m[13] * m[3] * m[10];

        inv[5] = m[0]  * m[10] * m[15] -
                 m[0]  * m[11] * m[14] -
                 m[8]  * m[2] * m[15] +
                 m[8]  * m[3] * m[14] +
                 m[12] * m[2] * m[11] -
                 m[12] * m[3] * m[10];

        inv[9] = -m[0]  * m[9] * m[15] +
                  m[0]  * m[11] * m[13] +
                  m[8]  * m[1] * m[15] -
                  m[8]  * m[3] * m[13] -
                  m[12] * m[1] * m[11] +
                  m[12] * m[3] * m[9];

        inv[13] = m[0]  * m[9] * m[14] -
                  m[0]  * m[10] * m[13] -
                  m[8]  * m[1] * m[14] +
                  m[8]  * m[2] * m[13] +
                  m[12] * m[1] * m[10] -
                  m[12] * m[2] * m[9];

        inv[2] = m[1]  * m[6] * m[15] -
                 m[1]  * m[7] * m[14] -
                 m[5]  * m[2] * m[15] +
                 m[5]  * m[3] * m[14] +
                 m[13] * m[2] * m[7] -
                 m[13] * m[3] * m[6];

        inv[6] = -m[0]  * m[6] * m[15] +
                  m[0]  * m[7] * m[14] +
                  m[4]  * m[2] * m[15] -
                  m[4]  * m[3] * m[14] -
                  m[12] * m[2] * m[7] +
                  m[12] * m[3] * m[6];

        inv[10] = m[0]  * m[5] * m[15] -
                  m[0]  * m[7] * m[13] -
                  m[4]  * m[1] * m[15] +
                  m[4]  * m[3] * m[13] +
                  m[12] * m[1] * m[7] -
                  m[12] * m[3] * m[5];

        inv[14] = -m[0]  * m[5] * m[14] +
                   m[0]  * m[6] * m[13] +
                   m[4]  * m[1] * m[14] -
                   m[4]  * m[2] * m[13] -
                   m[12] * m[1] * m[6] +
                   m[12] * m[2] * m[5];

        inv[3] = -m[1] * m[6] * m[11] +
                  m[1] * m[7] * m[10] +
                  m[5] * m[2] * m[11] -
                  m[5] * m[3] * m[10] -
                  m[9] * m[2] * m[7] +
                  m[9] * m[3] * m[6];

        inv[7] = m[0] * m[6] * m[11] -
                 m[0] * m[7] * m[10] -
                 m[4] * m[2] * m[11] +
                 m[4] * m[3] * m[10] +
                 m[8] * m[2] * m[7] -
                 m[8] * m[3] * m[6];

        inv[11] = -m[0] * m[5] * m[11] +
                   m[0] * m[7] * m[9] +
                   m[4] * m[1] * m[11] -
                   m[4] * m[3] * m[9] -
                   m[8] * m[1] * m[7] +
                   m[8] * m[3] * m[5];

        inv[15] = m[0] * m[5] * m[10] -
                  m[0] * m[6] * m[9] -
                  m[4] * m[1] * m[10] +
                  m[4] * m[2] * m[9] +
                  m[8] * m[1] * m[6] -
                  m[8] * m[2] * m[5];

        double det = m[0] * inv[0] + m[1] * inv[4] + m[2] * inv[8] + m[3] * inv[12];

        if (det == 0)
            throw "uninvertable matrix";

        det = 1.0 / det;

        Matrix result;
        for (int i = 0; i < 16; i++)
        	result.m[i] = inv[i] * det;

    	return result;
    }
};

struct Transform
{
	Matrix t;

private:
	bool _itset;
	Matrix _it;

public:
	Transform():
		t(Matrix::IDENTITY),
		_itset(false),
		_it(Matrix::IDENTITY)
	{
	}

	Transform(const Matrix& t):
		t(t),
		_itset(false),
		_it(Matrix::IDENTITY)
	{
	}

	Transform(const Transform& o):
		t(o.t),
		_itset(false),
		_it(Matrix::IDENTITY)
	{
	}

	Transform& operator = (const Transform& o)
	{
		t = o.t;
		_itset = false;
		return *this;
	}

private:
	Point multiplyPoint(const Matrix& m, const Point& p) const
	{
		double x = m(0,0)*p.x + m(0,1)*p.y + m(0,2)*p.z + m(0,3);
		double y = m(1,0)*p.x + m(1,1)*p.y + m(1,2)*p.z + m(1,3);
		double z = m(2,0)*p.x + m(2,1)*p.y + m(2,2)*p.z + m(2,3);
		double w = m(3,0)*p.x + m(3,1)*p.y + m(3,2)*p.z + m(3,3);
		return Point(x/w, y/w, z/w);
	}

public:
	Point transform(const Point& p) const
	{
		return multiplyPoint(t, p);
	}

	const Matrix& inverse()
	{
		if (!_itset)
		{
			_it = t.invert();
			_itset = true;
		}
		return _it;
	}

	Point untransform(const Point& p)
	{
		return multiplyPoint(inverse(), p);
	}

    Transform apply(const Matrix& o) const
    {
    	return Transform(o*t);
    }

    Transform translate(const Vector& v) const
    {
    	return apply(
    		Matrix(
				1, 0, 0, v.x,
				0, 1, 0, v.y,
				0, 0, 1, v.z,
				0, 0, 0, 1
			)
		);
    }

    Transform scale(const Vector& v) const
    {
    	return apply(
    		Matrix(
				v.x, 0,   0,   0,
				0,   v.y, 0,   0,
				0,   0,   v.z, 0,
				0,   0,   0,   1
			)
		);
    }

    Transform scale(double d) const
    {
    	return scale(Vector(d, d, d));
    }

    Transform rotate(const Vector& v, double angle) const
    {
    	Vector vn = v.normalise();
    	double sinTheta, cosTheta;

    	sincos(degToRad(angle), &sinTheta, &cosTheta);

    	Matrix t(
			vn.x * vn.x + (1.0f - vn.x * vn.x) * cosTheta,
			vn.x * vn.y * (1.0f - cosTheta) - vn.z * sinTheta,
			vn.x * vn.z * (1.0f - cosTheta) + vn.y * sinTheta,
			0,

			vn.x * vn.y * (1.0f - cosTheta) + vn.z * sinTheta,
			vn.y * vn.y + (1.0f - vn.y * vn.y) * cosTheta,
			vn.y * vn.z * (1.0f - cosTheta) - vn.x * sinTheta,
			0,

			vn.x * vn.z * (1.0f - cosTheta) - vn.y * sinTheta,
			vn.y * vn.z * (1.0f - cosTheta) + vn.x * sinTheta,
			vn.z * vn.z + (1.0f - vn.z * vn.z) * cosTheta,
			0,

			0,
			0,
			0,
			1
		);

    	return apply(t);
    }

    Transform lookAt(const Point& p, const Point& t, const Vector& up) const
    {
    	Vector dir = (t-p).normalise();
    	Vector left = up.cross(dir).normalise();
    	Vector newUp = dir.cross(left);

    	return apply(
    		Matrix(
				left.x,  left.y,  left.z,  0,
				newUp.x, newUp.y, newUp.z, 0,
				dir.x,   dir.y,   dir.z,   0,
				p.x,     p.y,     p.z,     1
			)
		);
    }

    Transform perspective(double fov, double clipNear, double clipFar)
    {
		/* Project vectors in camera space onto a plane at z=1:
		 *
		 *  xProj = x / z
		 *  yProj = y / z
		 *  zProj = (far * (z - near)) / (z * (far-near))
		 *
		 *  Camera-space depths are not mapped linearly!
		 */
		double recip = 1.0f / (clipFar - clipNear);

		/* Perform a scale so that the field of view is mapped
		 * to the interval [-1, 1] */
		double cot = 1.0f / tan(degToRad(fov / 2.0f));

		return apply(
			Matrix(
				cot,  0,    0,   0,
				0,    cot,  0,   0,
				0,    0,    clipFar * recip, -clipNear * clipFar * recip,
				0,    0,    1,   0
			)
		);
	}
};

struct Ray
{
	Vector point;
	Vector direction;

	Ray(const Vector& p, const Vector& d):
		point(p), direction(d)
	{
	}

	Ray(const Ray& o):
		point(o.point), direction(o.direction)
	{
	}

	Ray& operator = (const Ray& o)
	{
		point = o.point;
		direction = o.direction;
		return *this;
	}

	Vector project(double t) const
	{
		return point + direction*t;
	}
};

extern Transform world;

#endif

