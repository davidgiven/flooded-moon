struct Vector
{
	static Vector ORIGIN;
	static Vector X;
	static Vector Y;
	static Vector Z;

	double x, y, z, w;

	Vector():
		x(NAN), y(NAN), z(NAN), w(NAN)
	{}

	Vector(double xx, double yy, double zz, double ww):
		x(xx), y(yy), z(zz), w(ww)
	{}

	Vector(double xx, double yy, double zz):
		x(xx), y(yy), z(zz), w(1.0)
	{}

	Vector(const Vector& o):
		x(o.x), y(o.y), z(o.z), w(o.w)
	{}

	Vector& operator = (const Vector& o)
	{
		x = o.x;
		y = o.y;
		z = o.z;
		w = o.w;
		return *this;
	}

	bool isValid() const
	{
		return !isnan(x);
	}

	Vector operator + (const Vector& o) const
	{
		return Vector(x + o.x, y + o.y, z + o.z, w + o.w);
	}

	Vector operator - (const Vector& o) const
	{
		return Vector(x - o.x, y - o.y, z - o.z, w - o.w);
	}

	Vector operator * (double f) const
	{
		return Vector(x*f, y*f, z*f, w*f);
	}

	Vector operator / (double f) const
	{
		return Vector(x/f, y/f, z/f, w/f);
	}

	double length() const
	{
		return sqrt(x*x + y*y + z*z);
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

Vector Vector::ORIGIN(0, 0, 0);
Vector Vector::X(1, 0, 0);
Vector Vector::Y(0, 1, 0);
Vector Vector::Z(0, 0, 1);

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

		return Vector(
			s(0,0)*v.x + s(0,1)*v.y + s(0,2)*v.z + s(0,3)*v.w,
			s(1,0)*v.x + s(1,1)*v.y + s(1,2)*v.z + s(1,3)*v.w,
			s(2,0)*v.x + s(2,1)*v.y + s(2,2)*v.z + s(2,3)*v.w,
			s(3,0)*v.x + s(3,1)*v.y + s(3,2)*v.z + s(3,3)*v.w
		);
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
    	Matrix result = *this;

    	int indxc[4], indxr[4];
    	int ipiv[4];
    	memset(ipiv, 0, sizeof(int)*4);

    	for (int i=0; i<4; i++)
    	{
    		int irow = -1, icol = -1;
    		double big = 0;
    		for (int j=0; j<4; j++)
    		{
    			if (ipiv[j] != 1)
    			{
    				for (int k=0; k<4; k++)
    				{
    					if (ipiv[k] == 0)
    					{
    						if (abs(result(j,k)) >= big)
    						{
    							big = abs(result(j,k));
    							irow = j;
    							icol = k;
    						}
    					}
    					assert(ipiv[k] <= 1); // uninvertible
    				}
    			}
    		}
    		++ipiv[icol];
    		if (irow != icol)
    		{
    			for (int k=0; k<4; ++k)
    				swap(result(irow, k), result(icol, k));
    		}
    		indxr[i] = irow;
    		indxc[i] = icol;
    		assert(result(icol, icol) != 0); // uninvertible
    		double pivinv = 1 / result(icol, icol);
    		result(icol, icol) = 1.f;
    		for (int j = 0; j < 4; j++)
    			result(icol, j) *= pivinv;
    		for (int j = 0; j < 4; j++) {
    			if (j != icol) {
    				double save = result(j, icol);
    				result(j, icol) = 0;
    				for (int k = 0; k < 4; k++)
    					result(j, k) -= result(icol, k)*save;
    			}
    		}
    	}
    	for (int j = 4-1; j >= 0; j--) {
    		if (indxr[j] != indxc[j]) {
    			for (int k = 0; k < 4; k++)
    				swap(result(k, indxr[j]), result(k, indxc[j]));
    		}
    	}

    	return result;
    }
};

Matrix Matrix::IDENTITY(
		1, 0, 0, 0,
		0, 1, 0, 0,
		0, 0, 1, 0,
		0, 0, 0, 1
	);

Matrix Matrix::ZERO(
		0, 0, 0, 0,
		0, 0, 0, 0,
		0, 0, 0, 0,
		0, 0, 0, 0
	);

struct Transform
{
	Matrix t;
	Matrix it;

	Transform():
		t(Matrix::IDENTITY), it(Matrix::IDENTITY)
	{
	}

	Transform(const Matrix& t, const Matrix& it):
		t(t), it(it)
	{
	}

	Transform(const Transform& o):
		t(o.t), it(o.it)
	{
	}

	Transform& operator = (const Transform& o)
	{
		t = o.t;
		it = o.it;
	}

	Vector transform(const Vector& v) const
	{
		return t * v;
	}

	Vector untransform(const Vector& v) const
	{
		return it * v;
	}

    Transform apply(const Matrix& o, const Matrix& io) const
    {
    	return Transform(o*t, it*io);
    }

    Transform apply(const Matrix& o) const
    {
    	return apply(o, o.invert());
    }

    Transform translate(const Vector& v) const
    {
    	return apply(
    		Matrix(
				1, 0, 0, v.x,
				0, 1, 0, v.y,
				0, 0, 1, v.z,
				0, 0, 0, 1
			),
			Matrix(
				1, 0, 0, -v.x,
				0, 1, 0, -v.y,
				0, 0, 1, -v.z,
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
			),
    		Matrix(
				1.0/v.x, 0,       0,       0,
				0,       1.0/v.y, 0,       0,
				0,       0,       1.0/v.z, 0,
				0,       0,       0,       1
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

    	return apply(t, t.transpose());
    }

    Transform lookAt(const Vector& p, const Vector& t, const Vector& up) const
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
