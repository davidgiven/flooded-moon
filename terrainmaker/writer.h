/* TerrainMaker
 * © 2013 David Given
 *
 * This software is licensed under the Simplified BSD license. See COPYING
 * for the full text.
 */

#ifndef WRITER_H
#define WRITER_H

#include "matrix.h"
#include "texture.h"

class Writer
{
public:
	Writer():
		_hasTextures(false)
	{
	}

	virtual ~Writer()
	{
	}

	void clear();

	int addPoint(const Point& point);
	void addFace(const Point& va, const Point& vb, const Point& vc);
	void applyTextureData(Texture& texture);
	void calculateNormals();
	virtual void writeTo(const std::string& filename) = 0;

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

#endif

