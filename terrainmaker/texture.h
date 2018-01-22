/* TerrainMaker
 * © 2013 David Given
 *
 * This software is licensed under the Simplified BSD license. See COPYING
 * for the full text.
 */

#ifndef TEXTURE_H
#define TEXTURE_H

#include "functions.h"

class Texture
{
public:
	Texture()
	{
	}

	/* Calculates the (u,v) pair for a point on the mesh. */

	void at(const Point& p, double& u, double& v) const
	{
		Compiler::Vector<3> pv;
		pv.x = p.x;
		pv.y = p.y;
		pv.z = p.z;

		(*textureFunc)(&pv, &u, &v);
	}
};

#endif

