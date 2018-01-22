/* TerrainMaker
 * © 2013 David Given
 *
 * This software is licensed under the Simplified BSD license. See COPYING
 * for the full text.
 */

#ifndef SPHERICALROAM_H
#define SPHERICALROAM_H

#include "map.h"
#include "writer.h"

class SphericalRoam
{
public:
	SphericalRoam(const XYZMap& terrain, const Transform& world,
			double sealevel, double error);

	void writeTo(Writer& writer);

private:
	const XYZMap& _terrain;
	const Point _camera;
	double _sealevel;
	double _error;

	struct Facet;
	std::set<Facet*> _pendingFacets;
	std::set<Facet*> _completedFacets;

	Facet* addFacet(const Point& pa, const Point& pb, const Point& pc);
	void discard(Facet* facet);
	void split(Facet* f);
};

#endif

