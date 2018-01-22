/* TerrainMaker
 * © 2013 David Given
 *
 * This software is licensed under the Simplified BSD license. See COPYING
 * for the full text.
 */

/* This is a variant on the classic ROAM topography rendering algorithm.
 * It's largely stolen from Sean O'Neil's implementation, now mostly forgotten.
 *
 * http://www.gamasutra.com/view/feature/3042/a_realtime_procedural_universe_.php
 *
 * Copies of his actual code are:
 *
 * http://read.pudn.com/downloads179/doc/832241/GLDemo/ROAMSphere.h__.htm
 * http://read.pudn.com/downloads179/doc/832241/GLDemo/ROAMSphere.cpp__.htm
 *
 * (The original download has vanished.)
 *
 * This version is much simplified because we don't care about performance or
 * about doing it more than once, so we can simply ignore merging diamonds.
 */

#include "globals.h"
#include "matrix.h"
#include "sphericalroam.h"
#include "variables.h"

/* Each facet is arranged like this:
 *
 *       C
 *     / |
 *  h /  | o
 *   /   |
 *  /    |
 * A --- B
 *    a
 */

struct SphericalRoam::Facet
{
	Point pa;
	Point pb;
	Point pc;
	Facet* fa;
	Facet* fo;
	Facet* fh;

private:
	Point _midh;
	Point _realmidh;

public:
	Facet(const Point& pa, const Point& pb, const Point& pc):
			pa(pa), pb(pb), pc(pc),
			fa(NULL), fo(NULL), fh(NULL),
			_midh((pa.x+pc.x)/2, (pa.y+pc.y)/2, (pa.z+pc.z)/2)
	{
		assert(_midh != pa);
		assert(_midh != pb);
		assert(_midh != pc);
		assert(pa.isValid());
		assert(pb.isValid());
		assert(pc.isValid());
	}

	void setEdges(Facet* a, Facet* o, Facet* h)
	{
		fa = a;
		fo = o;
		fh = h;
	}

	void replaceEdge(Facet* edge, Facet* with)
	{
		if (fa == edge) { fa = with; return; }
		if (fo == edge) { fo = with; return; }
		if (fh == edge) { fh = with; return; }
		assert(false);
	}

	bool isDiamondWith(Facet* f)
	{
		return (fh == f) && (f->fh == this);
	}

	const Point& getRealMidh(const XYZMap& terrain)
	{
		if (!_realmidh.isValid())
		{
			_realmidh = terrain.mapToSphere(_midh);
			/* Occasionally _realmidh will coincide with a facet
			 * vertex, which causes bad stuff. Cope. */
			if ((_realmidh == pa) || (_realmidh == pb) || (_realmidh == pc))
				_realmidh = _midh;
		}
		return _realmidh;
	}
};

SphericalRoam::SphericalRoam(const XYZMap& terrain, const Transform& world,
		double sealevel, double error):
	_terrain(terrain),
	_camera(world.transform(Point::ORIGIN)),
	_sealevel(sealevel),
	_error(degToRad(error))
{
	Point p0 = _terrain.mapToSphere(Point(-1, 1, 1));
	Point p1 = _terrain.mapToSphere(Point(1, 1, 1));
	Point p2 = _terrain.mapToSphere(Point(1, -1, 1));
	Point p3 = _terrain.mapToSphere(Point(-1, -1, 1));
	Point p4 = _terrain.mapToSphere(Point(-1, 1, -1));
	Point p5 = _terrain.mapToSphere(Point(1, 1, -1));
	Point p6 = _terrain.mapToSphere(Point(1, -1, -1));
	Point p7 = _terrain.mapToSphere(Point(-1, -1, -1));

	Facet* f0 = addFacet(p1, p0, p3);
	Facet* f1 = addFacet(p3, p2, p1);
	Facet* f2 = addFacet(p5, p6, p7);
	Facet* f3 = addFacet(p7, p4, p5);
	Facet* f4 = addFacet(p0, p1, p5);
	Facet* f5 = addFacet(p5, p4, p0);
	Facet* f6 = addFacet(p3, p7, p6);
	Facet* f7 = addFacet(p6, p2, p3);
	Facet* f8 = addFacet(p0, p4, p7);
	Facet* f9 = addFacet(p7, p3, p0);
	Facet* fa = addFacet(p1, p2, p6);
	Facet* fb = addFacet(p6, p5, p1);

	f0->setEdges(f4, f9, f1);
	f1->setEdges(f7, fa, f0);
	f2->setEdges(fb, f6, f3);
	f3->setEdges(f8, f5, f2);
	f4->setEdges(f0, fb, f5);
	f5->setEdges(f3, f8, f4);
	f6->setEdges(f9, f2, f7);
	f7->setEdges(fa, f1, f6);
	f8->setEdges(f5, f3, f9);
	f9->setEdges(f6, f0, f8);
	fa->setEdges(f1, f7, fb);
	fb->setEdges(f2, f4, fa);

	/* Calculate the maximum sight distance. */

	Vector camerav = _camera.toVector();
	double cameradistance = _camera.length();
	std::cerr << "distance to core is " << cameradistance << "km\n"
			  << "sealevel here is " << _sealevel << "km\n";
	double tallest = MAXHEIGHT + sealevel;
	double horizon = sqrt(cameradistance*cameradistance - _sealevel*_sealevel);
	double maxsight = horizon + sqrt(tallest*tallest - _sealevel*_sealevel);
	double maxsightsquared = maxsight*maxsight;
	std::cerr << "distance to horizon is " << horizon <<
			"km; maximum sight distance is " << maxsight << "km\n";

	int i = 0;
	while (!_pendingFacets.empty())
	{
		double maxerror = 0.0;
		std::set<Facet*>::iterator iterator = _pendingFacets.begin();
		Facet* facet = *iterator;
		_pendingFacets.erase(iterator);

		bool dosplit = false;

		/* Do not split facets behind the camera. */

		Point va = world.transform(facet->pa);
		Point vb = world.transform(facet->pb);
		Point vc = world.transform(facet->pc);
		if (!vars.topoculling || ((va.y > 0) && (vb.y > 0) && (vc.y > 0)))
		{
			/* Is this facet over the maxsight? (Make an exception for very
			 * big facets.)
			 */

			Vector vab = facet->pa - facet->pb;
			Vector vac = facet->pa - facet->pc;
			Vector vbc = facet->pb - facet->pc;

			double distancesquared = va.lengthSquared();
			double sizesquared = vab.lengthSquared();

			/* Always consider facets which are very large. */

			if ((sizesquared > maxsightsquared) ||
					(distancesquared < maxsightsquared))
			{
				/* Calculate the apparent size of the facet. Don't split
				 * facets that are smaller than a certain absolute size
				 * to avoid degenerate cases. */

				double area = vab.cross(vac).length() / 2.0;
				if (area > (0.000500*0.000500))
				{
					Vector ca = va.toVector().normalise();
					Vector cb = vb.toVector().normalise();
					Vector cc = vc.toVector().normalise();
					double dab = acos(ca.dot(cb));
					double dac = acos(ca.dot(cc));
					double dbc = acos(cb.dot(cc));

					double mx = min(dab, dac, dbc);

					if (mx > _error)
						dosplit = true;
				}
			}
		}

		if (dosplit)
		{
			/* Consumes and destroys facet. */
			split(facet);
		}
		else
			_completedFacets.insert(facet);

		i++;
		if (!(i & 0xffff))
		{
			int cs = _completedFacets.size();
			int ss = _pendingFacets.size();
			std::cerr << "\r" << ss << "/" << cs << "   ";

#if 0
			if (cs > 1e6)
			{
				std::cerr << "\nToo much detail, bailing out\n";

				/* Add all remaining pending facets to completed facets
				 * list.
				 */

				while (!_pendingFacets.empty())
				{
					std::set<Facet*>::iterator iterator = _pendingFacets.begin();
					Facet* facet = *iterator;
					_pendingFacets.erase(iterator);
					_completedFacets.insert(facet);
				}

				break;
			}
#endif
		}
	}

	std::cerr << "\rfound " << _completedFacets.size() << " facets\n";
}

void SphericalRoam::writeTo(Writer& writer)
{
	for (std::set<Facet*>::const_iterator i = _completedFacets.begin(),
			e = _completedFacets.end(); i != e; i++)
	{
		Facet* f = *i;
		writer.addFace(f->pa, f->pb, f->pc);
	}
}

/* Discard a facet (remove it from both lists) */

void SphericalRoam::discard(Facet* facet)
{
	_pendingFacets.erase(facet);
	_completedFacets.erase(facet);
}

/* Splits a discarded facet; the new facets are queued on the pending list.
 * Any subsplits cause those facets to be discarded.
 */

void SphericalRoam::split(Facet* f)
{
	if (!f->isDiamondWith(f->fh))
	{
		/* The hypotenuse neighbour for this facet needs to be split. */

		discard(f->fh);
		split(f->fh);
	}
	assert(f->isDiamondWith(f->fh));

	/* Remove the neighbour. After this neither f nor n will be on a list.
	 * */

	Facet* n = f->fh;
	discard(n);

	/* Add the four new triangles. Right now we look like this:
	 *
	 *       a'
	 *    B'--- C'  A
	 *    |    /  / |
	 * o' |   /  /  | o
	 *    |  /  /   |
	 *    | /  /    |
	 *    A'  C --- B
	 *           a
	 */

	assert(f->pa == n->pc);
	assert(f->pc == n->pa);

	/*
	 * We're going to look like this:
	 *
	 *        o'
	 *    B'----- C'   A
	 *    |\ f2  /   / |
	 *    | \   /   /  |
	 *    |  \ /   /   |
	 *  a'|f3 m   m f1 | a
	 *    |  /   / \   |
	 *    | /   /   \  |
	 *    |/   / f0  \ |
	 *    A'  C ------ B
	 *            o
	 */

	const Point& m = f->getRealMidh(_terrain);
	assert(m != f->pa);
	assert(m != f->pb);
	assert(m != f->pc);

	Facet* f0 = addFacet(f->pc, m, f->pb);
	Facet* f1 = addFacet(f->pb, m, f->pa);
	Facet* f2 = addFacet(n->pc, m, n->pb);
	Facet* f3 = addFacet(n->pb, m, n->pa);

	f0->setEdges(f3, f1, f->fo);
	f1->setEdges(f0, f2, f->fa);
	f2->setEdges(f1, f3, n->fo);
	f3->setEdges(f2, f0, n->fa);

	f->fa->replaceEdge(f, f1);
	f->fo->replaceEdge(f, f0);
	n->fa->replaceEdge(n, f3);
	n->fo->replaceEdge(n, f2);

	/* f and n will never be used again. */

	delete f;
	delete n;
}

SphericalRoam::Facet* SphericalRoam::addFacet(const Point& pa, const Point& pb, const Point& pc)
{
	Facet* f = new Facet(pa, pb, pc);
	_pendingFacets.insert(f);
	return f;
}
