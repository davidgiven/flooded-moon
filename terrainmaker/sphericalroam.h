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

class SphericalRoam
{
private:
	class Facet;

public:
	SphericalRoam(const Transform& view, const Terrain& terrain, double error):
		_view(view),
		_terrain(terrain),
		_error(degToRad(error))
	{
		Point p0 = _terrain.mapToTerrain(Point(-1, 1, 1));
		Point p1 = _terrain.mapToTerrain(Point(1, 1, 1));
		Point p2 = _terrain.mapToTerrain(Point(1, -1, 1));
		Point p3 = _terrain.mapToTerrain(Point(-1, -1, 1));
		Point p4 = _terrain.mapToTerrain(Point(-1, 1, -1));
		Point p5 = _terrain.mapToTerrain(Point(1, 1, -1));
		Point p6 = _terrain.mapToTerrain(Point(1, -1, -1));
		Point p7 = _terrain.mapToTerrain(Point(-1, -1, -1));

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

		double r = radius + sealevel;
		double cameradistance = view.transform(Point::ORIGIN).length();
		std::cerr << "distance to core is " << cameradistance << "km\n";
		double tallest = MAXHEIGHT + r;
		double horizon = sqrt(cameradistance*cameradistance - r*r);
		double maxsight = horizon + sqrt(tallest*tallest - r*r);
		double maxsightsquared = maxsight*maxsight;
		std::cerr << "distance to horizon is " << horizon <<
				"km; maximum sight distance is " << maxsight << "km\n";

		int i = 0;
		while (!_pendingFacets.empty())
		{
			std::set<Facet*>::iterator iterator = _pendingFacets.begin();
			Facet* facet = *iterator;
			_pendingFacets.erase(iterator);

			/* Is this facet over the maxsight? (Make an exception for very
			 * big facets.)
			 */

			Point va = view.transform(facet->pa);
			Point vb = view.transform(facet->pb);
			Point vc = view.transform(facet->pc);

			double distancesquared = va.lengthSquared();
			double sizesquared = (facet->pa - facet->pb).lengthSquared();
			if ((sizesquared > maxsightsquared) ||
					(distancesquared < maxsightsquared))
			{
				/* Calculate the apparent size of the facet. */

				Vector ca = va.toVector().normalise();
				Vector cb = vb.toVector().normalise();
				Vector cc = vc.toVector().normalise();

				double dab = ca.dot(cb);
				double dac = ca.dot(cc);
				double dbc = cb.dot(cc);

				double a = min(acos(dab), min(acos(dac), acos(dbc)));
				if (a > _error)
				{
					/* Split this facet. */
					split(facet);
				}
				else
				{
					/* Otherwise, don't bother with it. */
					_completedFacets.insert(facet);
				}
			}
			else
			{
				/* Over maxsight facets just get dropped on the floor,
				 * neither completed nor pending.
				 */
			}

			i++;
			if (!(i & 0xffff))
			{
				int s = _completedFacets.size();
				std::cerr << "\r" << s << "  ";

#if 0
				if (s > 1e6)
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

	void writeTo(Writer& writer)
	{
		for (std::set<Facet*>::const_iterator i = _completedFacets.begin(),
				e = _completedFacets.end(); i != e; i++)
		{
			Facet* f = *i;
			writer.addFace(f->pa, f->pb, f->pc);
		}
	}

	/* Discard a facet (remove it from both lists) */

	void discard(Facet* facet)
	{
		_pendingFacets.erase(facet);
		_completedFacets.erase(facet);
	}

	/* Splits a discarded facet; the new facets are queued on the pending list.
	 * Any subsplits cause those facets to be discarded.
	 */

	void split(Facet* f)
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

private:
	const Transform& _view;
	const Terrain& _terrain;
	double _error;

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
	struct Facet
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
		double _error;

	public:
		Facet(const Point& pa, const Point& pb, const Point& pc):
				pa(pa), pb(pb), pc(pc),
				fa(NULL), fo(NULL), fh(NULL),

				_midh((pa.x+pc.x)/2, (pa.y+pc.y)/2, (pa.z+pc.z)/2),
				_error(NAN)
		{

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

		const Point& getRealMidh(const Terrain& terrain)
		{
			if (!_realmidh.isValid())
				_realmidh = terrain.mapToTerrain(_midh);
			return _realmidh;
		}

		double getError(const Transform& view, const Terrain& terrain)
		{
			if (isnan(_error))
			{
				const Point& realmid = getRealMidh(terrain);

				Vector vfake = view.transform(_midh).toVector().normalise();
				Vector vreal = view.transform(realmid).toVector().normalise();

				double d = vfake.dot(vreal);
				if (d < 1)
					_error = acos(d);
				else
					_error = 0;
				assert(!isnan(_error));
			}

			return _error;
		}
	};

	Facet* addFacet(const Point& pa, const Point& pb, const Point& pc)
	{
		Facet* f = new Facet(pa, pb, pc);
		_pendingFacets.insert(f);
		return f;
	}

	std::set<Facet*> _pendingFacets;
	std::set<Facet*> _completedFacets;
};
