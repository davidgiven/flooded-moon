class Propmaster
{
	typedef u_int32_t SectorID;

	struct Sector
	{
		SectorID id;
		Point pa;
		Point pb;
		Point pc;
	};

public:
	Propmaster(Transform& view, const Terrain& terrain, int maxrecursion,
			double maxdistance):
		_view(view),
		_terrain(terrain),
		_camera(_view.untransform(Point::ORIGIN)),
		_maxRecursion(maxrecursion),
		_maxDistance(maxdistance)
	{
		icosahedron();
	}

	void writeTo(const char* filename)
	{
		MeshWriter writer;

		for (Sectors::const_iterator i = _sectors.begin(),
				e = _sectors.end(); i != e; i++)
		{
			Sector* sector = *i;
			emitSector(writer, sector);
		}

		writer.writeTo(filename);
	}

private:
	void commitSector(const Point& va, const Point& vb, const Point& vc, SectorID id)
	{
		double size = max(
				(va - vb).length(),
				(va - vc).length(),
				(vb - vc).length()
			);

//		std::cerr << "sector " << id << " size " << size << "\n";

		Sector* sector = new Sector();
		sector->id = id;
		sector->pa = va;
		sector->pb = vb;
		sector->pc = vc;
		_sectors.insert(sector);
	}

	void facet(const Point& va, const Point& vb, const Point& vc, SectorID id)
	{
		/* Calculate the distance to the facet (the closest corner). */

		double distance = min(
				(va - _camera).length(),
				(vb - _camera).length(),
				(vc - _camera).length()
			);

		/* Calculate the size of the facet (the longest edge). */

		double size = max(
				(va - vb).length(),
				(va - vc).length(),
				(vb - vc).length()
			);

		/* If the facet is closer than our view horizon, *or* the
		 * facet is bigger than it is far away, then we consider it.
		 * Otherwise, it gets discarded.
		 */
//			std::cerr << id << " distance " << distance << " size " << size << "\n";
		if ((distance < _maxDistance) || (size > distance))
		{
			if (_recursionLevel >= _maxRecursion)
				commitSector(va, vb, vc, id);
			else
			{
				_recursionLevel++;

				Point vab = Vector(va.x+vb.x, va.y+vb.y, va.z+vb.z).normalise() * RADIUS;
				Point vbc = Vector(vb.x+vc.x, vb.y+vc.y, vb.z+vc.z).normalise() * RADIUS;
				Point vca = Vector(vc.x+va.x, vc.y+va.y, vc.z+va.z).normalise() * RADIUS;

				facet(va, vab, vca, id*4 + 0);
				facet(vb, vbc, vab, id*4 + 1);
				facet(vc, vca, vbc, id*4 + 2);
				facet(vab, vbc, vca, id*4 + 3);

				_recursionLevel--;
			}
		}
	}

	void icosahedron()
	{
		double x = 0.525731112119133606 * RADIUS;
		double z = 0.850650808352039932 * RADIUS;

		Point v0(-x, 0, z);
		Point v1(x, 0, z);
		Point v2(-x, 0, -z);
		Point v3(x, 0, -z);
		Point v4(0, z, x);
		Point v5(0, z, -x);
		Point v6(0, -z, x);
		Point v7(0, -z, -x);
		Point v8(z, x, 0);
		Point v9(-z, x, 0);
		Point va(z, -x, 0);
		Point vb(-z, -x, 0);

		_recursionLevel = 0;

		facet(v0, v4, v1, 0);
		facet(v0, v9, v4, 1);
		facet(v9, v5, v4, 2);
		facet(v4, v5, v8, 3);
		facet(v4, v8, v1, 4);

		facet(v8, va, v1, 5);
		facet(v8, v3, va, 6);
		facet(v5, v3, v8, 7);
		facet(v5, v2, v3, 8);
		facet(v2, v7, v3, 9);

		facet(v7, va, v3, 10);
		facet(v7, v6, va, 11);
		facet(v7, vb, v6, 12);
		facet(vb, v0, v6, 13);
		facet(v0, v1, v6, 14);

		facet(v6, v1, va, 15);
		facet(v9, v0, vb, 16);
		facet(v9, vb, v2, 17);
		facet(v9, v2, v5, 18);
		facet(v7, v2, vb, 19);
	}

	void emitTree(MeshWriter& writer, const Point& p, double width, double height)
	{
		Vector up = p.toVector().normalise();
		Vector tocamera = (p - _camera).normalise();
		Vector sideways = up.cross(tocamera);

		Point top = p + up*height;
		Point left = p + sideways*width;
		Point right = p - sideways*width;

		writer.addFace(right, top, left);
	}

	void emitSector(MeshWriter& writer, Sector* sector)
	{
		/* Calculate the area of the sector. */

		double ab = (sector->pa - sector->pb).length();
		double ac = (sector->pa - sector->pc).length();
		double bc = (sector->pb - sector->pc).length();
		double area = area_of_triangle(ab, ac, bc);

		double density = 100; // trees per square kilometre
		int treecount = (int)(area * density);

		srand(sector->id);
		for (int i = 0; i < treecount; i++)
		{
			double b0 = randf();
			double b1 = (1-b0) * randf();
			double b2 = 1 - b0 - b1;

			Point p(
					sector->pa.x*b0 + sector->pb.x*b1 + sector->pc.x*b2,
					sector->pa.y*b0 + sector->pb.y*b1 + sector->pc.y*b2,
					sector->pa.z*b0 + sector->pb.z*b1 + sector->pc.z*b2
				);

			double altitude = _terrain.terrain(p);
			if (altitude > (RADIUS+SEALEVEL+0.02))
			{
				p = _terrain.mapToTerrain(p, altitude);
				double height = 0.03 + randf()*0.03;
				emitTree(writer, p, 0.01, height);
			}
		}
	}

private:
	Transform& _view;
	const Terrain& _terrain;
	Point _camera;
	int _maxRecursion;
	double _maxDistance;
	int _recursionLevel;

	typedef std::set<Sector*> Sectors;
	Sectors _sectors;
};
