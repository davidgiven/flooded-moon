class Propmaster
{
	typedef u_int64_t SectorID;

public:
	Propmaster(const Transform& view, const Terrain& terrain, int maxrecursion,
			double maxdistance):
		_view(view),
		_terrain(terrain),
		_camera(_view.transform(Point::ORIGIN)),
		_maxRecursion(maxrecursion),
		_maxDistance(maxdistance)
	{
		icosahedron();
	}

private:
	void commitSector(const Point& va, const Point& vb, const Point& vc, SectorID id)
	{
		std::cerr << "sector " << id << "\n";
	}

	void facet(const Point& va, const Point& vb, const Point& vc, SectorID id)
	{
		if (_recursionLevel >= _maxRecursion)
			commitSector(va, vb, vc, id);
		else
		{
			_recursionLevel++;

			double distance = (va - _camera).length();
			if (distance < _maxDistance)
			{
				Point vab = Vector(va.x+vb.x, va.y+vb.y, va.z+vb.z).normalise();
				Point vbc = Vector(vb.x+vc.x, vb.y+vc.y, vb.z+vc.z).normalise();
				Point vca = Vector(vc.x+va.x, vc.y+va.y, vc.z+va.z).normalise();

				facet(va, vab, vca, id*4 + 0);
				facet(vb, vbc, vab, id*4 + 1);
				facet(vc, vca, vbc, id*4 + 2);
				facet(vab, vbc, vca, id*4 + 3);
			}

			_recursionLevel--;
		}
	}

	void icosahedron()
	{
		double x = 0.525731112119133606;
		double z = 0.850650808352039932;

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

private:
	const Transform& _view;
	const Terrain& _terrain;
	Point _camera;
	int _maxRecursion;
	double _maxDistance;
	int _recursionLevel;
};