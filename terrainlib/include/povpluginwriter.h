/* TerrainMaker
 * © 2015 David Given
 *
 * This software is licensed under the Simplified BSD license. See COPYING
 * for the full text.
 */

class PovPluginWriter : public Writer
{
private:
	void writePoint(const Point& p, struct povray_point3d& t)
	{
		t.x = p.x;
		t.y = p.y;
		t.z = p.z;
	}

	void writeUV(const PointData& p, struct povray_point2d& t)
	{
		t.x = std::isnan(p.u) ? 0.0 : p.u;
		t.y = std::isnan(p.v) ? 0.0 : p.v;
	}

	void writeTriangle(const Triangle& p, struct povray_triangle& t)
	{
		t.a = p.a;
		t.b = p.b;
		t.c = p.c;
	}

	static void mesh_freer(struct povray_mesh* mesh)
	{
		delete [] mesh->vertices;
		delete [] mesh->uvcoords;
		delete [] mesh->normals;
		delete [] mesh->triangles;
		delete mesh;
	}

public:
	PovPluginWriter()
	{
	}

	struct povray_mesh* writeToMesh(Transform& world)
	{
		Point camera = world.untransform(Point::ORIGIN);
		struct povray_mesh* mesh = new struct povray_mesh;
		mesh->freer = mesh_freer;

		mesh->number_of_vertices = mesh->number_of_uvcoords
			= mesh->number_of_normals = _pointArray.size();
		mesh->number_of_triangles = _faces.size();

		mesh->vertices = new struct povray_point3d[mesh->number_of_vertices];
		mesh->uvcoords = new struct povray_point2d[mesh->number_of_uvcoords];
		mesh->normals = new struct povray_point3d[mesh->number_of_normals];
		mesh->triangles = new struct povray_triangle[mesh->number_of_triangles];

		for (int i=0; i<_pointArray.size(); i++)
		{
			writePoint(_pointArray[i].point - camera, mesh->vertices[i]);
			writePoint(_pointArray[i].normal, mesh->normals[i]);
			writeUV(_pointArray[i], mesh->uvcoords[i]);
		}

		for (int i=0; i<_faces.size(); i++)
			writeTriangle(_faces[i], mesh->triangles[i]);

		std::cerr << "done creating mesh\n";
		return mesh;
	}

	void writeTo(const std::string& filename)
	{
		throw std::logic_error("not implemented");
	}
};
