/* TerrainMaker
 * © 2013 David Given
 *
 * This software is licensed under the Simplified BSD license. See COPYING
 * for the full text.
 */

class PovWriter : public Writer
{
public:
	PovWriter(const std::string& filename):
		Writer(filename)
	{
	}

	void writeTo(const char* filename)
	{
		std::ofstream of;
		of.open(filename, std::ios::out);
		of.precision(20);

		of << "vertex_vectors {\n"
		   << _pointArray.size();

		Point camera = world.untransform(Point::ORIGIN);

		for (int i=0; i<_pointArray.size(); i++)
		{
			const Point v = _pointArray[i].point - camera;
			
			of << ",\n<" << v.x << ", " << v.y << ", " << v.z << ">";
		}
		of << "\n}\n";

		#if 1
		of << "normal_vectors {\n"
		   << _pointArray.size();

		for (int i=0; i<_pointArray.size(); i++)
		{
			const Vector& v = _pointArray[i].normal;
			of << ",\n<" << v.x << ", " << v.y << ", " << v.z << ">";
		}
		of << "\n}\n";
		#endif

		of << "uv_vectors {\n"
		   << _pointArray.size();

		for (int i=0; i<_pointArray.size(); i++)
		{
			const PointData& pd = _pointArray[i];
			double u = std::isnan(pd.u) ? 0.0 : pd.u;
			double v = std::isnan(pd.v) ? 0.0 : pd.v;
			of << ",\n<" << u << ", " << v << ">";
		}
		of << "\n}\n";

		of << "face_indices {\n"
		   << _faces.size();

		for (int i=0; i<_faces.size(); i++)
		{
			const Triangle& t = _faces[i];
			of << ",\n<" << t.a << ", " << t.b << ", " << t.c << ">";
		}
		of << "\n"
		   << "}\n";

		of << "inside_vector CameraSky\n";

		of << "translate <"
		   << camera.x << ", " << camera.y << ", " << camera.z << ">\n";
	}
};
