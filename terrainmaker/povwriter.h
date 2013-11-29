/* TerrainMaker
 * © 2013 David Given
 *
 * This software is licensed under the Simplified BSD license. See COPYING
 * for the full text.
 */

class PovWriter : public Writer
{
public:
	PovWriter()
	{
	}

	void writeTo(const char* filename)
	{
		std::ofstream of;
		of.open(filename, std::ios::out);
		of.precision(10);

		of << "mesh2 {\n"
		   << "vertex_vectors {\n"
		   << _pointArray.size();

		for (int i=0; i<_pointArray.size(); i++)
		{
			const Point& v = _pointArray[i];
			of << ",\n<" << v.x << ", " << v.y << ", " << v.z << ">";
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
		   << "}\n"
		   << "}\n";
	}
};
