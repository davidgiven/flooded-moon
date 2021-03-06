/* TerrainMaker
 * © 2013 David Given
 *
 * This software is licensed under the Simplified BSD license. See COPYING
 * for the full text.
 */

#include "globals.h"
#include "plywriter.h"

void PlyWriter::writeTo(const std::string& filename)
{
	std::ofstream of;
	of.open(filename.c_str(), std::ios::out);
	of.precision(10);

	of << "ply\n"
		 "format ascii 1.0\n"
		 "element vertex " << _pointArray.size() << "\n"
		 "property float x\n"
		 "property float y\n"
		 "property float z\n"
		 "element face " << _faces.size() << "\n"
		 "property list uchar int vertex_indices\n"
		 "end_header\n";

	std::cerr << _pointArray.size() << " vertices and " << _faces.size() << " faces\n";

	for (int i=0; i<_pointArray.size(); i++)
	{
		const Point& v = _pointArray[i].point;
		of << v.x << " " << v.y << " " << v.z << "\n";
	}

	for (int i=0; i<_faces.size(); i++)
	{
		const Triangle& t = _faces[i];
		of << "3 " << t.a << " " << t.b << " " << t.c << "\n";
	}
}

