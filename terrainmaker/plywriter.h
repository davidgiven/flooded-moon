/* TerrainMaker
 * © 2013 David Given
 *
 * This software is licensed under the Simplified BSD license. See COPYING
 * for the full text.
 */

#ifndef PLYWRITER_H
#define PLYWRITER_H

#include "writer.h"

class PlyWriter : public Writer
{
public:
	PlyWriter() {}

	void writeTo(const std::string& filename);
};

#endif

