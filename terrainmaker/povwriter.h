/* TerrainMaker
 * © 2013 David Given
 *
 * This software is licensed under the Simplified BSD license. See COPYING
 * for the full text.
 */

#ifndef POVWRITER_H
#define POVWRITER_H

#include "writer.h"

class PovWriter : public Writer
{
public:
	PovWriter() {}

	void writeTo(const std::string& filename);
};

#endif

