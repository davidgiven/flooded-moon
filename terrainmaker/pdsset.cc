/* TerrainMaker
 * © 2013 David Given
 *
 * This software is licensed under the Simplified BSD license. See COPYING
 * for the full text.
 */

#include "globals.h"
#include "utils.h"
#include "matrix.h"
#include "pds.h"
#include "pdsset.h"

PDSSet terrainpds;
PDSSet geoidpds;

void PDSSet::add(PDS* pds)
{
	Record* r = new Record;
	pds->bounds(r->minLon, r->minLat, r->maxLon, r->maxLat);
	r->pds = pds;
	_records.push_back(r);
}

void PDSSet::add(const std::string& filename)
{
	try
	{
		PDS* pds = PDS::LoadFromSpec(filename);
		add(pds);
	}
	catch (std::exception& e)
	{
		std::string s = "error opening '";
		s += filename;
		s += "': ";
		s += e.what();
		fatalError(s);
	}
}

bool PDSSet::contains(double x, double y) const
{
	x = wrapf(0, 360.0, x);

	for (Record* r : _records)
	{
		if ((x >= r->minLon) && (x < r->maxLon) &&
			(y >= r->minLat) && (y < r->maxLat))
		{
			return true;
		}
	}
	return false;
}

double PDSSet::at(double x, double y) const
{
	x = wrapf(0, 360.0, x);

	for (Record* r : _records)
	{
		if ((x >= r->minLon) && (x < r->maxLon) &&
			(y >= r->minLat) && (y < r->maxLat))
		{
			return r->pds->at(x, y);
		}
	}
	return 0;
}

