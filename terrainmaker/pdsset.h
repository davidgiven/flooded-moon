/* TerrainMaker
 * © 2013 David Given
 *
 * This software is licensed under the Simplified BSD license. See COPYING
 * for the full text.
 */

#ifndef PDSSET_H
#define PDSSET_H

class PDSSet : public LatLongMap
{
private:
	struct Record
	{
		double minLat, minLon;
		double maxLat, maxLon;
		PDS* pds;
	};

public:
	PDSSet() {}

	PDSSet(std::initializer_list<std::string> list)
	{
		for (const std::string fn : list)
			add(fn);
	}

	PDSSet(std::string fn)
	{
		add(fn);
	}

	void add(PDS* pds);
	void add(const std::string& filename);

	bool contains(double x, double y) const;
	double at(double x, double y) const;

	using Map::at;

private:
	std::deque<Record*> _records;
};

extern PDSSet terrainpds;
extern PDSSet geoidpds;

#endif

