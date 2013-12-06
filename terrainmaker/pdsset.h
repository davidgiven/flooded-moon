/* TerrainMaker
 * © 2013 David Given
 *
 * This software is licensed under the Simplified BSD license. See COPYING
 * for the full text.
 */

class PDSSet : public Map
{
private:
	struct Record
	{
		double minLat, minLon;
		double maxLat, maxLon;
		PDS* pds;
	};

public:
	PDSSet()
	{
	}

	void add(PDS* pds)
	{
		Record* r = new Record;
		pds->bounds(r->minLon, r->minLat, r->maxLon, r->maxLat);
		r->pds = pds;
		_records.push_back(r);
	}

	void add(const std::string& filename)
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

	bool contains(double x, double y) const
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

	double at(double x, double y) const
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

private:
	std::deque<Record*> _records;
};

