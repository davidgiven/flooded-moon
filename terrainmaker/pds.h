/* TerrainMaker
 * © 2013 David Given
 *
 * This software is licensed under the Simplified BSD license. See COPYING
 * for the full text.
 */

#ifndef PDS_H
#define PDS_H

#include "map.h"
#include "pdslabel.h"
#include <boost/iostreams/device/mapped_file.hpp>

class PDS : public LatLongMap
{
public:
	static PDS* LoadFromSpec(const std::string& spec);

public:
	PDS(const std::string& labelfn, const std::string& datafn,
			const std::map<std::string, std::string>& parameters);

	bool contains(double x, double y) const
	{
		if ((x < _minLon) || (x >= _maxLon) ||
		    (y < _minLat) || (y >= _maxLat))
			return false;
		return true;
	}

	double sampleat(unsigned x, unsigned y) const
	{
		assert(x < _width);
		assert(y < _height);
		double v = _data[y*_width + x];
		return (v*_scalingFactor) + _offset;
	}

	void samples(unsigned& width, unsigned& height) const
	{
		width = _width;
		height = _height;
	}

	void bounds(double& minLon, double& minLat, double& maxLon, double& maxLat) const
	{
		minLat = _minLat;
		minLon = _minLon;
		maxLat = _maxLat;
		maxLon = _maxLon;
	}

	double at(double lon, double lat) const;

	void unfindsample(double px, double py, double& lon, double& lat) const;
	void findsample(double lon, double lat, double& x, double& y) const;

private:
	PDSLabel _label;
	boost::iostreams::mapped_file_source _file;
	const int16_t* _data;

	unsigned _recordlen;
	unsigned _width, _height;
	double _minLat, _maxLat;
	double _minLon, _maxLon;
	double _wScale, _hScale;
	double _centreLongitude;
	double _centreLatitude;

	double _scalingFactor;
	double _offset;

	enum
	{
		EQUIRECTANGULAR,
		POLAR_STEREOGRAPHIC
	};
	int _projection;

	double _tdash;
};

#endif

