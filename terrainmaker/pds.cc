/* TerrainMaker
 * © 2013 David Given
 *
 * This software is licensed under the Simplified BSD license. See COPYING
 * for the full text.
 */

#include "globals.h"
#include "utils.h"
#include "matrix.h"
#include "map.h"
#include "pdslabel.h"
#include "pds.h"
#include <boost/algorithm/string.hpp>

const std::map<std::string, std::string> emptyMap;

PDS* PDS::LoadFromSpec(const std::string& spec)
{
	std::deque<std::string> tokens;
	boost::split(tokens, spec, boost::is_any_of(":"));

	if (tokens.size() == 0)
		throw std::logic_error("invalid PDS spec");

	std::string labelfn = tokens[0];

	std::string datafn = labelfn;
	if (tokens.size() > 1)
	{
		datafn = tokens[1];
		if (datafn.empty())
			datafn = labelfn;
	}

	std::map<std::string, std::string> parameters;
	for (unsigned i = 2; i < tokens.size(); i++)
	{
		std::string p = tokens[i];
		std::string::size_type d = p.find_first_of('=');
		if (d == std::string::npos)
			throw std::logic_error("PDS parameter doesn't have a =");

		std::string k = p.substr(0, d);
		std::string v = p.substr(d+1);
		parameters[k] = v;
	}

	return new PDS(labelfn, datafn, parameters);
}

PDS::PDS(const std::string& labelfn, const std::string& datafn,
		const std::map<std::string, std::string>& parameters = emptyMap):
	_label(labelfn, parameters),
	_file(datafn)
{
	_recordlen = _label.getUnsigned("RECORD_BYTES");

	unsigned imageptr = _label.getUnsigned("^IMAGE");
	if (imageptr == 0)
		imageptr = 1; /* default to first record */
	_data = (int16_t*)((int8_t*)_file.data() + (imageptr-1)*_recordlen);

	_width = _label.getUnsigned("LINE_SAMPLES");
	_height = _label.getUnsigned("LINES");

	const std::string& projection = _label.getString("MAP_PROJECTION_TYPE");
	if ((projection == "SIMPLE CYLINDRICAL") || (projection == "EQUIRECTANGULAR"))
		_projection = EQUIRECTANGULAR;
	else if (projection == "POLAR STEREOGRAPHIC")
		_projection = POLAR_STEREOGRAPHIC;
	else
		throw std::logic_error("PDS file doesn't have supported map projection");

	_centreLatitude = _label.getFloat("CENTER_LATITUDE");
	_centreLongitude = _label.getFloat("CENTER_LONGITUDE");

	if (_label.getUnsigned("SAMPLE_BITS") != 16)
		throw std::logic_error("PDS file isn't 16 bit");

	if (_label.getString("SAMPLE_TYPE") != "LSB_INTEGER")
		throw std::logic_error(
			boost::str(boost::format("PDS file is %s, not LSB_INTEGER") % _label.getString("SAMPLE_TYPE")));
	if (!LITTLE_ENDIAN)
		throw std::logic_error("PDS files can't be read on big-endian systems");

	_minLat = _label.getFloat("MINIMUM_LATITUDE");
	_maxLat = _label.getFloat("MAXIMUM_LATITUDE");
	_minLon = _label.getFloat("WESTERNMOST_LONGITUDE");
	_maxLon = _label.getFloat("EASTERNMOST_LONGITUDE");

	_scalingFactor = _label.getFloat("SCALING_FACTOR");
	if (_scalingFactor == 0.0)
		_scalingFactor = 1.0;
	_offset = _label.getFloat("OFFSET");

	_wScale = (double)_width / (_maxLon - _minLon);
	_hScale = (double)_height / (_maxLat - _minLat);

	if (_projection == POLAR_STEREOGRAPHIC)
	{
		if (_minLat > 0.0)
			_tdash = tan(M_PI/4 - degToRad(_minLat)/2.0);
		else
			_tdash = tan(M_PI/4 + degToRad(_maxLat)/2.0);
	}

	std::cerr << "read PDS: " << _label.getString("DATA_SET_ID")
			  << " (" << _width << " x " << _height << ") = ("
			  << _minLon << "," << _minLat << " - "
			  << _maxLon << "," << _maxLat << ")\n"

			  << "    v' = (v * " << _scalingFactor << ") + " << _offset << "\n";

}

double PDS::at(double lon, double lat) const
{
	double px, py;
	findsample(lon, lat, px, py);

	double tl = getsample(floor(px), floor(py));
	double tr = getsample(ceil(px), floor(py));
	double bl = getsample(floor(px), ceil(py));
	double br = getsample(ceil(px), ceil(py));

	px = px - floor(px);
	py = py - floor(py);

	double v = ((1-px) * (1-py) * tl) +
			(px * (1-py) * tr) +
			((1-px) * py * bl) +
			(px * py * br);

	return (v*_scalingFactor) + _offset;
}

void PDS::findsample(double lon, double lat, double& x, double& y) const
{
	switch (_projection)
	{
		case EQUIRECTANGULAR:
		{
			double dx = (lon - _minLon) / (_maxLon - _minLon);
			double dy = (lat - _minLat) / (_maxLat - _minLat);

			x = dx * (double)(_width-1);
			y = (1-dy) * (double)(_height-1);

			assert(x >= 0.0);
			assert(y >= 0.0);
			break;
		}

		case POLAR_STEREOGRAPHIC:
		{
			double dlat = degToRad(lat);
			double dlon = degToRad(180.0 + lon - _centreLongitude);
			double t = tan(M_PI/4 - fabs(dlat)/2.0);
			double rho = t / _tdash;

			double e, n;
			if (_minLat > 0.0)
			{
				/* North polar. */

				e = rho * sin(dlon);
				n = - rho * cos(dlon);
			}
			else
			{
				/* South polar. */
				e = rho * sin(dlon);
				n = rho * cos(dlon);
			}

			x = (0.5 - e/2.0) * (double)(_width-1);
			y = (0.5 + n/2.0) * (double)(_height-1);
			break;
		}
	}
}

