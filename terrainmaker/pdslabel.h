/* TerrainMaker
 * © 2013 David Given
 *
 * This software is licensed under the Simplified BSD license. See COPYING
 * for the full text.
 */

#ifndef PDSLABEL_H
#define PDSLABEL_H

class PDSLabel
{
public:
	PDSLabel(const std::string& filename,
		const std::map<std::string, std::string>& parameters);

	bool hasKey(const std::string& key)
	{
		return _stringValues.find(key) != _stringValues.end();
	}

	const std::string& getString(const std::string& key)
	{
		return _stringValues[key];
	}

	double getFloat(const std::string& key)
	{
		const std::string& s = getString(key);
		return strtod(s.c_str(), NULL);
	}

	unsigned getUnsigned(const std::string& key)
	{
		const std::string& s = getString(key);
		return strtoul(s.c_str(), NULL, 0);
	}

protected:
	std::map<std::string, std::string> _stringValues;
};

#endif

