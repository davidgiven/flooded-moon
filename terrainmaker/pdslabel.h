/* TerrainMaker
 * © 2013 David Given
 *
 * This software is licensed under the Simplified BSD license. See COPYING
 * for the full text.
 */

#ifndef PDSLABEL_H
#define PDSLABEL_H

/* WARNING. This is not a robust parser and may OOB access on non-well-formed
 * input files. */

#include <boost/iostreams/device/mapped_file.hpp>

class parse_failed_exception : public std::logic_error
{
public:
	parse_failed_exception(const std::string& e):
		logic_error(e)
	{
	}
};

class PDSLabel
{
public:
	PDSLabel(const std::string& filename,
		const std::map<std::string, std::string>& parameters):
			_stringValues(parameters)
	{
		boost::iostreams::mapped_file_source file;
		file.open(filename);

		const char* p = (const char*) file.data();

		for (;;)
		{
			std::string keyword = get_keyword(p);
			if (keyword == "END")
				break;

			get_equals(p);

			std::string value = get_value(p);

			_stringValues[keyword] = value;
		}

	}

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

private:
	void skipws(const char*& p)
	{
		for (;;)
		{
			if (isspace(*p))
			{
				p++;
				continue;
			}

			if ((p[0] == '/') && (p[1] == '*'))
			{
				while ((p[0] != '*') || (p[1] != '/'))
					p++;
				p += 2;
				continue;
			}

			break;
		}
	}

	std::string get_keyword(const char*& p)
	{
		skipws(p);

		const char* start = p;
		while (!isspace(*p) && ((*p) != '='))
			p++;
		const char* end = p;

		return std::string(start, end);
	}

	void get_equals(const char*& p)
	{
		skipws(p);
		if (*p != '=')
			throw parse_failed_exception("missing '='");
		p++;
	}

	std::string get_value(const char*& p)
	{
		skipws(p);

		switch (*p)
		{
			case '"': return get_string_value(p);
			case '{': return get_list_value(p);
			default:  return get_simple_value(p);
		}
	}

	std::string get_string_value(const char*& p)
	{
		p++;

		const char* start = p;
		while (*p != '"')
			p++;
		const char* end = p;

		p++;
		return std::string(start, end);
	}

	std::string get_list_value(const char*& p)
	{
		const char* start = p;
		p++;

		while (*p != '}')
			p++;

		p++;
		const char* end = p;

		return std::string(start, end);
	}

	std::string get_simple_value(const char*& p)
	{
		const char* start = p;
		p++;

		while ((*p != '\r') && (*p != '\n'))
			p++;

		const char* end = p;

		return std::string(start, end);
	}

protected:
	std::map<std::string, std::string> _stringValues;
};

#endif

