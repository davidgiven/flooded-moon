#ifndef VARIABLES_H
#define VARIABLES_H

#include <boost/program_options.hpp>


class Variables
{
public:
	unsigned width, height;
	double latitude = 20.0;
	double longitude = -3.5;
	double altitude = 5.0;
	double azimuth = -10.0;
	double bearing = 40.0;
	double fov = 50.0;
	double time_of_day = 0;
	double radius;
	double sealevel;
	std::string cameraf;
	std::string topof;
	std::string seatopof;
	std::string propsf;
	double maxpropdistance = 20.0;
	std::string heightmapf;
	std::string seafuncf = "scripts/waterlevel.cal";
	std::string terrainfuncf = "scripts/terrain.cal";
	std::string texturefuncf = "scripts/texture.cal";
	std::string propsfuncf = "scripts/props.cal";
	double shmixels = 100.0;
	std::vector<std::string> terrain;
	std::vector<std::string> geoid;

	void parse(int argc, const char* argv[])
	{
		namespace po = boost::program_options;

		po::options_description options("Allowed options");
		options.add_options()
			("help,h",
					"produce help message")
			("config", po::value<std::vector<std::string>>()->composing(),
					"read additional options from config file")
			("width", po::value<unsigned>(&width)->default_value(2048),
					"width of output image")
			("height", po::value<unsigned>(&height)->default_value(2048),
					"height of output image")
			("latitude", po::value<double>(&latitude),
					"latitude")
			("longitude", po::value<double>(&longitude),
					"longitude")
			("altitude", po::value<double>(&altitude),
					"altitude (above sea level)")
			("azimuth", po::value<double>(&azimuth),
					"azimuth (0 looks straight ahead; negative looks down)")
			("bearing", po::value<double>(&bearing),
					"bearing")
			("fov", po::value(&fov),
					"field of view")
			("timeofday", po::value(&time_of_day),
					"time of day (virtual hours)")
			("shmixels", po::value<double>(&shmixels),
					"terrain quality")
			("radius", po::value<double>(&radius),
					"average planetary radius")
			("sealevel", po::value<double>(&sealevel),
					"sealevel (relative to radius)")
			("geoid", po::value<std::vector<std::string>>()->composing(),
					"add a geoid PDS file (repeatable)")
			("terrain", po::value<std::vector<std::string>>()->composing(),
					"add a terrain PDS file (repeatable)")
			("seafunc", po::value(&seafuncf),
					"filename of Calculon script for calculating sea")
			("terrainfunc", po::value(&terrainfuncf),
					"filename of Calculon script for calculating terrain")
			("texturefunc", po::value(&texturefuncf),
					"filename of Calculon script for calculating texture")
			("propsfunc", po::value(&propsfuncf),
					"filename of Calculon script for calculating prop density")
			("propdistance", po::value(&maxpropdistance),
					"maximum distance from the camera to render props")
			("camera", po::value<std::string>(&cameraf),
					"write camera info to specified file")
			("topo", po::value<std::string>(&topof),
					"generate land topography and write to specified file")
			("seatopo", po::value<std::string>(&seatopof),
					"generate ocean topography and write to specified file")
			("props", po::value<std::string>(&propsf),
					"generate props and write to specified file")
			("heightmap", po::value<std::string>(&heightmapf),
					"generate cylindrical heightmap and write to specified file")
		;

		po::positional_options_description posoptions;
		posoptions.add("config", -1);

		po::variables_map vm;

		po::store(po::parse_environment(options, "TERRAINMAKER_"), vm);
		po::notify(vm);

		if (argc > 0)
		{
			po::store(
				po::command_line_parser(argc, argv)
					.options(options)
					.positional(posoptions)
					.run(),
				vm);
			po::notify(vm);
		}

		if (vm.count("help"))
		{
			std::cout << "terrainmaker: makes terrain.\n"
					  << options;
			exit(1);
		}

		if (vm.count("config") > 0)
		{
			for (auto s : vm["config"].as<std::vector<std::string>>())
			{
				std::ifstream stream(s);
				po::store(po::parse_config_file(stream, options), vm);
			}
			po::notify(vm);
		}

		if (vm.count("terrain") > 0)
			terrain = vm["terrain"].as<std::vector<std::string>>();

		if (vm.count("geoid") > 0)
			terrain = vm["geoid"].as<std::vector<std::string>>();
	}
};

#endif

