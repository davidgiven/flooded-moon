#ifndef VARIABLES_H
#define VARIABLES_H

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

	void parse(int argc, const char* argv[]);
};

extern Variables vars;

#endif

