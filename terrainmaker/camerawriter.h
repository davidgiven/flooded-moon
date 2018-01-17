/* TerrainMaker
 * © 2013 David Given
 *
 * This software is licensed under the Simplified BSD license. See COPYING
 * for the full text.
 */

class CameraWriter
{
public:
	CameraWriter()
	{
	}

private:
	void replace(std::string& haystack, const std::string& needle,
			const std::string& subst)
	{
		std::string::size_type replstart = haystack.find(needle);
		if (replstart != std::string::npos)
			haystack.replace(replstart, needle.size(), subst);
	}

public:
	void writePov(const char* outfilename)
	{
		Point camera = world.untransform(Point::ORIGIN);
		std::cerr << "camera at (" << camera.x << ", " << camera.y
				<< ", " << camera.z << ")\n";

		Point target = world.untransform(Point(0, 1, 0));
		std::cerr << "looking at (" << target.x << ", " << target.y
				<< ", " << target.z << ")\n";

		Vector up = (world.untransform(Point(0, 0, -1)) - camera).normalise();
		std::cerr << "up (" << up.x << ", " << up.y
				<< ", " << up.z << ")\n";

		std::ofstream outputf(outfilename, std::ios::out);
		outputf << "#declare CameraLocation = <"
			    << camera.x << ", " << camera.y << ", " << camera.z << ">;\n"
		        << "#declare CameraLookAt = <"
		        << target.x << ", " << target.y << ", " << target.z << ">;\n"
		        << "#declare CameraSky = <"
		        << up.x << ", " << up.y << ", " << up.z << ">;\n"
				<< "#declare CameraAltitude = " << vars.altitude << ";\n"
				<< "#declare CameraLongitude = " << vars.longitude << ";\n"
				<< "#declare CameraLatitude = " << vars.latitude << ";\n"
				<< "#declare CameraBearing = " << vars.bearing << ";\n"
				<< "#declare CameraAzimuth = " << vars.azimuth << ";\n"
				<< "#declare CameraFieldOfView = " << vars.fov << ";\n"
				<< "#declare Time_Of_Day = " << (vars.time_of_day+6.0) << ";\n"
			;
	}

	void writeMitsuba(const char* outfilename, const char* tmplfilename)
	{
		std::ifstream templatef(tmplfilename);
		std::stringstream templatebuffer;
		templatebuffer << templatef.rdbuf();
		std::string templates = templatebuffer.str();

		Point camera = world.untransform(Point::ORIGIN);
		std::cerr << "camera at (" << camera.x << ", " << camera.y
				<< ", " << camera.z << ")\n";

		Point target = world.untransform(Point(0, 1, 0));
		std::cerr << "looking at (" << target.x << ", " << target.y
				<< ", " << target.z << ")\n";

		Vector up = (world.untransform(Point(0, 0, 1)) - camera).normalise();
		std::cerr << "up (" << up.x << ", " << up.y
				<< ", " << up.z << ")\n";

		std::stringstream lookatbuffer;
		lookatbuffer << "<lookat origin=\""
				<< camera.x << ", " << camera.y << ", " << camera.z << "\" "
				<< "target=\""
				<< target.x << ", " << target.y << ", " << target.z << "\" "
				<< "up=\""
				<< up.x << ", " << up.y << ", " << up.z << "\"/>\n";
		replace(templates, "<LOOKAT/>", lookatbuffer.str());

		std::stringstream fovbuffer;
		fovbuffer << "<float name=\"fov\" value=\""
		          << vars.fov
				  << "\"/>";
		replace(templates, "<FOV/>", fovbuffer.str());

		if (vars.altitude > ATMOSPHERE)
			replace(templates, "<AIR/>", "");
		else
			replace(templates, "<AIR/>", "<ref id=\"air\"/>");

		std::ofstream outputf(outfilename, std::ios::out);
		outputf << templates;
	}

	void writeBlender(const char* outfilename)
	{
		Point camera = world.untransform(Point::ORIGIN);
		std::cerr << "camera at (" << camera.x << ", " << camera.y
				<< ", " << camera.z << ")\n";

		Point target = world.untransform(Point(0, -1, 0));
		std::cerr << "looking at (" << target.x << ", " << target.y
				<< ", " << target.z << ")\n";

		Vector up = (world.untransform(Point(0, 0, -1)) - camera).normalise();
		std::cerr << "up (" << up.x << ", " << up.y
				<< ", " << up.z << ")\n";

		std::ofstream outputf(outfilename, std::ios::out);

		outputf << "clocation = mathutils.Vector(("
		        << camera.x << ", " << camera.y << ", " << camera.z << "))\n"
				<< "ctarget = mathutils.Vector(("
		        << target.x << ", " << target.y << ", " << target.z << "))\n"
				<< "cup = mathutils.Vector(("
		        << up.x << ", " << up.y << ", " << up.z << "))\n"
				<< "lookAt(camerao, clocation, ctarget, cup)\n";
	}
};
