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
		size_t replstart = haystack.find(needle);
		haystack.replace(replstart, needle.size(), subst);
	}

public:
	void write(const char* outfilename, const char* tmplfilename,
			Transform& view, double altitude)
	{
		std::ifstream templatef(tmplfilename);
		std::stringstream templatebuffer;
		templatebuffer << templatef.rdbuf();
		std::string templates = templatebuffer.str();

		Point camera = view.untransform(Point::ORIGIN);
		std::cerr << "camera at (" << camera.x << ", " << camera.y
				<< ", " << camera.z << ")\n";

		Point target = view.untransform(Point(0, 1, 0));
		std::cerr << "looking at (" << target.x << ", " << target.y
				<< ", " << target.z << ")\n";

		Vector up = (view.untransform(Point(0, 0, 1)) - camera).normalise();
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

		if (altitude > ATMOSPHERE)
			replace(templates, "<AIR/>", "");
		else
			replace(templates, "<AIR/>", "<ref id=\"air\"/>");

		std::ofstream outputf(outfilename, std::ios::out);
		outputf << templates;
	}
};
