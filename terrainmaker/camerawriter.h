class CameraWriter
{
public:
	CameraWriter()
	{
	}

	void write(const char* outfilename, const char* tmplfilename,
			Transform& view)
	{
		std::ifstream templatef(tmplfilename);
		std::stringstream templatebuffer;
		templatebuffer << templatef.rdbuf();
		std::string templates = templatebuffer.str();

		Vector camera = view.untransform(Vector::ORIGIN);
		std::cerr << "camera at (" << camera.x << ", " << camera.y
				<< ", " << camera.z << ")\n";

		Vector target = view.untransform(Vector::Y);
		std::cerr << "looking at (" << target.x << ", " << target.y
				<< ", " << target.z << ")\n";

		Vector up = (view.untransform(Vector::Z) - camera).normalise();
		std::cerr << "up (" << up.x << ", " << up.y
				<< ", " << up.z << ")\n";

		std::stringstream lookatbuffer;
		lookatbuffer << "<lookat origin=\""
				<< camera.x << ", " << camera.y << ", " << camera.z << "\" "
				<< "target=\""
				<< target.x << ", " << target.y << ", " << target.z << "\" "
				<< "up=\""
				<< up.x << ", " << up.y << ", " << up.z << "\"/>\n";

		size_t replstart = templates.find("<LOOKAT/>");
		templates.replace(replstart, 9, lookatbuffer.str());

		std::ofstream outputf(outfilename, std::ios::out);
		outputf << templates;
	}
};
