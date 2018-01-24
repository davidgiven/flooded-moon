#include "globals.h"

void fatalError(const std::string& e)
{
	std::cerr << "error: " << e << "\n";
	exit(1);
}
