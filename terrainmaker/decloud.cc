#include <string>
#include "globals.h"
#include "utils.h"
#include "matrix.h"
#include "pds.h"
#include "map.h"
#include <pcl/io/pcd_io.h>
#include <pcl/point_types.h>

namespace
{
    Point lonLatToXYZ(double lon, double lat, double radius)
    {
        Transform t;
        t = t.rotate(Vector::Y, -lat);
        t = t.rotate(Vector::Z, lon-90);
        Point p(radius, 0.0, 0.0);
        p = t.transform(p);
        return p;
    }
}

int main(int argc, const char* argv[])
{
    std::string inputFilename = argv[1];
    std::string outputFilename = argv[1];
    PDS* pds = PDS::LoadFromSpec(inputFilename);

    unsigned width, height;
    pds->samples(width, height);
    pcl::PointCloud<pcl::PointXYZ> cloud;
    cloud.is_dense = true;
    cloud.width = width * height;
    cloud.height = 1;
    cloud.points.resize(cloud.width);

    int progress = 0;
    unsigned point = 0;
    for (unsigned y=0; y<height; y++)
    {
		int newprogress = (y*100)/height;
		if (newprogress != progress)
		{
			std::cerr << '\r' << newprogress << "%";
			progress = newprogress;
		}

        for (unsigned x=0; x<width; x++)
        {
            double v = pds->sampleat(x, y);
            double lon, lat;
            pds->unfindsample(x, y, lon, lat);

            Point p = lonLatToXYZ(lon, lat, v) / 1000.0;
            pcl::PointXYZ& pclPoint = cloud.points[point++];
            pclPoint.x = p.x;
            pclPoint.y = p.y;
            pclPoint.z = p.z;
        }
    }

    pcl::io::savePCDFileBinary(outputFilename, cloud);

    return 0;
}