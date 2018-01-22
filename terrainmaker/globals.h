/* TerrainMaker
 * © 2013-2018 David Given
 *
 * This software is licensed under the Simplified BSD license. See COPYING
 * for the full text.
 */

#ifndef GLOBALS_H
#define GLOBALS_H

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <vector>
#include <map>
#include <list>
#include <set>
#include <deque>
#include <iostream>
#include <fstream>
#include <sstream>
#include <memory>
#include <unordered_map>
#include <boost/format.hpp>

extern void fatalError(const std::string& e);

const double MAXHEIGHT = 22; // maximum height of any object on the surface
const double ATMOSPHERE = 20;

using std::min;
using std::max;
using std::unique_ptr;

#endif
