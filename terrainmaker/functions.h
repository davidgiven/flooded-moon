/* TerrainMaker
 * © 2013 David Given
 *
 * This software is licensed under the Simplified BSD license. See COPYING
 * for the full text.
 */

#ifndef FUNCTIONS_H

#include "calculon.h"
typedef Calculon::Instance<Calculon::RealIsDouble> Compiler;
extern Compiler::StandardSymbolTable calculonSymbols;

typedef void MapFunc(Compiler::Vector<3>* xyz, double* height);
extern Compiler::Program<MapFunc>* seaFunc;
extern Compiler::Program<MapFunc>* terrainFunc;

typedef void UVFunc(Compiler::Vector<3>* pv, double* u, double* v);
extern Compiler::Program<UVFunc>* textureFunc;

extern Compiler::Program<MapFunc>* propsFunc;

extern void initCalculon(void);

#endif

