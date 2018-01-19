/* TerrainMaker
 * © 2013 David Given
 *
 * This software is licensed under the Simplified BSD license. See COPYING
 * for the full text.
 */

#include "globals.h"
#include "utils.h"
#include "matrix.h"

Vector Vector::X(1, 0, 0);
Vector Vector::Y(0, 1, 0);
Vector Vector::Z(0, 0, 1);

Point Point::ORIGIN(0, 0, 0);

Matrix Matrix::IDENTITY(
		1, 0, 0, 0,
		0, 1, 0, 0,
		0, 0, 1, 0,
		0, 0, 0, 1
	);

Matrix Matrix::ZERO(
		0, 0, 0, 0,
		0, 0, 0, 0,
		0, 0, 0, 0,
		0, 0, 0, 0
	);

