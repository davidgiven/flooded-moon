pngtopnm Global.png | pnmscale -xscale 2 -yscale 8 | pnmsmooth -size 3 9 | pnmpaste NorthPole.pgm 0 0 | pnmpaste SouthPole.pgm 0 21760 | pgmtopng > Huge.png
