/*
    This file is part of the FElt finite element analysis package.
    Copyright (C) 1993 Jason I. Gobat and Darren C. Atkinson

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

/***************************************************************************
 *
 * File:	draw.c
 *
 * Description: Contains the routines to draw a FELT structure in two
 *		or three dimensions.  DrawStructure is the only one
 *		that the user would ever call.  2d and 3d are in separate
 *		but really quite similar subroutines simply for clarity,
 *		the 3d stuff gets kind of nasty looking.
 *
 ****************************************************************************/

# include <stdio.h>
# include <string.h>
# include <math.h>
# include "draw.hpp"
# include "problem.h"
# include "error.h"

# define WindowWidth 400
# define WindowHeight 400

void WriteWireframe2D (FILE *fp, const std::vector< cvector1<Node> > &table, double mag)
{
   double	maxX, minX, maxY, minY, Xscale, Yscale;
   double	x, y;
   double	w, h;
   int		width, height;
   int		sx1, sy1;
   int		sx2, sy2;

   maxX = minX = table [1][1] -> x;
   maxY = minY = table [1][1] -> y;

   for (size_t i = 1 ; i <= table.size(); i++) {
      for (size_t j = 1 ; j <= 2; j++) {
         x = table [i][j] -> x + table [i][j] -> dx [1] * mag;
         y = table [i][j] -> y + table [i][j] -> dx [2] * mag;

         if (x > maxX) maxX = x;
         else if (x < minX) minX = x;

         if (y > maxY) maxY = y;
         else if (y < minY) minY = y;
      }
   }

   minX -= 0.05*(maxX - minX); 
   minY -= 0.05*(maxY - minY);
   maxX += 0.05*(maxX - minX);
   maxY += 0.05*(maxY - minY);
  
   height = WindowHeight;
   width = WindowWidth;
 
   w = (double) WindowWidth;
   h = (double) WindowHeight;

   if ((maxX - minX)/w > (maxY - minY)/h) {
      Xscale = (float) (w / (maxX - minX));
      Yscale = Xscale;
      height = (int) ((maxY - minY) * Yscale);
   }
   else {
      Yscale = (float) (h / (maxY - minY));
      Xscale = Yscale;
      width = (int) ((maxX - minX) * Xscale);
   }

   fprintf (fp, "%d, %d\n", width, height);

   for (size_t i = 1 ; i <= table.size(); i++) {
      sx1 = (int) ((table [i][1] -> x + table [i][1] -> dx [1] * mag  - minX) * Xscale);
      sy1 = (int) (height - (table [i][1] -> y + table [i][1] -> dx [2] * mag - minY) * Yscale);
      sx2 = (int) ((table [i][2] -> x + table [i][2] -> dx [1] * mag - minX) * Xscale);
      sy2 = (int) (height - (table [i][2] -> y + table [i][2] -> dx [2] * mag - minY)  * Yscale);

      fprintf (fp, "%d, %d, %d, %d\n", sx1, sy1, sx2, sy2);
   }
} 
