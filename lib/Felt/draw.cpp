/*
    This file is part of the FElt finite element analysis package.
    Copyright (C) 1993-2000 Jason I. Gobat and Darren C. Atkinson

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
 * Description:	Contains code to draw ASCII pictures of the structures
 *		described by a FElt problem instance
 *
 ***************************************************************************/

# include <vector>
# include <stdio.h>
# include <math.h>
# include "draw.hpp"
# include "problem.h"
# include "fe.h"
# include "error.h"

# define XSIZE  10	/* "pixels" / inch in x direction		*/
# define YSIZE  4.8     /* "pixels" / inch in y direction		*/

# define Sign(x) ((x) >= 0 ? 1 : -1)

/*****************************************************************************
 *
 * Function:	Line
 *
 * Description:	draws a line using the Bresenham integer algorithm
 *
 *****************************************************************************/

static void
Line(std::vector< std::vector<char> >&b, int xs, int ys, int xe, int ye)
{
   int		x,y,
		dx,dy,
		e;
   int		signx, signy,
		temp;
   unsigned	change;

   if (xe == xs && ye == ys)
      return;

   signx = Sign (xe - xs);
   signy = Sign (ye - ys);

   dx = signx*(xe - xs);
   dy = signy*(ye - ys);

   if (dy <= dx)
      change = 0;
   else {
      change = 1;
      temp = dx;
      dx = dy;
      dy = temp;
   }

   e = 2*dy - dx;
   x = xs;
   y = ys;

   for (int i = 0 ; i <= dx ; i++) {
      b [y][x] = '+';

      while (e >= 0) {
         if (!change)
            y += signy;
         else
            x += signx;
         
         e -= 2*dx;
      }

      if (!change)
         x += signx;
      else
         y += signy;

      e += 2*dy;
   }

   b [ys][xs] = 'O';
   b [ye][xe] = 'O';

   return;
}

/**************************************************************************
 *
 * Function:	DrawStructureASCII
 *
 ***************************************************************************/

void
DrawStructureASCII(FILE *fp, unsigned cols, unsigned rows)
{
    unsigned	  i, j;
    Element	 *e;
    unsigned ne;
    int		  xs, ys;
    int		  xe, ye;
    double	  min_x, max_x;
    double	  min_y, max_y;
    double	  xscale, yscale;
    int		  w, h;
    double	  ar;

    const Node *n = problem.nodes.c_ptr1();
    e = problem.elements;
    const unsigned nn = problem.nodes.size();
    ne = problem.num_elements;

	/*
	 * allocate the bitmap and clear it
	 */

    w = cols - 1;
    h = rows - 1; 

    std::vector< std::vector<char> > b(rows);
    for (i = 0 ; i < rows ; i++)
        b [i] = std::vector<char>(cols, ' ');

    	/*
	 * figure out the scaling
	 */

    min_x = max_x = n [1] -> x;
    min_y = max_y = n [1] -> y;

    for (i = 1 ; i <= nn ; i++) {
        if (n [i] -> x > max_x)
            max_x = n [i] -> x;
        else if (n [i] -> x < min_x)
            min_x = n [i] -> x;

        if (n [i] -> y > max_y)
            max_y = n [i] -> y;
        else if (n [i] -> y < min_y)
            min_y = n [i] -> y;
    }

    ar = XSIZE / YSIZE;

    if (XSIZE*(max_x - min_x) / w > YSIZE*(max_y - min_y) / h) {
        xscale = (max_x - min_x) / (double) w;
        yscale = xscale*ar;
    }
    else {
        yscale = (max_y - min_y) / (double) h;
        xscale = yscale/ar;
    }

    for (i = 1 ; i <= ne ; i++) {
        if (e [i] -> definition -> shapenodes == 2) {
            xs = (int) ((e[i] -> node [1] -> x - min_x)/xscale);     
            xe = (int) ((e[i] -> node [2] -> x - min_x)/xscale);     
            ys = (int) ((e[i] -> node [1] -> y - min_y)/yscale);     
            ye = (int) ((e[i] -> node [2] -> y - min_y)/yscale);     
            Line (b, xs, h - ys, xe, h - ye);
        }
        else {
            xs = (int) ((e[i] -> node [1] -> x - min_x)/xscale);     
            ys = (int) ((e[i] -> node [1] -> y - min_y)/yscale);     
            
            for (j = 2 ; j <= e[i] -> definition -> shapenodes ; j++) {
                xe = (int) ((e[i] -> node [j] -> x - min_x)/xscale);     
                ye = (int) ((e[i] -> node [j] -> y - min_y)/yscale);     
                Line (b, xs, h - ys, xe, h - ye);
                xs = xe;
                ys = ye;
            }

            xe = (int) ((e[i] -> node [1] -> x - min_x)/xscale);     
            ye = (int) ((e[i] -> node [1] -> y - min_y)/yscale);     
            Line (b, xs, h - ys, xe, h - ye);
        }
    }

   	/* 
	 * print the bitmap to the screen
	 */

    for (i = 0 ; i < rows ; i++) {
        for (j = 0 ; j < cols ; j++)
            fprintf (fp, "%c", b [i][j]); 

        fprintf (fp, "\n");
    }

    return;
}
