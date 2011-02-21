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

/****************************************************************************
 *
 * File:	brick_grid.c
 *
 ***************************************************************************/

# include <math.h>
# include "error.h"
# include "fe.h"
# include "meshgen.hpp"
# include "rules.h"

unsigned 
GenerateBrickGrid(Grid grid, cvector1<Element> &element, cvector1<Node> &node, 
                  unsigned int bnode, unsigned int belement)
{
   RuleFunction xrule_func;
   RuleFunction yrule_func;
   RuleFunction zrule_func;
   unsigned	ecount, ncount;
   unsigned	nodenum [9];
   unsigned	i, j, k;
   unsigned	m;
   unsigned	xnum, ynum, znum;
   double	x_length;
   double	y_length;
   double	z_length;

   xnum = grid -> xnumber;
   ynum = grid -> ynumber;
   znum = grid -> znumber;

   const size_t ne = xnum*ynum*znum;

   if (ne <= 0) {
      error ("nothing to generate");
      return 1;
   }

   if (grid -> definition -> shapenodes != 8) {
      error ("brick grid generation requires eight node elements");
      return 1;
   }

   const size_t nn = (xnum + 1)*(ynum + 1)*(znum + 1);

	/*
	 * allocate some memory to hold everything that we will generate
	 */

   node.resize(nn);

   for (i = 1 ; i <= nn ; i++)
       node[i] = new node_t;

   element.resize(ne);

   for (i = 1 ; i <= ne ; i++)
       element [i].reset(new element_t(0, grid -> definition));

	/*
	 * a couple of simple computations
	 */

   x_length = grid -> xe - grid -> xs;
   y_length = grid -> ye - grid -> ys;
   z_length = grid -> ze - grid -> zs;

   xrule_func = AssignRule (grid -> xrule);
   yrule_func = AssignRule (grid -> yrule);
   zrule_func = AssignRule (grid -> zrule);

	/*	
	 * generate a grid-work of nodes for all of our elements to use
	 */

   ncount = 0;
   for (k = 1 ; k <= znum + 1 ; k++) {
      for (j = 1 ; j <= ynum + 1 ; j++) {
         for (i = 1 ; i <= xnum + 1 ; i++) {
 
            ncount++;
            node [ncount] -> number = bnode + ncount;

            node [ncount] -> x = grid -> xs + xrule_func(i, xnum, x_length);
            node [ncount] -> y = grid -> ys + yrule_func(j, ynum, y_length);
            node [ncount] -> z = grid -> zs + zrule_func(k, znum, z_length);

         }
      }
   }

   ecount = 0;

	/*
	 * generate all the elements that run parallel to the x-axis
	 */

   for (k = 1 ; k <= znum ; k++) {
      for (j = 1 ; j <= ynum ; j++) {
         for (i = 1 ; i <= xnum ; i++) {

            ecount++;
            element [ecount] -> number = belement + ecount;

            nodenum[1] = i + (j - 1)*(xnum + 1) + (k - 1)*(xnum + 1)*(ynum + 1);
            nodenum[2] = nodenum[1] + 1; 
            nodenum[4] = nodenum[1] + (xnum + 1);
            nodenum[3] = nodenum[4] + 1;
            for (m = 5 ; m <= 8 ; m++)
               nodenum[m] = nodenum[m-4] + (xnum + 1)*(ynum + 1);

            for (m = 1 ; m <= 8 ; m++)
               element [ecount] -> node [m] = node [nodenum[m]];
         }
      }
   }

   return 0;
}
