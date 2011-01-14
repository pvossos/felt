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

# include <math.h>
# include "error.h"
# include "fe.h"
# include "objects.h"
# include "meshgen.hpp"
# include "rules.h"

unsigned
GenerateQuadGrid(Grid grid, cvector1<Element> &element, cvector1<Node> &node,
                 unsigned int bnode, unsigned int belement)
{
   RuleFunction xrule_func;
   RuleFunction yrule_func;
   unsigned	ecount, ncount;
   unsigned	node1, node2, node3, node4;
   unsigned	i, j;
   unsigned	xnum,ynum;
   double	x_length,
		y_length;

   xnum = grid -> xnumber;
   ynum = grid -> ynumber;

   const size_t ne = xnum*ynum;

   if (ne <= 0) {
      error ("nothing to generate");
      return 1;
   }

   if (grid -> definition -> shapenodes != 4) {
      error ("quadrilateral grid generation requires four node elements");
      return 1;
   }

   const size_t nn = (xnum + 1)*(ynum + 1);

	/*
	 * allocate some memory to hold everything that we will generate
	 */

   node.resize(nn);
   for (i = 1 ; i <= nn ; i++) {
      if (!(node [i] = CreateNode (0)))
         Fatal ("allocation error in quadrilateral grid generation");
   }

   element.resize(ne);
   for (i = 1 ; i <= ne ; i++) {
      if (!(element [i] = CreateElement (0, grid -> definition)))
         Fatal ("allocation error in grid generation");
   }

	/*
	 * a couple of simple computations
	 */

   x_length = grid -> xe - grid -> xs;
   y_length = grid -> ye - grid -> ys;

   xrule_func = AssignRule (grid -> xrule);
   yrule_func = AssignRule (grid -> yrule);

	/*	
	 * generate a grid-work of nodes for all of our elements to use
	 */

   ncount = 0;
   for (j = 1 ; j <= ynum + 1 ; j++) {
      for (i = 1 ; i <= xnum + 1 ; i++) {
 
         ncount++;
         node [ncount] -> number = bnode + ncount;

         node [ncount] -> x = grid -> xs + xrule_func(i, xnum, x_length);
         node [ncount] -> y = grid -> ys + yrule_func(j, ynum, y_length);
         node [ncount] -> z = 0.0;

      }
   }

   ecount = 0;

	/*
	 * generate all the elements that run parallel to the x-axis
	 */

   for (j = 1 ; j <= ynum ; j++) {
      for (i = 1 ; i <= xnum ; i++) {

         ecount++;
         element [ecount] -> number = belement + ecount;

         node1 = i + (j - 1)*(xnum + 1);
         node2 = node1 + 1; 
         node4 = node1 + (xnum + 1);
         node3 = node4 + 1;

         element [ecount] -> node [1] = node [node1];
         element [ecount] -> node [2] = node [node2];
         element [ecount] -> node [3] = node [node3];
         element [ecount] -> node [4] = node [node4];
      }
   }

   return 0;
}
