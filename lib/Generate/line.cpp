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
GenerateLine (Line line, cvector1<Element> &element, cvector1<Node> &node,
              unsigned int bnode, unsigned int belement)
{
   RuleFunction rule_func;
   unsigned	i;
   double	L;
   double	Lx;
   double	Ly;
   double	Lz;
   double	theta;
   double	phi;

   size_t ne = line -> number;
   size_t nn = ne + 1;

   if (ne <= 0) {
      error ("nothing to generate");
      return 1;
   } 

   if (line -> definition -> numnodes != 2) {
      error ("line generation requires two node elements");
      return 1;
   }

	/*
	 * allocate some memory to hold everything that we will generate
	 */

   node.resize(nn);
   for (i = 1 ; i <= nn ; i++) {
      if (!(node [i] = CreateNode (0)))
         Fatal ("allocation error in line generation");
   }

   element.resize(ne);
   for (i = 1 ; i <= ne ; i++) {
      if (!(element [i] = CreateElement (0, line -> definition)))
         Fatal ("allocation error in line generation");
   }

	/*
	 * figure out the spacing function
	 */

   rule_func = AssignRule(line -> rule);

   Lx = line -> xe - line -> xs;
   Ly = line -> ye - line -> ys;
   Lz = line -> ze - line -> zs;

	/*
	 * generate the nodal coordinates _along_ the line
	 */

   L     = sqrt(Lx*Lx + Ly*Ly + Lz*Lz);
   theta = atan2(Ly, Lx);
   phi   = atan2(sqrt(Lx*Lx + Ly*Ly), Lz);

   cvector1<double> x(nn);
   for (i = 1 ; i <= nn ; i++) 
      x [i] = rule_func(i, ne, L);

	/*	
	 * generate all the nodes
	 */
   
   for (i = 1 ; i <= nn ; i++) {
      node [i] -> number = i + bnode;
      node [i] -> x = line -> xs + x[i]*sin(phi)*cos(theta);
      node [i] -> y = line -> ys + x[i]*sin(phi)*sin(theta);
      node [i] -> z = line -> zs + x[i]*cos(phi); 
   }

	/*
	 * attach all the elements to the nodes
	 */

   for (i = 1 ; i <= ne ; i++) {

      element [i] -> number = i + belement;
      element [i] -> node [1] = node [i];
      element [i] -> node [2] = node [i + 1];

   } 

   return 0;
}
