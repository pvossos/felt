/*
    This file is part of the FElt finite element analysis package.
    Copyright (C) 1993-1997 Jason I. Gobat and Darren C. Atkinson

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

/************************************************************************
 *									*
 * File:	spring.c						*
 *									*
 * Description:	This file contains the definition structure and		*
 *		stiffness function for a simple linear one-dimensional	*
 *		spring element.						*
 *									*
 ************************************************************************/

# include <math.h>
# include <stdio.h>
# include "allocate.h"
# include "fe.h"
# include "error.h"
# include "misc.h"


int springEltSetup ( ), springEltStress ( );

struct definition springDefinition = {
    "spring", springEltSetup, springEltStress, 
    Linear, 2, 2, 1, 1, {0, 1, 0, 0, 0, 0, 0}, 0
};


int springEltSetup (element, mass_mode, tangent)
   Element 	element;
   char		mass_mode;
   int		tangent;
{
   double		factor, L;

   L = ElementLength (element, 1); 

   if (L <= TINY) {
      error ("length of element %d is zero to machine precision",element -> number);
      return 1;
   } 

   if (element -> material -> E == 0) {
      error ("spring element %d has 0.0 for Young's modulus (E)", element -> number);
      return 1;
   }
   if (element -> material -> A == 0) {
      error ("spring element %d has 0.0 for cros-sectional area (A)", element -> number);
      return 1;
   }

   factor = (element -> material -> A * element -> material -> E / L);

   if (element -> K == NullMatrix)
      element -> K = CreateMatrix (2, 2);

   MatrixData (element -> K) [1][1] = factor;
   MatrixData (element -> K) [1][2] = -factor;
   MatrixData (element -> K) [2][1] = -factor;
   MatrixData (element -> K) [2][2] = factor;

   if (mass_mode) {
      if (element -> M == NullMatrix)
         element -> M = CreateMatrix (2, 2);

      ZeroMatrix (element -> M);

      if (mass_mode == 'l') {
         factor = (element -> material -> A * element -> material -> rho * L)/2.0;

         MatrixData (element -> M) [1][1] = factor;
         MatrixData (element -> M) [2][2] = factor;
      }
      else {
         factor = (element -> material -> A * element -> material -> rho * L)/6.0;

         MatrixData (element -> M) [1][1] = 2.0*factor;
         MatrixData (element -> M) [2][2] = 2.0*factor;
         MatrixData (element -> M) [1][2] = factor; 
         MatrixData (element -> M) [2][1] = factor; 
      }
   }

   return 0;
}

int springEltStress (element)
   Element	element;
{
   double	cx, dx1, dx2;
   double	L, EonL,
		stress;
		
   L = ElementLength (element, 1);

   if (L <= TINY) {
      error ("length of element %d is zero to machine precision",element -> number);
      return 1;
   } 

   cx = (element -> node[2] -> x - element -> node[1] -> x)/L;

   EonL = element -> material -> E / L;

   dx1 = element -> node[1] -> dx[1];
   dx2 = element -> node[2] -> dx[1];

   stress = EonL*(cx*dx2 - cx*dx1);

   element -> ninteg = 1;
   SetupStressMemory (element);
  
   element -> stress[1] -> x = (element -> node[1] -> x + 
                                element -> node[2] -> x)/2.0;
   element -> stress[1] -> y = 0.0;

   element -> stress[1] -> values[1] = stress;

   return 0;
}
