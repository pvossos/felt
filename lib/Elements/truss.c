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

/************************************************************************
 * File:	truss.c							*
 *									*
 * Description:	This file contains the definition structure and		*
 *		stiffness function for a truss element.			*
 ************************************************************************/

# include <math.h>
# include <stdio.h>
# include "allocate.h"
# include "fe.h"
# include "error.h"
# include "misc.h"


static int trussEltSetup (Element element, char mass_mode, int tangent); 
static int trussEltStress (Element element); 

struct definition trussDefinition = {
    "truss", trussEltSetup, trussEltStress, 
    Linear, 2, 2, 1, 3, {0, 1, 2, 3, 0, 0, 0}, 0
};


static Matrix
TrussMassMatrix(Element element, char mass_mode)
{
   static Matrix	me = NullMatrix;
   double		L;
   double		factor;

   if (me == NullMatrix) {
      me = CreateMatrix (2,2);
      ZeroMatrix (me);
   }

   L = ElementLength (element, 3);

   if (mass_mode == 'l') {
      factor = (element -> material -> A * element -> material -> rho * L)/2.0;

      MatrixData (me) [1][1] = factor;
      MatrixData (me) [2][2] = factor;
   }
   else {
      factor = (element -> material -> A * element -> material -> rho * L)/6.0;

      MatrixData (me) [1][1] = 2.0*factor;
      MatrixData (me) [2][2] = 2.0*factor;
      MatrixData (me) [1][2] = factor; 
      MatrixData (me) [2][1] = factor; 
   }

   return me;
}

static Matrix
TrussTransformMatrix(Element element, double cx, double cy, double cz)
{
   double		L;
   static Matrix	T = NullMatrix;

   if (T == NullMatrix) 
      T = CreateMatrix (2,6);

   ZeroMatrix (T);

   L = ElementLength (element, 3);

   cx = (element -> node[2] -> x - element -> node[1] -> x)/L;
   cy = (element -> node[2] -> y - element -> node[1] -> y)/L;
   cz = (element -> node[2] -> z - element -> node[1] -> z)/L;

   MatrixData (T) [1][1] = cx;
   MatrixData (T) [1][2] = cy;
   MatrixData (T) [1][3] = cz;
   MatrixData (T) [2][4] = cx;
   MatrixData (T) [2][5] = cy;
   MatrixData (T) [2][6] = cz;

   return T;
}

static Vector
TrussEquivNodalForces(Element element, Matrix T, int *err_count)
{
   double		L;
   double		wa,wb;
   double		force1,
			force2;
   int			count;
   unsigned		i;
   static Matrix	Tt;
   static Vector 	equiv = NullMatrix;
   static Vector	result;
 
   if (equiv == NullMatrix) {
      equiv = CreateVector (2);
      result = CreateVector (6);
      Tt = CreateMatrix (6,2);
   }

   count = 0;
   wa = wb = force1 = force2 = 0; /* gcc -Wall */
 
   if (element -> numdistributed != 1) {
      error ("element %d can only have one distributed load", element -> number);
      count ++;
   }

   if (element -> distributed[1] -> nvalues != 2) {
      error ("truss elt %d does not have 2 nodal values for a distributed load",
              element -> number);
      count++;
   }

   L = ElementLength (element, 3);

   if (L <= TINY) {
      error ("length of element %d is zero to machine precision",element -> number);
      count ++;
   } 

   if (element -> distributed[1] -> direction != Parallel) {
       error ("invalid direction specified for beam elt %d distributed load",
              element -> number);
       count++;
   }

   for (i = 1 ; i <= element -> distributed[1] -> nvalues ; i++) {
      if (element -> distributed[1] -> value[i].node < 1 || 
          element -> distributed[1] -> value[i].node > 2) {

          error ("incorrect node numbering for beam elt %d distributed load",
                  element -> number);
          count++;
      }
   }

   if (element -> distributed[1] -> value[1].node == 
       element -> distributed[1] -> value[2].node) {

       error ("incorrect node numbering for elt %d distributed load", 
               element -> number);
       count++;
   }

	/* 
	 * Thats all the error checking, bail out if we've had any
	 */

   if (count) {
      *err_count = count;
      return NullMatrix;
   }

   if (element -> distributed[1] -> value[1].node == 1) {
      wa = element -> distributed[1] -> value[1].magnitude;
      wb = element -> distributed[1] -> value[2].magnitude;
   }
   else if (element -> distributed[1] -> value[1].node == 2) {
      wb = element -> distributed[1] -> value[1].magnitude;
      wa = element -> distributed[1] -> value[2].magnitude;
   }

   if (wa == wb) 
      force1 = force2 = wa*L/2;
   else if (fabs (wa) > fabs (wb)) {
      force1 = wb*L/2 + (wa - wb)*L/3;
      force2 = wb*L/2 + (wa - wb)*L/6;
   }
   else if (fabs (wb) > fabs (wa)) {
      force1 = wa*L/2 + (wb - wa)*L/6;
      force2 = wa*L/2 + (wb - wa)*L/3;
   }

   VectorData (equiv) [1] = force1;
   VectorData (equiv) [2] = force2;

	/*
	 * Now that we know all is okay, allocate some memory if we
	 * haven't already done so for some other element
	 */

   SetEquivalentForceMemory (element);

   TransposeMatrix (Tt, T);
   MultiplyMatrices (result, Tt, equiv);

   *err_count = 0;
   return result; 
}

static double
AxialDisplacement(Element e, double cx, double cy, double cz)
{
   double	dx1, dx2;
   double	dy1, dy2;
   double	dz1, dz2;
   double	stretch;

   dx1 = e -> node[1] -> dx[1];
   dy1 = e -> node[1] -> dx[2];
   dz1 = e -> node[1] -> dx[3];
   dx2 = e -> node[2] -> dx[1];
   dy2 = e -> node[2] -> dx[2];
   dz2 = e -> node[2] -> dx[3];

   stretch = cx*dx2 + cy*dy2 + cz*dz2 - cx*dx1 - cy*dy1 - cz*dz1;

   return stretch;
}

static int
trussEltSetup(Element element, char mass_mode, int tangent)
{
   double		AEonL,L;
   Matrix		T;
   static Vector	equiv;
   int			count;
   static Matrix	ke = NullMatrix;
   Matrix		me;
   double		factor;
   double		sign;
   double		cx, cy, cz;
   unsigned		i, j;

   if (ke == NullMatrix) {
      equiv = CreateVector (6);
      ke = CreateMatrix (2,2);
   }

   L = ElementLength (element, 3); 

   if (L <= TINY) {
      error ("length of element %d is zero to machine precision",element -> number);
      return 1;
   } 

	/*
	 * create a stiffness matrix for this element if it doesn't
	 * already have one and do some one time checks to make sure
	 * the material properties are ok
	 */

   if (element -> K == NullMatrix) {
      element -> K = CreateMatrix (6,6);

      if (element -> material -> E == 0) {
         error ("truss element %d has 0.0 for Young's modulus (E)", element -> number);
         return 1;
      }
      if (element -> material -> A == 0) {
         error ("truss element %d has 0.0 for cros-sectional area (A)", element -> number);
         return 1;
      }
   }

	/*
	 * calculate the linear stiffness portion
	 */

   AEonL = (element -> material -> A * element -> material -> E / L);

   cx = (element -> node[2] -> x - element -> node[1] -> x)/L;
   cy = (element -> node[2] -> y - element -> node[1] -> y)/L;
   cz = (element -> node[2] -> z - element -> node[1] -> z)/L;

   MatrixData (ke) [1][1] = AEonL;
   MatrixData (ke) [1][2] = -AEonL;
   MatrixData (ke) [2][1] = -AEonL;
   MatrixData (ke) [2][2] = AEonL;

   T = TrussTransformMatrix (element, cx, cy, cz);

   MultiplyAtBA (element -> K,T,ke);

	/*
	 * now if we need to, we add in the nonlinear portion
	 * such that the result will be the tangent stiffness matrix -
	 * we'll also fill the matrix internal force so that we
	 * will be able to assemble a residual load vector later
	 */

   if (tangent) {
      factor = AEonL*AxialDisplacement (element, cx, cy, cz);
 
      for (i = 1 ; i <= 2 ; i++) {
         for (j = 1 ; j <= 2 ; j++) {

            if (i == j)
               sign = 1;
            else
               sign = -1;

            element -> K -> data [i*3 - 2][j*3 - 2] += sign*factor*(1 - cx*cx);
            element -> K -> data [i*3 - 2][j*3 - 1] += -sign*factor*cx*cy;
            element -> K -> data [i*3 - 2][j*3]     += -sign*factor*cx*cz;
            element -> K -> data [i*3 - 1][j*3 - 2] += -sign*factor*cx*cy;
            element -> K -> data [i*3 - 1][j*3 - 1] += sign*factor*(1 - cy*cy);
            element -> K -> data [i*3 - 1][j*3]     += -sign*factor*cy*cz;
            element -> K -> data [i*3][j*3 - 2]     += -sign*factor*cx*cz;
            element -> K -> data [i*3][j*3 - 1]     += -sign*factor*cy*cz;
            element -> K -> data [i*3][j*3]         += sign*factor*(1 - cz*cz);
         }
      }

      if (element -> f == NullMatrix)
         element -> f = CreateColumnVector (6);

      element -> f -> data [i][1] = cx*factor*L;
      element -> f -> data [i][2] = cy*factor*L;
      element -> f -> data [i][3] = cz*factor*L;
      element -> f -> data [i][4] = -element -> f -> data [i][1]; 
      element -> f -> data [i][5] = -element -> f -> data [i][2]; 
      element -> f -> data [i][6] = -element -> f -> data [i][3]; 
   }
   
	/*
	 * assemble the element load vector due to distributed loads
	 */

   if (element -> numdistributed > 0) {
      equiv = TrussEquivNodalForces (element, T, &count);
      if (equiv == NullMatrix)
         return count; 

      element -> node[1] -> eq_force[1] += VectorData (equiv) [1];
      element -> node[1] -> eq_force[2] += VectorData (equiv) [2];
      element -> node[1] -> eq_force[3] += VectorData (equiv) [3];
      element -> node[2] -> eq_force[1] += VectorData (equiv) [4];
      element -> node[2] -> eq_force[2] += VectorData (equiv) [5];
      element -> node[2] -> eq_force[3] += VectorData (equiv) [6];
   }

	/*
	 * generate a mass matrix
	 */

   if (mass_mode) {
      me = TrussMassMatrix (element, mass_mode);

      if (me == NullMatrix)
         return 1;

      if (element -> M == NullMatrix)
         element -> M = CreateMatrix (6,6);
       
      MultiplyAtBA (element -> M, T, me);
   }

   return 0;
}

static int
trussEltStress(Element element)
{
   double	EonL;
   double	cx,cy,cz;
   double	L;
   double	stress;
		
   L = ElementLength (element, 3);

   if (L <= TINY) {
      error ("length of element %d is zero to machine precision",element -> number);
      return 1;
   } 

   EonL = element -> material -> E / L;

   cx = (element -> node[2] -> x - element -> node[1] -> x)/L;
   cy = (element -> node[2] -> y - element -> node[1] -> y)/L;
   cz = (element -> node[2] -> z - element -> node[1] -> z)/L;

   stress = EonL*AxialDisplacement(element, cx, cy, cz);

   element -> ninteg = 1;
   SetupStressMemory (element);
  
   element -> stress[1] -> x = (element -> node[1] -> x + 
                                element -> node[2] -> x)/2.0;
   element -> stress[1] -> y = (element -> node[1] -> y + 
                                element -> node[2] -> y)/2.0;

   element -> stress[1] -> values[1] = stress;

   return 0;
}
