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
 * File:	ctg.c							*
 *									*
 * Description: This file contains the definition structure and the	*
 *		stiffness and stress functions for the constant 	*
 *		temperature gradient triangular element for heat 	*
 *		transfer problems.					*
 ************************************************************************/

# include <math.h>
# include <stdio.h>
# include "allocate.h"
# include "fe.h"
# include "error.h"
# include "misc.h"

int CTGLumpedCapacityMatrix ( );
int CTGConsistentCapacityMatrix ( );
Vector	CTGResolveConvection ( );
Matrix	CTGLocalB ( );
Matrix	PlanarConductivity ( );
int	ctgEltSetup ( );
int	ctgEltStress ( );

struct definition ctgDefinition = {
   "ctg", ctgEltSetup, ctgEltStress, 
   Planar, 3, 3, 0, 1, {0, 1, 0, 0, 0, 0, 0}, 0
};

int ctgEltSetup (element, mass_mode, tangent)
   Element	element;
   char		mass_mode;
   int		tangent;
{
   unsigned		i;
   Vector		equiv;
   int			count;
   Matrix		B,
			D;
   double		factor;
   double		area;

   if (element -> material -> Kx == 0) {
      error ("CTG element %d has 0.0 for x-conductivity (Kx)", element -> number);
      return 1;
   }
   if (element -> material -> Ky == 0) {
      error ("CTG element %d has 0.0 for y-conductivity (Ky)", element -> number);
      return 1;
   }
   if (element -> material -> t == 0) {
      error ("CTG element %d has 0.0 for thickness (t)", element -> number);
      return 1;
   }
   if (mass_mode && element -> material -> c == 0) {
      error ("CTG element %d has 0.0 for heat capacitance (c)", element -> number);
      return 1;
   }

   B = CTGLocalB (element, &area);
   if (B == NullMatrix)
      return 1;
   
   D = PlanarConductivity (element);

   if (D == NullMatrix)
      return 1;

   factor = element -> material -> t * area;
   
   if (element -> K == NullMatrix)
      element -> K = CreateMatrix (3,3);

   MultiplyAtBA (element -> K, B, D);

   ScaleMatrix (element -> K, element -> K, factor, 0.0);

   if (element -> numdistributed > 0) {
      equiv = CTGResolveConvection (element, &count);
      if (equiv == NullMatrix)
         return count;

       for (i = 1; i <= 3 ; i++) 
          element -> node[i] -> eq_force[1] += VectorData (equiv) [i];
   }

   if (mass_mode) {
      if (element -> M == NullMatrix)
         element -> M = CreateMatrix (3,3);
     
      if (mass_mode == 'l') 
         CTGLumpedCapacityMatrix (element, area);
      else if (mass_mode == 'c') 
         CTGConsistentCapacityMatrix (element, area);
   }

   return 0;
}

int ctgEltStress (element)
   Element	element;
{
   element -> ninteg = 0;
   return 0;
} 

int CTGLumpedCapacityMatrix (e, A)
   Element	e;
   double	A;
{
   double	factor;

   factor = e -> material -> t * e -> material -> c *
            e -> material -> rho * A / 3;

   ZeroMatrix (e -> M);
   MatrixData (e -> M) [1][1] = factor;
   MatrixData (e -> M) [2][2] = factor;
   MatrixData (e -> M) [3][3] = factor;

   return 0;
}

int CTGConsistentCapacityMatrix (e, area)
   Element	e;
   double	area;
{
   return 0;
}

Matrix PlanarConductivity (element)
   Element	element;
{
   static Matrix	D = NullMatrix;

   if (D == NullMatrix) {
      D = CreateMatrix (2,2);
      ZeroMatrix (D);
   }

   MatrixData (D) [1][1] = element -> material -> Kx;
   MatrixData (D) [2][2] = element -> material -> Ky;

   return D;
}

Matrix CTGLocalB (element, area)
   Element	element;
   double	*area;
{
   static Matrix 	B = NullMatrix;
   double		xc1,yc1,
			xc2,yc2,
			xc3,yc3,
             		beta[4],
			gamma[4],
			A,
			factor;
   unsigned		j;

   if (B == NullMatrix) 
      B = CreateMatrix (2,3);

   ZeroMatrix (B);

   xc1 = element -> node[1] -> x;
   xc2 = element -> node[2] -> x;
   xc3 = element -> node[3] -> x;
   yc1 = element -> node[1] -> y;
   yc2 = element -> node[2] -> y;
   yc3 = element -> node[3] -> y;

   beta[1] = yc2 - yc3;
   beta[2] = yc3 - yc1;
   beta[3] = yc1 - yc2;

   gamma[1] = xc3 - xc2;
   gamma[2] = xc1 - xc3;
   gamma[3] = xc2 - xc1;

   A = 0.5*(xc1*(beta[1]) + xc2*(beta[2]) + xc3*(beta[3]));
   
   if (A < 0) {
      error("incorrect node ordering for element %d (must be ccw)",element -> number);
      return NullMatrix;
   }
   if (A == 0) {
      error ("area of element %d is zero, check node numbering",element -> number);
      return NullMatrix;
   }
   
   for (j = 1 ; j <= 3 ; j++) {
      MatrixData (B) [1][j] = beta[j];
      MatrixData (B) [2][j] = gamma[j];
   }

   factor = 0.5/A;
   ScaleMatrix (B,B,factor,0.0);

   if (area != NULL)
      (*area) = A;

   return B;
}

Vector CTGResolveConvection (element, err_count)
   Element	element;
   int		*err_count;
{
   double		L;
   double		factor;
   int			count;
   double		xc1,xc2,
			yc1,yc2;
   double		thick;
   double		conv_coeff;
   double		Tinf;
   unsigned		node_a,
			node_b;
   unsigned		i;
   static Vector 	equiv = NullMatrix;
   static Matrix	convK;
 
   if (equiv == NullMatrix) {
      equiv = CreateVector (3);
      convK = CreateMatrix (3,3);
   }

   count = 0;
 
   if (element -> numdistributed > 3) {
      error ("ctg element %d can have at most three convecting edges",
              element -> number);
      count++;
   }

   thick = element -> material -> t;

   ZeroMatrix (convK);

   for (i = 1 ; i <= 3 ; i++)
      VectorData (equiv) [i] = 0.0;

   for (i = 1 ; i <= element -> numdistributed ; i++) {

      if (element -> distributed[i] -> nvalues != 2) {
         error ("convection %s does not have 2 nodal values (element %d)",
                 element -> distributed[i] -> name,element -> number);
         count++;
      }

      node_a = element -> distributed[i] -> value[1].node;
      node_b = element -> distributed[i] -> value[2].node;

      if (node_a < 1 || node_a > 3 || node_b < 1 || node_b > 3) {
         error ("incorrect node numbering for convection %s (element %d)", 
                element -> distributed[i] -> name,element -> number);
         count++;
      }

      if (node_a == node_b) {
         error ("incorrect node numbering for convection %s (element %d)", 
                element -> distributed[i] -> name,element -> number);
         count++;
      }

	/* 
	 * Thats all the error checking we can do right now, 
	 * bail out if we've had any
	 */

      if (count) {
         *err_count = count;
         return NullMatrix;
      }

      xc1 = element -> node[node_a] -> x;
      xc2 = element -> node[node_b] -> x;
      yc1 = element -> node[node_a] -> y;
      yc2 = element -> node[node_b] -> y;

      L = sqrt ((xc1 - xc2)*(xc1 - xc2) + (yc1 - yc2)*(yc1 - yc2));

      if (L <= TINY) {
         error ("length of side of element %d is zero to machine precision",
                 element -> number);
         *err_count = 1;
         return NullMatrix;
      } 

	/*
	 * calculate the additional "force" that we will store in the
	 * nodes eq_force structure
	 */

      conv_coeff = element -> distributed[i] -> value[1].magnitude;
      Tinf = element -> distributed[i] -> value[2].magnitude;
      factor = conv_coeff*Tinf*L*thick/2.0;

      VectorData (equiv) [node_a] += factor;
      VectorData (equiv) [node_b] += factor;

	/*
 	 * calculate the contribution of this convecting edge to
	 * the overall element stiffness matrix
	 */

      factor = conv_coeff*L*thick/6.0;

      MatrixData (convK) [node_a][node_a] += 2.0*factor;
      MatrixData (convK) [node_b][node_b] += 2.0*factor;       
      MatrixData (convK) [node_a][node_b] += factor;
      MatrixData (convK) [node_b][node_a] += factor;       
   }

	/*	
	 * add all of the convective contributions into the
	 * element -> K stiffness matrix
	 */

   AddMatrices (element -> K, element -> K, convK);

	/*
	 * Now that we know all is okay, allocate some memory if we
	 * haven't already done so for some other element
	 */

   SetEquivalentForceMemory (element);

   *err_count = 0;
   return equiv; 
}
