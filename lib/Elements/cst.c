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
 * File:	cst.c							*
 *									*
 * Description: This file contains the definition structure and the	*
 *		stiffness function for plane stress / plane strain	* 
 *		CST elements.						* 	
 ************************************************************************/

# include <math.h>
# include <stdio.h>
# include "allocate.h"
# include "fe.h"
# include "error.h"
# include "misc.h"

# define PLANESTRESS 1
# define PLANESTRAIN 2

int CSTPlaneStrainEltSetup ( ), CSTPlaneStrainEltStress ( );
int CSTPlaneStressEltSetup ( ), CSTPlaneStressEltStress ( );

struct definition CSTPlaneStrainDefinition = {
    "CSTPlaneStrain", CSTPlaneStrainEltSetup, CSTPlaneStrainEltStress, 
    Planar, 3, 3, 10, 2, {0, 1, 2, 0, 0, 0, 0}, 0
};

struct definition CSTPlaneStressDefinition = {
    "CSTPlaneStress", CSTPlaneStressEltSetup, CSTPlaneStressEltStress, 
    Planar, 3, 3, 10, 2, {0, 1, 2, 0, 0, 0, 0}, 0
};

void    CSTLumpedMassMatrix ( );
Matrix  CSTLocalB 	    ( );
Vector	CSTEquivNodalForces ( );
int	CSTElementSetup ( );
int	CSTElementStress    ( );

int CSTPlaneStrainEltSetup (element, mass_mode, tangent)
   Element	element;
   char		mass_mode;
   int		tangent;
{
   return CSTElementSetup (element, mass_mode, tangent, PLANESTRAIN);
}

int CSTPlaneStressEltSetup (element, mass_mode, tangent)
   Element	element;
   char		mass_mode;
   int		tangent;
{
   return CSTElementSetup (element, mass_mode, tangent, PLANESTRESS);
}

int CSTPlaneStressEltStress (element)
   Element	element;
{
   return CSTElementStress (element, PLANESTRESS);
}

int CSTPlaneStrainEltStress (element)
   Element	element;
{
   return CSTElementStress (element,  PLANESTRAIN);
}

int CSTElementSetup (element, mass_mode, tangent, type)
   Element	element;
   char		mass_mode;
   int		tangent;
   unsigned	type;
{
   unsigned		i;
   Vector		equiv;
   int			count;
   Matrix		B,
			D;
   double		factor;
   double		area;

   if (element -> material -> nu == 0) {
      error ("CST element %d has 0.0 for Poisson's ratio (nu)", element -> number);
      return 1;
   }
   if (element -> material -> E == 0) {
      error ("CST element %d has 0.0 for Young's modulus (E)", element -> number);
      return 1;
   }
   if (element -> material -> t == 0) {
      error ("CST element %d has 0.0 for thickness (t)", element -> number);
      return 1;
   }

   B = CSTLocalB (element, &area);
   if (B == NullMatrix)
      return 1;
   
   if (type == PLANESTRAIN)
      D = PlaneStrainD (element);
   else if (type == PLANESTRESS)
      D = PlaneStressD (element);
   else
      D = NullMatrix; /* gcc -Wall */

   if (D == NullMatrix)
      return 1;

   factor = element -> material -> t * area;
   
   if (element -> K == NullMatrix)
      element -> K = CreateMatrix (6,6);

   MultiplyAtBA (element -> K, B, D);

   ScaleMatrix (element -> K, element -> K, factor, 0.0);

   if (element -> numdistributed > 0) {
      equiv = CSTEquivNodalForces (element, &count);
      if (equiv == NullMatrix)
         return count;

       for (i = 1; i <= 3 ; i++) {
          element -> node[i] -> eq_force[1] += VectorData (equiv) [2*i - 1];
          element -> node[i] -> eq_force[2] += VectorData (equiv) [2*i];
       }
   }

	/*	
	 * form the element mass matrix if necessary (note that we only
	 * have a lumped formulation for now)
	 */

   if (mass_mode) {
      if (element -> M == NullMatrix)
         element -> M = CreateMatrix (6,6);

      if (mass_mode == 'l')
         CSTLumpedMassMatrix (element, area);
      else
         CSTLumpedMassMatrix (element, area);
   }

   return 0;
}

int CSTElementStress (element, type)
   Element	element;
   unsigned	type;
{
   static Vector	stress = NullMatrix,
			d;
   unsigned		i, j;
   static Matrix	temp;
   Matrix		D,
			B;
   double		x,y;
   double		sigma_x,
			sigma_y,
			tau_xy;
   
   if (stress == NullMatrix) {
      stress = CreateVector (3);
      d = CreateVector (6);
      temp = CreateMatrix (3,6);
   }

   B = CSTLocalB (element, NULL);
   if (B == NullMatrix)
      return 1;

   if (type == PLANESTRAIN)
      D = PlaneStrainD (element);
   else if (type == PLANESTRESS)
      D = PlaneStressD (element);
   else
      D = NullMatrix; /* gcc -Wall */

   if (D == NullMatrix)
      return 1;

   x = 0;
   y = 0;
   for (i = 1; i <= 3 ; i++) {
      VectorData (d) [2*i - 1] = element -> node[i] -> dx[1];
      VectorData (d) [2*i] = element -> node[i] -> dx[2];
      x += element -> node[i] -> x;
      y += element -> node[i] -> y;
   }

   MultiplyMatrices (temp, D, B);  

   MultiplyMatrices (stress, temp, d);
  
   sigma_x = VectorData (stress) [1];
   sigma_y = VectorData (stress) [2];
   tau_xy = VectorData (stress) [3];

   element -> ninteg = 1;
   SetupStressMemory (element);

   element -> stress [1] -> x = x/3;
   element -> stress [1] -> y = y/3;

   element -> stress [1] -> values [1] = sigma_x;
   element -> stress [1] -> values [2] = sigma_y;
   element -> stress [1] -> values [3] = 0.0;		/* sigma_z */
   element -> stress [1] -> values [4] = tau_xy;
   element -> stress [1] -> values [5] = 0.0;		/* tau_xz  */
   element -> stress [1] -> values [6] = 0.0;		/* tau_yz  */

   PrincipalStresses2D(element -> stress [1] -> values);

   for (i = 1 ; i <= 3 ; i++) {
      if (element -> node [i] -> stress == NULL)
         AllocateNodalStress(element -> node [i]);

      element -> node [i] -> numelts ++;

      for (j = 1 ; j <= 10 ; j++)
         element -> node [i] -> stress [j] += element -> stress [1] -> values [j];
   }

   return 0;
} 

Matrix CSTLocalB (element, area)
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
      B = CreateMatrix (3,6);

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
   
   for (j = 0 ; j < 3 ; j++) {
      MatrixData (B) [1][2*j + 1] = beta[j+1];
      MatrixData (B) [2][2*j + 2] = gamma[j+1];
      MatrixData (B) [3][2*j + 1] = gamma[j+1];
      MatrixData (B) [3][2*j + 2] = beta[j+1];
   }

   factor = 0.5/A;
   ScaleMatrix (B,B,factor,0.0);

   if (area != NULL)
      (*area) = A;

   return B;
}

void CSTLumpedMassMatrix (element, area)
   Element	element;
   double	area;
{
   double	factor;
   unsigned	i;

   ZeroMatrix (element -> M); 

   factor = (element -> material -> t * element -> material -> rho * area)/3.0;

   for (i = 1 ; i <= 6 ; i++)
      MatrixData (element -> M) [i][i] = factor;   
   
   return;
}

Vector CSTEquivNodalForces (element, err_count)
   Element	element;
   int		*err_count;
{
   double		L;
   double		wa,wb;
   double		force1,
			force2;
   int			count;
   double		xc1,xc2,
			yc1,yc2;
   double		thick;
   unsigned		node_a,
			node_b;
   unsigned		i;
   static Vector 	equiv = NullMatrix;
 
   if (equiv == NullMatrix) 
      equiv = CreateVector (6);

   count = 0;
   force1 = force2 = 0; /* gcc -Wall */
 
   if (element -> numdistributed > 2) {
      error ("cst element %d can have at most two distributed loads",
              element -> number);
      count++;
   }

   thick = element -> material -> t;

   for (i = 1 ; i <= 6 ; i++)
      VectorData (equiv) [i] = 0.0;

   for (i = 1 ; i <= element -> numdistributed ; i++) {

      if (element -> distributed[i] -> nvalues != 2) {
         error ("load %s does not have 2 nodal values (element %d)",
                 element -> distributed[i] -> name,element -> number);
         count++;
      }

      if (element -> distributed[i] -> direction != GlobalX &&
         element -> distributed[i] -> direction != GlobalY) {
          error ("invalid direction specified for load %s (element %d)",
                 element -> distributed[i] -> name,element -> number);
          count++;
      }

      node_a = element -> distributed[i] -> value[1].node;
      node_b = element -> distributed[i] -> value[2].node;

      if (node_a < 1 || node_a > 3 || node_b < 1 || node_b > 3) {
         error ("incorrect node numbering for load %s (element %d)", 
                element -> distributed[i] -> name,element -> number);
         count++;
      }

      if (node_a == node_b) {
         error ("incorrect node numbering for load %s (element %d)", 
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

      wa = element -> distributed[i] -> value[1].magnitude;
      wb = element -> distributed[i] -> value[2].magnitude;

      if (wa == wb) 		          /* uniform distributed load */
         force1 = force2 = wa*L*thick/2.0;
      else if (fabs(wa) > fabs(wb)) {     /* load sloping node1 to node2 */
         force2 = wb*L*thick/2.0 + (wa - wb)*L*thick/6.0;
         force1 = wb*L*thick/2.0 + (wa - wb)*L*thick/3.0; 
      }
      else if (fabs(wa) < fabs(wb)) {     /* load sloping node2 to node1 */
         force2 = wa*L*thick/2.0 + (wb - wa)*L*thick/6.0;
         force1 = wa*L*thick/2.0 + (wb - wa)*L*thick/3.0; 
      } 

      if (element -> distributed[i] -> direction == GlobalX) {
         VectorData (equiv) [2*node_a - 1] += force1;
         VectorData (equiv) [2*node_b - 1] += force2;
      }
      else {
         VectorData (equiv) [2*node_a] += force1;
         VectorData (equiv) [2*node_b] += force2;
      } 
   }

	/*
	 * Now that we know all is okay, allocate some memory if we
	 * haven't already done so for some other element
	 */

   SetEquivalentForceMemory (element);

   *err_count = 0;
   return equiv; 
}
