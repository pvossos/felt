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
 * File:	axisymm.c						*
 *									*
 * Description: This file contains the code for the axisymmetric	*
 *		triangular element					*
 *									*
 ************************************************************************/

# include <math.h>
# include <stdio.h>
# include "allocate.h"
# include "fe.h"
# include "error.h"
# include "misc.h"

int axisymmetricEltSetup ( );
int axisymmetricEltStress ( );

struct definition axisymmetricDefinition = {
    "axisymmetric", axisymmetricEltSetup, axisymmetricEltStress, 
    Planar, 3, 3, 4, 2, {0, 1, 3, 0, 0, 0, 0}, 0
};

Matrix  AxisymmetricLocalB 	     ( );
Vector	AxisymmetricEquivNodalForces ( );

int axisymmetricEltSetup (element, mass_mode, tangent)
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
   double		r_avg;
   double		z_avg;

   if (element -> material -> nu == 0) {
      error ("Axisymmetric element %d has 0.0 for Poisson's ratio (nu)", element -> number);
      return 1;
   }
   if (element -> material -> E == 0) {
      error ("Axisymmetric element %d has 0.0 for Poisson's ratio (nu)", element -> number);
      return 1;
   }

   B = AxisymmetricLocalB (element, &area, &r_avg, &z_avg);
   if (B == NullMatrix)
      return 1;
   
   D = AxisymmetricD (element);
   if (D == NullMatrix)
      return 1;

/*
   fprintf (stdout,"element %d D = \n", element -> number);
   PrintMatrix (D, stdout);
   fprintf (stdout,"element %d B = \n", element -> number);
   PrintMatrix (B, stdout);
*/

   if (element -> K == NullMatrix)
      element -> K = CreateMatrix (6,6);

   MultiplyAtBA (element -> K, B, D);
 
   factor = 2.0*M_PI*r_avg*area;
   ScaleMatrix (element -> K, element -> K, factor, 0);
/*
   fprintf (stdout,"element %g area = \n", area);
   fprintf (stdout,"element %d stiffness = \n", element -> number);
   PrintMatrix (element -> K, stdout);
*/
   if (element -> numdistributed > 0) {
      equiv = AxisymmetricEquivNodalForces (element, area, &count);
      if (equiv == NullMatrix)
         return count;

       for (i = 1; i <= 3 ; i++) {
          element -> node[i] -> eq_force[1] += VectorData (equiv) [2*i - 1];
          element -> node[i] -> eq_force[2] += VectorData (equiv) [2*i];
       }
/*
       fprintf (stdout, "Equivalent forces = \n");
       PrintMatrix(equiv, stdout);
*/
   }

	/*	
	 * form the element mass matrix if necessary (note that we only
	 * have a lumped formulation for now)
	 */

   if (mass_mode) {
      error ("mass matrices not supported for axisymmetric elements");
      return 1;
   }

   return 0;
}

int axisymmetricEltStress (element)
   Element	element;
{
   static Vector	stress = NullMatrix,
			d;
   unsigned		i;
   static Matrix	temp;
   Matrix		D, B;
   double		r_avg;
   double		z_avg;
   
   if (stress == NullMatrix) {
      stress = CreateVector (4);
      d = CreateVector (6);
      temp = CreateMatrix (4,6);
   }

   B = AxisymmetricLocalB (element, NULL, &r_avg, &z_avg);
   if (B == NullMatrix)
      return 1;

   D = AxisymmetricD (element);
   if (D == NullMatrix)
      return 1;

   for (i = 1; i <= 3 ; i++) {
      VectorData (d) [2*i - 1] = element -> node[i] -> dx[1];
      VectorData (d) [2*i] = element -> node[i] -> dx[2];
   }

   MultiplyMatrices (temp, D, B);  

   MultiplyMatrices (stress, temp, d);
  
   element -> ninteg = 1;
   SetupStressMemory (element);

   element -> stress [1] -> x = r_avg;
   element -> stress [1] -> y = z_avg;

   element -> stress [1] -> values [1] = mdata(stress,1,1);
   element -> stress [1] -> values [2] = mdata(stress,2,1);
   element -> stress [1] -> values [3] = mdata(stress,3,1);
   element -> stress [1] -> values [4] = mdata(stress,4,1);

   return 0;
} 

Matrix AxisymmetricLocalB (element, area, r_avg, z_avg)
   Element	element;
   double	*area;
   double	*r_avg;
   double	*z_avg;
{
   static Matrix 	B = NullMatrix;
   double		rc1, zc1;
   double		rc2, zc2;
   double		rc3, zc3;
   double		alpha [4];
   double		beta [4];
   double		gamma [4];
   double		A, r, z;
   double		factor;
   unsigned		j;

   if (B == NullMatrix) 
      B = CreateMatrix (4,6);

   ZeroMatrix (B);

   rc1 = element -> node[1] -> x;
   rc2 = element -> node[2] -> x;
   rc3 = element -> node[3] -> x;
   zc1 = element -> node[1] -> z;
   zc2 = element -> node[2] -> z;
   zc3 = element -> node[3] -> z;

   alpha[1] = rc2*zc3 - zc2*rc3;
   alpha[2] = rc3*zc1 - zc3*rc1;
   alpha[3] = rc1*zc2 - zc1*rc2;

   beta[1] = zc2 - zc3;
   beta[2] = zc3 - zc1;
   beta[3] = zc1 - zc2;

   gamma[1] = rc3 - rc2;
   gamma[2] = rc1 - rc3;
   gamma[3] = rc2 - rc1;

   A = 0.5*(rc1*(beta[1]) + rc2*(beta[2]) + rc3*(beta[3]));
   
   if (A < 0) {
      error("incorrect node ordering for element %d (must be ccw)",element -> number);
      return NullMatrix;
   }
   if (A == 0) {
      error ("area of element %d is zero, check node numbering",element -> number);
      return NullMatrix;
   }
  
   r = (rc1 + rc2 + rc3) / 3.0; 
   z = (zc1 + zc2 + zc3) / 3.0; 

   for (j = 0 ; j < 3 ; j++) {
      MatrixData (B) [1][2*j + 1] = beta[j+1];
      MatrixData (B) [2][2*j + 2] = gamma[j+1];
      MatrixData (B) [3][2*j + 1] = alpha[j+1]/r + beta[j+1] + gamma[j+1]*z/r;
      MatrixData (B) [4][2*j + 1] = gamma[j+1];
      MatrixData (B) [4][2*j + 2] = beta[j+1];
   }

   factor = 0.5/A;
   ScaleMatrix (B,B,factor,0.0);

   if (area != NULL)
      *area = A;

   if (r_avg != NULL)
      *r_avg = r;

   if (z_avg != NULL)
      *z_avg = z;

   return B;
}

Vector AxisymmetricEquivNodalForces (element, area, err_count)
   Element	element;
   double	area;
   int		*err_count;
{
   double		Mr, Mp;
   double		p1, p2;
   double		force1,
			force2;
   int			count;
   double		r1,r2,r3,
			z1,z2,z3;
   double		alpha [4];
   double		beta [4];
   double		gamma [4];
   double		a;
   double		b;
   double		g;
   double		diff;
   unsigned		node_a,
			node_b;
   unsigned		i;
   static Vector 	equiv = NullMatrix;
 
   if (equiv == NullMatrix) 
      equiv = CreateVector (6);

   count = 0;
   force1 = force2 = 0; /* gcc -Wall */
 
   if (element -> numdistributed > 2) {
      error ("axisymmetric element %d can have at most two distributed loads",
              element -> number);
      count++;
   }

   for (i = 1 ; i <= 6 ; i++)
      VectorData (equiv) [i] = 0.0;

   r1 = element -> node[1] -> x;
   r2 = element -> node[2] -> x;
   r3 = element -> node[3] -> x;
   z1 = element -> node[1] -> z;
   z2 = element -> node[2] -> z;
   z3 = element -> node[3] -> z;

   alpha[1] = r2*z3 - r3*z2;
   alpha[2] = r3*z1 - z3*r1;
   alpha[3] = r1*z2 - z1*r2;

   beta[1] = z2 - z3;
   beta[2] = z3 - z1;
   beta[3] = z1 - z2;

   gamma[1] = r3 - r2;
   gamma[2] = r1 - r3;
   gamma[3] = r2 - r1;

   for (i = 1 ; i <= element -> numdistributed ; i++) {

      if (element -> distributed[i] -> nvalues != 2) {
         error ("load %s does not have 2 nodal values (element %d)",
                 element -> distributed[i] -> name, element -> number);
         count++;
      }

      if (element -> distributed[i] -> direction != Axial &&
         element -> distributed[i] -> direction != Radial) {
          error ("invalid direction specified for load %s (element %d)",
                 element -> distributed[i] -> name, element -> number);
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

      r1 = element -> node[node_a] -> x;
      r2 = element -> node[node_b] -> x;
      z1 = element -> node[node_a] -> z;
      z2 = element -> node[node_b] -> z;

      p1 = element -> distributed[i] -> value[1].magnitude;
      p2 = element -> distributed[i] -> value[2].magnitude;

      Mr = (r2 - r1) / (z2 - z1);
      Mp = (p2 - p1) / (z2 - z1);
     
      diff = z1 - z2;

      a = alpha [node_a];
      b = beta [node_a];
      g = gamma [node_a];

      force1 =  -0.5*p1*r1*diff*(g*z1 + 2*b*r1 + 2*a + g*z2)
               + p1*diff*diff*(g*z1 + 3*a + 6*b*r1 + 2*g*z2)*Mr/6.0
               + r1*diff*diff*(g*z1 + 3*b*r1 + 3*a + 2*g*z2)*Mp/6.0
               - diff*diff*diff*(g*z1 + 8*b*r1 + 4*a + 3*g*z2)*Mp*Mr/12.0
               + b*diff*diff*diff*diff*Mr*Mr*Mp/4.0
               - b*p1*diff*diff*diff*Mr*Mr/3.0; 

      a = alpha [node_b];
      b = beta [node_b];
      g = gamma [node_b];

      force2 =  -0.5*p1*r1*diff*(g*z1 + 2*b*r1 + 2*a + g*z2)
               + p1*diff*diff*(g*z1 + 3*a + 6*b*r1 + 2*g*z2)*Mr/6.0
               + r1*diff*diff*(g*z1 + 3*b*r1 + 3*a + 2*g*z2)*Mp/6.0
               - diff*diff*diff*(g*z1 + 8*b*r1 + 4*a + 3*g*z2)*Mp*Mr/12.0
               + b*diff*diff*diff*diff*Mr*Mr*Mp/4.0
               - b*p1*diff*diff*diff*Mr*Mr/3.0; 

      if (element -> distributed[i] -> direction == Radial) {
         VectorData (equiv) [2*node_a - 1] += M_PI/area*force1;
         VectorData (equiv) [2*node_b - 1] += M_PI/area*force2;
      }
      else /* Axial */ {
         VectorData (equiv) [2*node_a] += M_PI/area*force1;
         VectorData (equiv) [2*node_b] += M_PI/area*force2;
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
