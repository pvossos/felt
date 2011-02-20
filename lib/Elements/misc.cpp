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

/*****************************************************************************
 *
 * File:	misc.c
 * 
 * Description:	contains various commonly used routines in formulating
 *		element stiffness matrices
 *
 ******************************************************************************/

# include <math.h>
# include "fe.h"
# include "misc.h"
# include "error.h"

unsigned
GaussPoints(unsigned int npoints, double **xpoints, double **weights)
{
   static double  x[3][3] = {{0.0,0.0,0.0},
                             {-0.57735026918962,0.57735026918962,0.0},
                             {-0.77459666924148,0.0,0.77459666924148}};
   static double  w[3][3] = {{1.0,0.0,0.0},
                             {1.0,1.0,0.0},
                             {0.5555555555555,0.8888888888888,0.5555555555555}};

   if (npoints > 3 || npoints < 1)
      return 1;
   else {
      if (xpoints != NULL)
         *xpoints = x[npoints - 1];
      if (weights != NULL)
         *weights = w[npoints - 1];
      
      return 0;
   }
}

Matrix 
PlaneStrainD(Element element)
{
   static Matrix	D = NullMatrix;
   static double	prev_nu = -99;
   static double	prev_E = -99;
   double 		poisson,
			factor;

   if (D == NullMatrix) 
      D = CreateMatrix (3,3);

   if (element -> material -> E != prev_E ||
       element -> material -> nu != prev_nu) {

      ZeroMatrix (D);

      poisson = element -> material -> nu;

      MatrixData (D) [1][1] = 1 - poisson;
      MatrixData (D) [1][2] = poisson; 
      MatrixData (D) [2][1] = poisson;
      MatrixData (D) [2][2] = 1 - poisson;
      MatrixData (D) [3][3] = (1 - 2*poisson)/2;

      if (1 - 2*poisson <= TINY) {
         error ("singularity in constitutive matrix for element %d",element -> number);
         return NULL;
      }

      factor = element -> material -> E / ((1 + poisson)*(1 - 2*poisson));
      ScaleMatrix (D,D,factor,0.0);

      prev_nu = element -> material -> nu;
      prev_E = element -> material -> E;
   }

   return D;
}

Matrix
PlaneStressD(Element element)
{
   static Matrix	D = NullMatrix;
   static double	prev_nu = -99;
   static double	prev_E = -99;
   double 		poisson,
			factor;

   if (D == NullMatrix) 
      D = CreateMatrix (3,3);

   if (element -> material -> nu != prev_nu || 
       element -> material -> E != prev_E) {

      ZeroMatrix (D);

      poisson = element -> material -> nu;

      MatrixData (D) [1][1] = 1;
      MatrixData (D) [1][2] = poisson; 
      MatrixData (D) [2][1] = poisson;
      MatrixData (D) [2][2] = 1;
      MatrixData (D) [3][3] = (1 - poisson)/2;

      if (1 - poisson*poisson <= TINY) {
         error ("singularity in constitutive matrix for element %d",element -> number);
         return NullMatrix;
      }

      factor = element -> material -> E / (1 - poisson*poisson);
      ScaleMatrix (D,D,factor,0.0);

      prev_E = element -> material -> E;
      prev_nu = element -> material -> nu;
   }

   return D;
}

Matrix
AxisymmetricD(Element element)
{
   static Matrix	D = NullMatrix;
   static double	prev_nu = -99;
   static double	prev_E = -99;
   double 		poisson,
			factor;

   if (D == NullMatrix) 
      D = CreateMatrix (4,4);

   if (element -> material -> nu != prev_nu || 
       element -> material -> E != prev_E) {

      ZeroMatrix (D);

      poisson = element -> material -> nu;

      MatrixData (D) [1][1] = 1 - poisson;
      MatrixData (D) [1][2] = poisson; 
      MatrixData (D) [1][3] = poisson; 

      MatrixData (D) [2][1] = poisson;
      MatrixData (D) [2][2] = 1 - poisson;
      MatrixData (D) [2][3] = poisson;

      MatrixData (D) [3][1] = poisson; 
      MatrixData (D) [3][2] = poisson; 
      MatrixData (D) [3][3] = 1 - poisson;

      MatrixData (D) [4][4] = 0.5*(1 - 2*poisson);

      if (1 - 2*poisson <= TINY) {
         error ("singularity in constitutive matrix for element %d",element -> number);
         return NullMatrix;
      }

      factor = element -> material -> E / (1 - 2*poisson) / (1 + poisson);
      ScaleMatrix (D,D,factor,0.0);

      prev_E = element -> material -> E;
      prev_nu = element -> material -> nu;
   }

   return D;
}

Matrix
IsotropicD(Element element)
{
   static Matrix	D = NullMatrix;
   static double	prev_nu = -99;
   static double	prev_E = -99;
   double 		poisson,
			factor;

   if (D == NullMatrix) 
      D = CreateMatrix (6, 6);

   if (element -> material -> nu != prev_nu ||
       element -> material -> E != prev_E) {

      ZeroMatrix (D);

      poisson = element -> material -> nu;

      MatrixData (D) [1][1] = 1.0 - poisson;
      MatrixData (D) [1][2] = poisson; 
      MatrixData (D) [1][3] = poisson; 

      MatrixData (D) [2][1] = poisson;
      MatrixData (D) [2][2] = 1.0 - poisson;
      MatrixData (D) [2][3] = poisson;

      MatrixData (D) [3][1] = poisson;
      MatrixData (D) [3][2] = poisson;
      MatrixData (D) [3][3] = 1.0 - poisson;

      MatrixData (D) [4][4] = (1.0 - 2*poisson)/2.0;
      MatrixData (D) [5][5] = (1.0 - 2*poisson)/2.0;
      MatrixData (D) [6][6] = (1.0 - 2*poisson)/2.0;

      if (1.0 - 2.0*poisson <= TINY) {
         error ("singularity in constitutive matrix for element %d",element -> number);
         return NullMatrix;
      }

      factor = element -> material -> E / (1.0 + poisson) / (1.0 - 2*poisson);
      ScaleMatrix (D, D, factor, 0.0);

      prev_E = element -> material -> E;
      prev_nu = element -> material -> nu;
   }

   return D;
}

double
ElementLength(Element element, unsigned int coords)
{
   if (coords == 1)
      return fabs (element -> node[2] -> x - element -> node[1] -> x);
   else if (coords == 2) 
      return sqrt ((element -> node[2] -> x - element -> node[1] -> x)*
                   (element -> node[2] -> x - element -> node[1] -> x) +
                   (element -> node[2] -> y - element -> node[1] -> y)* 
                   (element -> node[2] -> y - element -> node[1] -> y));
   else if (coords == 3)
      return sqrt ((element -> node[2] -> x - element -> node[1] -> x)*
                   (element -> node[2] -> x - element -> node[1] -> x) +
                   (element -> node[2] -> y - element -> node[1] -> y)* 
                   (element -> node[2] -> y - element -> node[1] -> y) +
                   (element -> node[2] -> z - element -> node[1] -> z)* 
                   (element -> node[2] -> z - element -> node[1] -> z));
   else
      return 0.0;
}

double
ElementArea(Element e, unsigned int n)
{
   unsigned	i;
   double	sum;

   sum = e -> node[1] -> x*(e -> node[2] -> y - e -> node[n] -> y) +
         e -> node[n] -> x*(e -> node[1] -> y - e -> node[n-1] -> y);

   for (i = 2 ; i <= n-1 ; i++)
      sum += e -> node[i] -> x*(e -> node[i+1] -> y - e -> node[i-1] -> y);

   return sum/2;
}

void
ResolveHingeConditions(Element element)
{
   unsigned	nodes, ndofs;
   unsigned	i,j;
   unsigned	m,n;
   unsigned	dof;

   nodes = element -> definition -> numnodes;
   ndofs = element -> definition -> numdofs;

   for (i = 1 ; i <= nodes ; i++) {
      for (j = 1 ; j <= ndofs ; j++) {
         if (element -> node[i] -> constraint -> constraint 
               [element -> definition -> dofs[j]] == 'h') {
     
            dof = (i-1)*ndofs + j;
            for (m = 1 ; m <= nodes*ndofs ; m++) {
               for (n = 1 ; n <= nodes*ndofs ; n++) {

                  if (m != dof && n != dof) {
                     MatrixData (element -> K) [m][n] -= 
                        MatrixData (element -> K) [dof][n] /
                        MatrixData (element -> K) [dof][dof] *
                        MatrixData (element -> K) [m][dof];
                  }
               }
            }
            element -> K = ZeroRowCol (element -> K, dof);
         }
      }
   }
   return;
}

void
SetEquivalentForceMemory(Element element)
{
    unsigned	i,j;

	/*
	 * loop over all this element's nodes and allocate space for
	 * the eq_force array if that has not already been done (i.e., it
	 * could have gotten done from some other element
	 */

    for (i = 1 ; i <= element -> definition -> numnodes ; i++) {
        if (element->node[i]->eq_force.empty()) {
            element->node[i]->eq_force.resize(6, 0);
        }
    } 

    return;
}

void
MultiplyAtBA(Matrix C, Matrix A, Matrix B)
{
    double	temp [100];
    double	result;
    unsigned	i,j,k;

    for (j = 1 ; j <= MatrixCols (A) ; j++) {

       for (i = 1 ; i <= MatrixCols (B) ; i++) {
          temp [i] = 0;
          for (k = 1 ; k <= MatrixRows (B) ; k++) 
             temp [i] += MatrixData (B) [k][i] * MatrixData (A) [k][j];
       }

       for (i = 1 ; i <= MatrixCols (A) ; i++) {
          result = 0;
          for (k = 1 ; k <= MatrixCols (B) ; k++) 
             result += temp [k] * MatrixData (A) [k][i];

          MatrixData (C) [j][i] = result;
       }                  
    }
}

Matrix
ZeroRowCol(Matrix K, unsigned int dof)
{
   unsigned	i,
		size;

   size = MatrixRows (K);

   for (i = 1 ; i <= size ; i++) {
      MatrixData (K) [i][dof] = 0;
      MatrixData (K) [dof][i] = 0; 
   }

   MatrixData (K) [dof][dof] = 1;

   return K;
} 

void
AllocationError(Element e, char *msg)
{
   Fatal ("allocation error computing element %d %s\n", e -> number, msg);
}
