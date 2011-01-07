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
* File:		iso_2d.c
*
* Description:	contains the element definition routines for isoparametric
*		plane stress / plane strain elements
*
******************************************************************************/

# include <stdio.h>
# include <math.h>
# include "fe.h"
# include "error.h"
# include "misc.h"

# define PLANESTRESS 1
# define PLANESTRAIN 2


static int iso2d_PlaneStressEltSetup (Element element, char mass_mode, int tangent);
static int iso2d_PlaneStressEltStress (Element element);
static int iso2d_PlaneStrainEltSetup (Element element, char mass_mode, int tangent);
static int iso2d_PlaneStrainEltStress (Element element);

struct definition iso2d_PlaneStressDefinition = {
   "iso2d_PlaneStress", 
   iso2d_PlaneStressEltSetup, iso2d_PlaneStressEltStress, 
   Planar, 9, 4, 6, 2, {0, 1, 2, 0, 0, 0, 0}, 0
};

struct definition iso2d_PlaneStrainDefinition = {
   "iso2d_PlaneStrain", 
   iso2d_PlaneStrainEltSetup, iso2d_PlaneStrainEltStress, 
   Planar, 9, 4, 6, 2, {0, 1, 2, 0, 0, 0, 0}, 0
};

static unsigned LocalIsoShapeFunctions   (Element element, Matrix N, Matrix dNdx, Matrix dNde, Vector weights);
static Vector   GlobalIsoShapeFunctions  (Element element, Matrix N, Matrix dNdxi, Matrix dNde, Matrix dNdx, Matrix dNdy, int ninteg, unsigned int nodes);
static Matrix   Iso2dLocalB (Element element, unsigned int numnodes, Matrix dNdx, Matrix dNdy, unsigned int point);
static int      iso2dElementSetup (Element element, char mass_mode, int tangent, unsigned int type);
static int  	 iso2dElementStress    (Element element, unsigned int type);

static int
iso2d_PlaneStressEltSetup(Element element, char mass_mode, int tangent)
{
   return iso2dElementSetup (element, mass_mode, tangent, PLANESTRESS);
}

static int
iso2d_PlaneStrainEltSetup(Element element, char mass_mode, int tangent)
{
   return iso2dElementSetup (element, mass_mode, tangent, PLANESTRAIN);
}

static int
iso2d_PlaneStressEltStress(Element element)
{
   return iso2dElementStress (element, PLANESTRESS);
}

static int
iso2d_PlaneStrainEltStress(Element element)
{
   return iso2dElementStress (element,  PLANESTRAIN);
}

static int
iso2dElementSetup(Element element, char mass_mode, int tangent, unsigned int type)
{
   unsigned		numnodes;
   unsigned		i,j;
   int			ninteg;
   Matrix		B;
   Matrix		D;
   Vector		jac;
   static Vector	weights;
   static Matrix	tempK;
   static Matrix	N, dNdxi, dNde,
                        dNdx, dNdy = NullMatrix;
   static Matrix	Bt, temp;

   if (dNdy == NullMatrix) {
  
      N     = CreateMatrix (9,9);
      dNdxi = CreateMatrix (9,9);
      dNde  = CreateMatrix (9,9);
      dNdx  = CreateMatrix (9,9);
      dNdy  = CreateMatrix (9,9);
      weights = CreateVector (9);
      tempK = CreateMatrix (18,18);
      Bt    = CreateMatrix (18,3);
      temp  = CreateMatrix (18,3);
   }

   if (element -> material -> E == 0) {
      error ("isoparametric element %d has 0.0 for Young's modulus (E)",element -> number);
      return 1;
   }
   if (element -> material -> nu == 0) {
      error ("isoparametric element %d has 0.0 for Poisson's ratio (nu)",element -> number);
      return 1;
   }
   if (element -> material -> t == 0) {
      error ("isoparametric element %d has 0.0 for thickness (t)",element -> number);
      return 1;
   }
  
   numnodes = LocalIsoShapeFunctions (element, N, dNdxi, dNde, weights);  

   if (element -> node[3] -> number == element -> node[4] -> number) {
      if (numnodes != 4) {
         error ("triangular isoparametric elt %d has more than 3 unique nodes",
                element -> number);
         return 1;
      }
      else 
         numnodes = 3;
   }

   if (numnodes > 4) {
      ninteg = 9;
      numnodes = 9;
   } else
      ninteg = 4; 

   ninteg = 4;

   jac = GlobalIsoShapeFunctions (element,N,dNdxi,dNde,dNdx,dNdy,
                                  ninteg,numnodes);

   if (type == PLANESTRESS)
      D = PlaneStressD (element);
   else if (type == PLANESTRAIN)
      D = PlaneStrainD (element);
   else
      D = NullMatrix; /* gcc -Wall */

   if (D == NullMatrix)
      return 1;

   for (i = 1 ; i <= ninteg ; i++) {
      if (VectorData (jac)[i] <= 0.0) {
         error ("det |J| for elt %d is <= 0, check elt distortion",element -> number);
         return 1;
      }
   } 

   if (element -> K == NullMatrix) {
      if (numnodes == 3) {
         element -> K = CreateMatrix (8,8);
         MatrixRows (element -> K) = 6;
         MatrixCols (element -> K) = 6;
      }
      else
         element -> K = CreateMatrix (2*numnodes, 2*numnodes);
   }

   ZeroMatrix (element -> K);

	/*
	 * We need to fool the matrix routines since really we can have
	 * a variable size stiffness array ... there's no need to operate
	 * on all those zeros if we don't have to
	 */

   MatrixRows (tempK) = 2*numnodes;
   MatrixCols (tempK) = 2*numnodes;

   for (i = 1 ; i <= ninteg ; i++) {
      B = Iso2dLocalB (element, numnodes, dNdx, dNdy, i);
      if (B == NullMatrix)
         return 1;

      MatrixRows (Bt) = MatrixRows (temp) = MatrixCols (B);

      TransposeMatrix (Bt, B);

      MultiplyMatrices (temp, Bt, D);
      MultiplyMatrices (tempK, temp, B);
      ScaleMatrix (tempK, tempK, 
                   VectorData (weights) [i]*VectorData (jac) [i], 0.0);
      AddMatrices (element -> K, element -> K, tempK);
   }
   ScaleMatrix (element -> K, element -> K, element -> material -> t, 0.0);

	/*
	 * clean out the 7 & 8th rows and columns if this was a triangular
	 * element so nothing extra gets assembled into the global
	 * stiffness routines
	 */

   if (numnodes == 3) {
      for (i = 1 ; i <= 8 ; i++) 
         for (j = 7 ; j <= 8 ; j++) 
            MatrixData (element -> K) [i][j] = 0.0;

      for (i = 7 ; i <= 8 ; i++) 
         for (j = 1 ; j <= 6 ; j++) 
            MatrixData (element -> K) [i][j] = 0.0;
   }

   return 0;
} 

static int
iso2dElementStress(Element element, unsigned int type)
{
   element -> ninteg = 0;

   return 0;
} 

static Matrix
Iso2dLocalB(Element element, unsigned int numnodes, Matrix dNdx, Matrix dNdy, unsigned int point)
{
   unsigned		i;
   static Matrix	B = NullMatrix;

   if (B == NullMatrix) 
      B = CreateMatrix (3,18);

   for (i = 1 ; i <= numnodes ; i++) {
      MatrixData (B) [1][2*i - 1] = MatrixData (dNdx) [i][point];
      MatrixData (B) [1][2*i]     = 0.0;
      MatrixData (B) [2][2*i - 1] = 0.0;
      MatrixData (B) [2][2*i]     = MatrixData (dNdy) [i][point];
      MatrixData (B) [3][2*i - 1] = MatrixData (dNdy) [i][point];
      MatrixData (B) [3][2*i]     = MatrixData (dNdx) [i][point];
   }

   MatrixCols (B) = numnodes*2;

   return B;
}
    
static Vector
GlobalIsoShapeFunctions(Element element, Matrix N, Matrix dNdxi, Matrix dNde, Matrix dNdx, Matrix dNdy, int ninteg, unsigned int nodes)
{
   unsigned		i,j;
   static Vector	jac,
			dxdxi, dxde,
			dydxi, dyde = NullMatrix;

   if (dyde == NullMatrix) {

      dxdxi = CreateVector (9);
      dxde  = CreateVector (9);
      dydxi = CreateVector (9);
      dyde  = CreateVector (9);
      jac = CreateVector (9);
   }
 
   for (i = 1 ; i <= 9 ; i++) {
      VectorData (dxdxi) [i] = 0.0; 
      VectorData (dxde) [i] = 0.0; 
      VectorData (dydxi) [i] = 0.0; 
      VectorData (dyde) [i] = 0.0; 
      VectorData (jac) [i] = 0.0;
   }

   for (i = 1 ; i <= ninteg ; i++) {
      for (j = 1 ; j <= nodes ; j++) {

         if (element -> node[j] != NULL) {
            VectorData (dxdxi) [i] += MatrixData (dNdxi) [j][i]*
                                      (element -> node[j] -> x);

            VectorData (dxde) [i] += MatrixData (dNde) [j][i]*
                                      (element -> node[j] -> x);
   
            VectorData (dydxi) [i] += MatrixData (dNdxi) [j][i]*
                                      (element -> node[j] -> y);
     
            VectorData (dyde) [i] += MatrixData (dNde) [j][i]*
                                      (element -> node[j] -> y);
         }
      }

      VectorData (jac) [i] = VectorData (dxdxi)[i] * VectorData (dyde)[i] -
                         VectorData (dxde)[i] * VectorData (dydxi)[i];

      for (j = 1 ; j <= nodes ; j++) {
         if (element -> node[j] != NULL) {
            MatrixData (dNdx) [j][i] = (MatrixData (dNdxi) [j][i]*VectorData (dyde)[i] -
                                  MatrixData (dNde)[j][i]*VectorData (dydxi)[i])/
                                  VectorData (jac)[i];
            MatrixData (dNdy) [j][i] = -(MatrixData (dNdxi)[j][i]*VectorData (dxde)[i] -
                                   MatrixData (dNde)[j][i]*VectorData (dxdxi)[i])/
                                   VectorData (jac)[i]; 
         } 
         else {
            MatrixData (dNdx) [j][i] = 0.0;
            MatrixData (dNdy) [j][i] = 0.0;
         }
      }
   }

   return jac;
}

/*****************************************************************************
*
* Function:	LocalIsoShapeFunctions
*
* Description:  calculates the shape functions and the derivatives (w/ respect
*		to xi, eta coordinates) of the shape function for a four to 
*		nine node plane stress / plane strain element	
*
* Note:		The approach looks rather brutish, but it seems much clearer
*		to me this way and we have to do each individual computation 
*		anyways, whether we put ourselves in a loop and use index
*		notation or not ...
*
******************************************************************************/

static unsigned
LocalIsoShapeFunctions(Element element, Matrix N, Matrix dNdx, Matrix dNde, Vector weights)
{
   unsigned		i,j,k;
   int			ninteg;
   double		eta,xi;
   double		Nt[10];
   double		de[10],dx[10];
   double		*gauss_points;
   double		*gauss_wts;
   static unsigned	numnodes;
   static int 	        points [10];
   static int	        prev_points [10] = {-1,-1,-1,-1,-1,-1,-1,-1,-1,-1};
   unsigned		same_flag;

   same_flag = 1;
   numnodes = 4;
   for (i = 5 ; i <= 9 ; i++) {
      points [i] = 0;

      if (element -> node[i] != NULL) {
         numnodes ++;
         points [i] = 1;
      }

      if (points [i] != prev_points [i])
         same_flag = 0;
   }
   
   if (same_flag)
      return numnodes;

   if (numnodes == 4)
      ninteg = 2;
   else
      ninteg = 3;

   for (i = 5 ; i <= 9 ; i++)
      prev_points [i] = points [i];
   
   ninteg = 2;
   GaussPoints (ninteg, &gauss_points, &gauss_wts);

   for (i = 0 ; i < ninteg ; i++) {
      xi  = gauss_points [i];
      for (j = 0 ; j < ninteg ; j++) {

         eta = gauss_points [j];

         Nt [1] = 0.25*(1 - eta)*(1 - xi);
         dx [1] = 0.25*(-1 + eta);
         de [1] = 0.25*(-1 + xi);
         Nt [2] = 0.25*(1 - eta)*(1 + xi);
         dx [2] = 0.25*(1 - eta);
         de [2] = 0.25*(-1 - xi);
         Nt [3] = 0.25*(1 + eta)*(1 + xi);
         dx [3] = 0.25*(1 + eta);
         de [3] = 0.25*(1 + xi);
         Nt [4] = 0.25*(1 + eta)*(1 - xi);
         dx [4] = 0.25*(-1 - eta);
         de [4] = 0.25*(1 - xi);

         if (numnodes == 4 && 
             element -> node[3] -> number == element -> node[4] -> number) {
 
             Nt [3] += Nt [4];
             dx [3] += dx [4];
             de [3] += de [4];
/*
             printf ("%g %g %g %g %g\n", eta, xi, Nt [3], dx [3], de [3]);
*/
         }

         if (numnodes > 4) {

            if (points [9]) {
               Nt [9] = (1 - eta*eta)*(1 - xi*xi);
               dx [9] = 2*xi*(eta*eta - 1);
               de [9] = 2*eta*(xi*xi - 1);
               for (k = 1 ; k <= 4 ; k++) {
                  Nt [k] -= 0.25*Nt [9];
                  dx [k] -= 0.25*dx [9];
                  de [k] -= 0.25*de [9];
               }
            }
            else
               Nt [9] = dx [9] = de [9] = 0.0;

            if (points [5]) {
               Nt [5] = 0.5*((1 - eta)*(1 - xi*xi) - Nt[9]);
               dx [5] = -xi + xi*eta - 0.5*dx[9];
               de [5] = 0.5*(-1 + xi*xi - de[9]);
            }
            else
               Nt [5] = dx [5] = de [5] = 0.0;
            
            if (points [6]) {
               Nt [6] = 0.5*((1 - eta*eta)*(1 + xi) - Nt[9]);
               dx [6] = 0.5*(1 - eta*eta - dx[9]);
               de [6] = -eta - eta*xi - 0.5*de[9];
            }
            else
               Nt [6] = dx [6] = de [6] = 0.0;
            
            if (points [7]) {
               Nt [7] = 0.5*((1 + eta)*(1 - xi*xi) - Nt[9]);
               dx [7] = -xi - eta*xi - 0.5*dx[9];
               de [7] = 0.5*(1 - xi*xi - de[9]);
            }
            else
               Nt [7] = dx [7] = de [7] = 0.0;
            
            if (points [8]) {
               Nt [8] = 0.5*((1 - eta*eta)*(1 - xi) - Nt[9]);
               dx [8] = 0.5*(-1 + eta*eta - dx[9]);
               de [8] = -eta + xi*eta - 0.5*de[9];
            }
            else
               Nt [8] = dx [8] = de [8] = 0.0;
            
            Nt [1] -= 0.5*(Nt[5] + Nt[8]);
            dx [1] -= 0.5*(dx[5] + dx[8]);
            de [1] -= 0.5*(de[5] + de[8]);
            Nt [2] -= 0.5*(Nt[5] + Nt[6]);
            dx [2] -= 0.5*(dx[5] + dx[6]);
            de [2] -= 0.5*(de[5] + de[6]);
            Nt [3] -= 0.5*(Nt[6] + Nt[7]);
            dx [3] -= 0.5*(dx[6] + dx[7]);
            de [3] -= 0.5*(de[6] + de[7]);
            Nt [4] -= 0.5*(Nt[7] + Nt[8]);
            dx [4] -= 0.5*(dx[7] + dx[8]);
            de [4] -= 0.5*(de[7] + de[8]);
         }

         for (k = 1 ; k <= 9 ; k++) {
            MatrixData (N) [k][i*ninteg + j+1] = Nt [k];
            MatrixData (dNdx) [k][i*ninteg + j+1] = dx [k];
            MatrixData (dNde) [k][i*ninteg + j+1] = de [k];
         }

         VectorData (weights) [i*ninteg + j+1] = gauss_wts [j];
      }
   }

   return numnodes;
}
