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
 * File:	brick.c	
 *
 * Description:	contains code for a trilinear isoparametric "brick" element
 *
 *****************************************************************************/

# include <stdio.h>
# include <math.h>
# include "fe.h"
# include "error.h"
# include "misc.h"

static int brickEltSetup (Element element, char mass_mode, int tangent);
static int brickEltStress (Element element);

struct definition brickDefinition = {
   "brick",
   brickEltSetup,
   brickEltStress,
   Solid, 			/* shape				     */
   8, 				/* shape nodes				     */
   8, 				/* number of nodes			     */
   10, 				/* # of stress components at each int. point */
   3,				/* number of DOFs per node		     */
  {0, 1, 2, 3, 0, 0, 0}, 	/* DOF map				     */
   0				/* retain K flag			     */
};

static void	LocalShapeFunctions (Element element, Matrix N, Matrix dNdxi, Matrix dNde, Matrix dNdzt, int first, int nodal);
static Vector	GlobalShapeFunctions (Element element, Matrix dNdxi, Matrix dNde, Matrix dNdzt, Matrix dNdx, Matrix dNdy, Matrix dNdz);
static void     AddContribution (Matrix K, Matrix B, Matrix D, double jac);
static Matrix   LocalB (Element element, Matrix dNdx, Matrix dNdy, Matrix dNdz, unsigned int point);

	/*
	 * shape function / shape function derivative matrices - we
 	 * only need space for them once and we use them in both
	 * setup and stressess ...
	 */

static Matrix	N = NullMatrix;
static Matrix	dNde;
static Matrix	dNdxi;
static Matrix	dNdzt;
static Matrix	dNdx;
static Matrix 	dNdy;
static Matrix	dNdz;

static int
brickEltSetup(Element element, char mass_mode, int tangent)
{
   unsigned	i;
   Matrix	D;
   Matrix	B;
   Vector	jac;
   int		count;

   if (N == NullMatrix) {
      N     = CreateMatrix (8, 8);
      dNdxi = CreateMatrix (8, 8);
      dNde  = CreateMatrix (8, 8);
      dNdzt = CreateMatrix (8, 8);
      dNdx  = CreateMatrix (8, 8);
      dNdy  = CreateMatrix (8, 8);
      dNdz  = CreateMatrix (8, 8);
   }

   count = 0;

   if (element -> material -> E == 0.0) {
      error ("element %d has 0.0 for Young's modulus (E)", element -> number);
      count++;
   }

   if (element -> material -> nu == 0.0) {
      error ("element %d has 0.0 for Poisson's ratio (nu)", element -> number);
      count++;
   }

   if (count)
      return count;

   LocalShapeFunctions (element, N, dNdxi, dNde, dNdzt, element -> number == 1, 0);
   jac = GlobalShapeFunctions (element, dNdxi, dNde, dNdzt, dNdx, dNdy, dNdz);

   D = IsotropicD (element);
   if (D == NullMatrix)
      return 1;

   if (element -> K == NullMatrix)
      element -> K = CreateMatrix (24, 24);

   ZeroMatrix (element -> K);
   
   for (i = 1 ; i <= 8 ; i++) {
      B = LocalB (element, dNdx, dNdy, dNdz, i);
      AddContribution (element -> K, B, D, VectorData (jac) [i]);
   }

   return 0; 
}

static int
brickEltStress(Element element)
{
   static Vector	stress = NullMatrix,
			d;
   static Matrix	temp;
   static Vector	weights;
   static Matrix	N, dNdxi, dNde, dNdzt,
                        dNdx, dNdy, dNdz = NullMatrix;
   Matrix		D,
			B;
   Vector		jac;
   unsigned		i,j;
   double		x,y,z;

   if (dNdz == NullMatrix) {
  
      N     = CreateMatrix (8,8);
      dNdxi = CreateMatrix (8,8);
      dNde  = CreateMatrix (8,8);
      dNdzt = CreateMatrix (8,8);
      dNdx  = CreateMatrix (8,8);
      dNdy  = CreateMatrix (8,8);
      dNdz  = CreateMatrix (8,8);
      weights = CreateVector (8);
   }
   
   if (stress == NullMatrix) {
      stress = CreateVector (6);
      d = CreateVector (24);
      temp = CreateMatrix (6,24);
   }

   D = IsotropicD (element);
   if (D == NullMatrix)
      return 1;

   LocalShapeFunctions (element, N, dNdxi, dNde, dNdzt, element -> number == 1, 1);
   jac = GlobalShapeFunctions (element, dNdxi, dNde, dNdzt, dNdx, dNdy, dNdz);

   for (i = 1 ; i <= 8 ; i++) {
      VectorData (d) [3*i - 2] = element -> node[i] -> dx[1];
      VectorData (d) [3*i - 1] = element -> node[i] -> dx[2];
      VectorData (d) [3*i]     = element -> node[i] -> dx[3];
   }

   element -> ninteg = 8;
   SetupStressMemory (element);

   for (i = 1 ; i <= 8 ; i++) {
      B = LocalB (element, dNdx, dNdy, dNdz, i);
      if (B == NullMatrix)
         return 1;

      x = y = z = 0.0;
      for (j = 1 ; j <= 8 ; j++) {
         x += MatrixData (N)[j][i]*element -> node[j] -> x;
         y += MatrixData (N)[j][i]*element -> node[j] -> y;
         z += MatrixData (N)[j][i]*element -> node[j] -> z;
      }
   
      MultiplyMatrices (temp, D, B);  

      MultiplyMatrices (stress, temp, d);
    
      element -> stress [i] -> x = x;
      element -> stress [i] -> y = y; 
      element -> stress [i] -> z = z; 
      element -> stress [i] -> values [1] = VectorData (stress) [1];
      element -> stress [i] -> values [2] = VectorData (stress) [2];
      element -> stress [i] -> values [3] = VectorData (stress) [3];
      element -> stress [i] -> values [4] = VectorData (stress) [4];
      element -> stress [i] -> values [5] = VectorData (stress) [5];
      element -> stress [i] -> values [6] = VectorData (stress) [6];

      PrincipalStresses3D(element -> stress [i] -> values.c_ptr1());
   }

   for (i = 1 ; i <= 8 ; i++) {
       if (element -> node [i] -> stress.empty()) 
           AllocateNodalStress(element -> node [i]);
      
      element -> node [i] -> numelts ++;

      for (j = 1 ; j <= 10 ; j++)
         element -> node [i] -> stress [j] += element -> stress [i] -> values [j]; 
   }

   return 0;
}

static Matrix
LocalB(Element element, Matrix dNdx, Matrix dNdy, Matrix dNdz, unsigned int point)
{
   static Matrix	B = NullMatrix;
   unsigned		i;

   if (B == NullMatrix) 
      B = CreateMatrix (6, 24);

   ZeroMatrix (B);

   for (i = 1 ; i <= 8 ; i++) {
      MatrixData (B) [1][3*i - 2] = MatrixData (dNdx) [i][point];
      MatrixData (B) [2][3*i - 1] = MatrixData (dNdy) [i][point];
      MatrixData (B) [3][3*i]     = MatrixData (dNdz) [i][point];
      MatrixData (B) [4][3*i - 1] = MatrixData (dNdz) [i][point];
      MatrixData (B) [4][3*i]     = MatrixData (dNdy) [i][point];
      MatrixData (B) [5][3*i - 2] = MatrixData (dNdz) [i][point];
      MatrixData (B) [5][3*i]     = MatrixData (dNdx) [i][point];
      MatrixData (B) [6][3*i - 2] = MatrixData (dNdy) [i][point];
      MatrixData (B) [6][3*i - 1] = MatrixData (dNdx) [i][point];
   }      

   return B;
}

static void
AddContribution(Matrix K, Matrix B, Matrix D, double jac)
{
   unsigned	i, j, k;
   double	result;
   double	temp [7];

   for (j = 1 ; j <= 24 ; j++) {
      for (i = 1 ; i <= 6 ; i++) {	/* loop over columns of D 	*/

         temp [i] = 0.0;
         for (k = 1 ; k <= 6 ; k++) 	/* loop over rows of D		*/
            temp [i] += MatrixData (D) [k][i] * MatrixData (B) [k][j];
      }

      for (i = 1 ; i <= 24 ; i++) {
         result = 0.0;
         for (k = 1 ; k <= 6 ; k ++) 	/* loop over columns of BTD	*/
            result += temp [k] * MatrixData (B) [k][i];

         MatrixData (K) [j][i] += jac*result;
      }
   }

   return;
}

#define PT 0.57735026918962576451

static double xi_n [ ]  = {0, -1, 1, 1, -1, -1, 1, 1, -1};
static double e_n [ ]   = {0, -1, -1, 1, 1, -1, -1, 1, 1};
static double zt_n [ ]  = {0, -1, -1, -1, -1, 1, 1, 1, 1};

static double xi_points [ ]   = {0, -PT, PT, PT, -PT, -PT, PT, PT, -PT};
static double eta_points [ ]  = {0, -PT, -PT, PT, PT, -PT, -PT, PT, PT};
static double zeta_points [ ] = {0, -PT, -PT, -PT, -PT, PT, PT, PT, PT};

static void
LocalShapeFunctions(Element element, Matrix N, Matrix dNdxi, Matrix dNde, Matrix dNdzt, int first, int nodal)
{
   double	eta, en;
   double	xi, xn;
   double	zeta, zn;
   unsigned	i, j;
   double      *xi_p;
   double      *eta_p;
   double      *zeta_p;

   if (!first)
      return;

   if (nodal) {
      xi_p = xi_n;
      eta_p = e_n;
      zeta_p = zt_n;
   }
   else {
      xi_p = xi_points;
      eta_p = eta_points;
      zeta_p = zeta_points;
   }

   for (i = 1 ; i <= 8 ; i++) {		/* loop over integration points   */
      xi   = xi_p [i]; 
      eta  = eta_p [i]; 		/* set the int. point coordinates */
      zeta = zeta_p [i]; 

      for (j = 1 ; j <= 8 ; j++) {	/* loop over nodes		    */
         xn = xi_n [j];
         en = e_n [j];			/* set the natural nodal coordinate */
         zn = zt_n [j];

         MatrixData (N) [j][i]     = 0.125*(1 + eta*en)*(1 + xi*xn)*(1 + zeta*zn);
         MatrixData (dNdxi) [j][i] = 0.125*xn*(1 + en*eta + zn*zeta + en*eta*zn*zeta);
         MatrixData (dNde) [j][i]  = 0.125*en*(1 + xn*xi + zn*zeta + xn*xi*zn*zeta);
         MatrixData (dNdzt) [j][i] = 0.125*zn*(1 + en*eta + xi*xn + xi*xn*en*eta);
      }
   }

   return;
}

static Vector
GlobalShapeFunctions(Element element, Matrix dNdxi, Matrix dNde, Matrix dNdzt, Matrix dNdx, Matrix dNdy, Matrix dNdz)
{
   static Vector	jac = NullMatrix;
   unsigned		i, j;
   double		dxdxi, dydxi, dzdxi;
   double		dxde,dyde, dzde;
   double		dxdzt, dydzt, dzdzt;
   double		cof [4][4];

   if (jac == NullMatrix) 
      jac = CreateVector (8);

   for (i = 1 ; i <= 8 ; i++) {
      dxdxi = dydxi = dzdxi = 0;
      dxde = dyde = dzde = 0;
      dxdzt = dydzt = dzdzt = 0;

      for (j = 1 ; j <= 8 ; j++) {
         dxdxi += MatrixData (dNdxi) [j][i] * element -> node [j] -> x;
         dydxi += MatrixData (dNdxi) [j][i] * element -> node [j] -> y;
         dzdxi += MatrixData (dNdxi) [j][i] * element -> node [j] -> z;

         dxde += MatrixData (dNde) [j][i] * element -> node [j] -> x;
         dyde += MatrixData (dNde) [j][i] * element -> node [j] -> y;
         dzde += MatrixData (dNde) [j][i] * element -> node [j] -> z;

         dxdzt += MatrixData (dNdzt) [j][i] * element -> node [j] -> x;
         dydzt += MatrixData (dNdzt) [j][i] * element -> node [j] -> y;
         dzdzt += MatrixData (dNdzt) [j][i] * element -> node [j] -> z;
      }
    
      cof [1][1] = dyde*dzdzt - dydzt*dzde;
      cof [1][2] = dydzt*dzdxi - dydxi*dzdzt;
      cof [1][3] = dydxi*dzde - dyde*dzdxi;
      cof [2][1] = dzde*dxdzt - dzdzt*dxde;
      cof [2][2] = dzdzt*dxdxi - dzdxi*dxdzt;
      cof [2][3] = dzdxi*dxde - dzde*dxdxi;
      cof [3][1] = dxde*dydzt - dxdzt*dyde;
      cof [3][2] = dxdzt*dydxi - dxdxi*dydzt;
      cof [3][3] = dxdxi*dyde - dxde*dydxi;

      VectorData (jac) [i] = dxdxi*cof [1][1] + dxde*cof [1][2] + dxdzt*cof [1][3];

      for (j = 1 ; j <= 8 ; j++) {
         MatrixData (dNdx) [j][i] = (MatrixData (dNdxi) [j][i]*cof [1][1] +
                                 MatrixData (dNde) [j][i]*cof [1][2] +
                                 MatrixData (dNdzt) [j][i]*cof [1][3]) /
                                VectorData (jac) [i];
         MatrixData (dNdy) [j][i] = (MatrixData (dNdxi) [j][i]*cof [2][1] +
                                 MatrixData (dNde) [j][i]*cof [2][2] +
                                 MatrixData (dNdzt) [j][i]*cof [2][3]) /
                                VectorData (jac) [i];
         MatrixData (dNdz) [j][i] = (MatrixData (dNdxi) [j][i]*cof [3][1] +
                                 MatrixData (dNde) [j][i]*cof [3][2] +
                                 MatrixData (dNdzt) [j][i]*cof [3][3]) /
                                VectorData (jac) [i];

      }
   } 

   return jac;
}
