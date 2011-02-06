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
* File:		iso_quad.c
*
* Description:	contains the element definition routines for isoparametric
*		plane stress / plane strain elements with only four nodes
*		(faster, simpler, etc.)
*
******************************************************************************/

# include <stdio.h>
# include <math.h>
# include "fe.h"
# include "error.h"
# include "misc.h"

# define PLANESTRESS 1
# define PLANESTRAIN 2

# define FORCE	 1
# define NOFORCE 0

static void     QuadLumpedMassMatrix (Element element, unsigned int numnodes);
static unsigned LocalQuadShapeFunctions   (Element element, unsigned int ninteg, Matrix N, Matrix dNdx, Matrix dNde, Vector weights, unsigned int force_init);
static Vector   GlobalQuadShapeFunctions  (Element element, Matrix dNdxi, Matrix dNde, Matrix dNdx, Matrix dNdy, int ninteg, unsigned int nodes);
static Matrix   IsoQuadLocalB (Element element, unsigned int numnodes, Matrix dNdx, Matrix dNdy, unsigned int point);
static Vector	 IsoQuadEquivNodalForces (Element element, int *err_count);
static int      QuadElementSetup (Element element, char mass_mode, int tangent, unsigned int type);
static int  	 QuadElementStress    (Element element, unsigned int type);

static int quad_PlaneStrainEltSetup (Element element, char mass_mode, int tangent), quad_PlaneStrainEltStress (Element element);
static int quad_PlaneStressEltSetup (Element element, char mass_mode, int tangent), quad_PlaneStressEltStress (Element element);

struct definition quad_PlaneStrainDefinition = {
   "quad_PlaneStrain", 
   quad_PlaneStrainEltSetup, quad_PlaneStrainEltStress, 
   Planar, 4, 4, 10, 2, {0, 1, 2, 0, 0, 0, 0}, 0
};

struct definition quad_PlaneStressDefinition = {
   "quad_PlaneStress", 
   quad_PlaneStressEltSetup, quad_PlaneStressEltStress, 
   Planar, 4, 4, 10, 2, {0, 1, 2, 0, 0, 0, 0}, 0
};

static int
quad_PlaneStrainEltSetup(Element element, char mass_mode, int tangent)
{
   return QuadElementSetup (element, mass_mode, tangent, PLANESTRAIN);
}

static int
quad_PlaneStrainEltStress(Element element)
{
   return QuadElementStress (element, PLANESTRAIN);
}

static int
quad_PlaneStressEltStress(Element element)
{
   return QuadElementStress (element, PLANESTRESS);
}

static int
quad_PlaneStressEltSetup(Element element, char mass_mode, int tangent)
{
   return QuadElementSetup (element, mass_mode, tangent, PLANESTRESS);
}

static int
QuadElementSetup(Element element, char mass_mode, int tangent, unsigned int type)
{
   unsigned		numnodes;
   int			ninteg;
   Matrix		B;
   Matrix		D;
   Vector		jac;
   Vector		equiv;
   int			count;
   static Vector	weights;
   static Matrix	tempK;
   static Matrix	N, dNdxi, dNde,
                        dNdx, dNdy = NullMatrix;
   static Matrix	Bt, temp;

   if (dNdy == NullMatrix) {
  
      N     = CreateMatrix (4,4);
      dNdxi = CreateMatrix (4,4);
      dNde  = CreateMatrix (4,4);
      dNdx  = CreateMatrix (4,4);
      dNdy  = CreateMatrix (4,4);
      weights = CreateVector (4);
      tempK = CreateMatrix (8,8);
      Bt    = CreateMatrix (8,3);
      temp  = CreateMatrix (8,3);
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

   ninteg = 4;	/* 2 x 2 quadrature */
  
   numnodes = LocalQuadShapeFunctions (element, ninteg, N, 
                                       dNdxi, dNde, weights, NOFORCE);  

   jac = GlobalQuadShapeFunctions (element,dNdxi,dNde,dNdx,dNdy,
                                   ninteg,numnodes);

   if (type == PLANESTRESS)
      D = PlaneStressD (element);
   else if (type == PLANESTRAIN)
      D = PlaneStrainD (element);
   else
      D = NullMatrix; /* gcc -Wall */

   if (D == NullMatrix)
      return 1;
   
   for (int i = 1 ; i <= ninteg ; i++) {
      if (VectorData (jac) [i] <= 0.0) {
         error ("det |J| for elt %d is <= 0, check elt distortion",element -> number);
         return 1;
      }
   } 

   if (element -> K == NullMatrix) {
      element -> K = CreateMatrix (8,8);

      if (numnodes == 3) {
         MatrixRows (element -> K) = 6;
         MatrixCols (element -> K) = 6;
      }
   }

   ZeroMatrix (element -> K);

	/*
	 * set-up so that multiplications work right
	 */

   MatrixRows (tempK) = 2*numnodes;
   MatrixCols (tempK) = 2*numnodes;

   for (int i = 1 ; i <= ninteg ; i++) {
      B = IsoQuadLocalB (element, numnodes, dNdx, dNdy, i);
      if (B == NullMatrix)
         return 1;

      MatrixRows (Bt) = MatrixRows (temp) = MatrixCols (B);
      TransposeMatrix (Bt, B);

      MultiplyMatrices (temp, Bt, D);
      MultiplyMatrices (tempK, temp, B);

      ScaleMatrix (tempK, tempK, VectorData (weights) [i]*VectorData (jac) [i], 0.0);
      AddMatrices (element -> K, element -> K, tempK);
   }
   ScaleMatrix (element -> K, element -> K, element -> material -> t, 0.0);

	/*
	 * clean out the 7 & 8th rows and columns if this was a triangular
	 * element so nothing extra gets assembled into the global
	 * stiffness routines
	 */

   if (numnodes == 3) {
      for (size_t i = 1 ; i <= 8 ; i++) 
         for (size_t j = 7 ; j <= 8 ; j++) 
            MatrixData (element -> K) [i][j] = 0.0;

      for (size_t i = 7 ; i <= 8 ; i++) 
         for (size_t j = 1 ; j <= 6 ; j++) 
            MatrixData (element -> K) [i][j] = 0.0;
   }

   if (mass_mode) {
      element -> M = CreateMatrix (8, 8);
   
      if (mass_mode == 'l') 
         QuadLumpedMassMatrix (element, numnodes);
      else if (mass_mode == 'c')
         QuadLumpedMassMatrix (element, numnodes);
   }

   if (element -> numdistributed > 0) {
      equiv = IsoQuadEquivNodalForces (element, &count);
      if (equiv == NullMatrix)
         return count;

       for (size_t i = 1; i <= numnodes ; i++) {
          element -> node[i] -> eq_force[1] += VectorData (equiv) [2*i - 1];
          element -> node[i] -> eq_force[2] += VectorData (equiv) [2*i];
       }
   }

   return 0;
} 

static int
QuadElementStress(Element element, unsigned int type)
{
   static Vector	stress = NullMatrix,
			d;
   static Matrix	temp;
   static Vector	weights;
   static Matrix	N, dNdxi, dNde,
                        dNdx, dNdy = NullMatrix;
   unsigned		numnodes;
   int			ninteg;
   Matrix		D,
			B;
   double		sigma_x,
			sigma_y,
			tau_xy;
   Vector		jac;
   unsigned		i,j;
   double		x,y;

   if (dNdy == NullMatrix) {
  
      N     = CreateMatrix (4,4);
      dNdxi = CreateMatrix (4,4);
      dNde  = CreateMatrix (4,4);
      dNdx  = CreateMatrix (4,4);
      dNdy  = CreateMatrix (4,4);
      weights = CreateVector (4);
   }
   
   if (stress == NullMatrix) {
      stress = CreateVector (3);
      d = CreateVector (8);
      temp = CreateMatrix (3,8);
   }

   ninteg = 4;

   if (type == PLANESTRAIN)
      D = PlaneStrainD (element);
   else if (type == PLANESTRESS)
      D = PlaneStressD (element);
   else
      D = NullMatrix; /* gcc -Wall */

   if (D == NullMatrix)
      return 1;

   if (element -> number == 1)
      numnodes = LocalQuadShapeFunctions (element, ninteg, N, 
                                          dNdxi, dNde, weights, FORCE);  
   else
      numnodes = LocalQuadShapeFunctions (element, ninteg, N, 
                                          dNdxi, dNde, weights, NOFORCE);  

   ninteg = 4;
   
   jac = GlobalQuadShapeFunctions (element,dNdxi,dNde,dNdx,dNdy,
                                   ninteg,numnodes);

   for (i = 1 ; i <= numnodes ; i++) {
      VectorData (d) [2*i - 1] = element -> node[i] -> dx[1];
      VectorData (d) [2*i] = element -> node[i] -> dx[2];
   }

   MatrixRows (d) = numnodes*2;
   MatrixCols (temp) = numnodes*2;

   element -> ninteg = ninteg;
   SetupStressMemory (element);

   for (int i = 1 ; i <= ninteg ; i++) {
      B = IsoQuadLocalB (element, numnodes, dNdx, dNdy, i);
      if (B == NullMatrix)
         return 1;

      x = y = 0.0;
      for (size_t j = 1 ; j <= numnodes ; j++) {
         x += MatrixData (N)[j][i]*element -> node[j] -> x;
         y += MatrixData (N)[j][i]*element -> node[j] -> y;
      }
   
      MultiplyMatrices (temp, D, B);  

      MultiplyMatrices (stress, temp, d);
    
      sigma_x = VectorData (stress) [1];
      sigma_y = VectorData (stress) [2];
      tau_xy = VectorData (stress) [3];

      element -> stress [i] -> x = x;
      element -> stress [i] -> y = y; 
      element -> stress [i] -> values [1] = sigma_x;
      element -> stress [i] -> values [2] = sigma_y;
      element -> stress [i] -> values [3] = 0.0;	/* sigma_z */
      element -> stress [i] -> values [4] = tau_xy;
      element -> stress [i] -> values [5] = 0.0;
      element -> stress [i] -> values [6] = 0.0;

      PrincipalStresses2D(element -> stress [i] -> values);
   }

   for (i = 1 ; i <= numnodes ; i++) {
      if (element -> node [i] -> stress == NULL)
         AllocateNodalStress(element -> node [i]);

      element -> node [i] -> numelts ++;

      for (j = 1 ; j <= 10 ; j++)
         element -> node [i] -> stress [j] += element -> stress [i] -> values [j];
   }


   return 0;
} 

static void
QuadLumpedMassMatrix(Element element, unsigned int numnodes)
{
   double	area, mass;
   unsigned	i;

   area = ElementArea (element, numnodes);
   mass = area * element -> material -> rho * 
          element -> material -> t / (double) numnodes;

   ZeroMatrix (element -> M);

   for (i = 1 ; i <= numnodes ; i++) {
      MatrixData (element -> M) [2*i - 1][2*i - 1] = mass;
      MatrixData (element -> M) [2*i][2*i] = mass;
   }

   return;
}
 
static Matrix
IsoQuadLocalB(Element element, unsigned int numnodes, Matrix dNdx, Matrix dNdy, unsigned int point)
{
   unsigned		i;
   static Matrix	B = NullMatrix;

   if (B == NullMatrix) 
      B = CreateMatrix (3,8);

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
GlobalQuadShapeFunctions(Element element, Matrix dNdxi, Matrix dNde, Matrix dNdx, Matrix dNdy, int ninteg, unsigned int nodes)
{
   unsigned		i,j;
   static Vector	jac,
			dxdxi, dxde,
			dydxi, dyde = NullMatrix;

   if (dyde == NullMatrix) {

      dxdxi = CreateVector (4);
      dxde  = CreateVector (4);
      dydxi = CreateVector (4);
      dyde  = CreateVector (4);
      jac = CreateVector (4);
   }
 
   for (i = 1 ; i <= 4 ; i++) {
      VectorData (dxdxi) [i] = 0.0; 
      VectorData (dxde) [i] = 0.0; 
      VectorData (dydxi) [i] = 0.0; 
      VectorData (dyde) [i] = 0.0; 
      VectorData (jac) [i] = 0.0;
   }

   for (int i = 1 ; i <= ninteg ; i++) {
      for (j = 1 ; j <= nodes ; j++) {

         VectorData (dxdxi) [i] += MatrixData (dNdxi) [j][i]*
                                   (element -> node[j] -> x);

         VectorData (dxde) [i] += MatrixData (dNde) [j][i]*
                                   (element -> node[j] -> x);

         VectorData (dydxi) [i] += MatrixData (dNdxi) [j][i]*
                                   (element -> node[j] -> y);
     
         VectorData (dyde) [i] += MatrixData (dNde) [j][i]*
                                   (element -> node[j] -> y);
      }

      VectorData (jac) [i] = VectorData (dxdxi)[i] * VectorData (dyde)[i] -
                         VectorData (dxde)[i] * VectorData (dydxi)[i];

      for (j = 1 ; j <= nodes ; j++) {
         MatrixData (dNdx)[j][i] = (MatrixData (dNdxi) [j][i]*VectorData (dyde)[i] -
                               MatrixData (dNde)[j][i]*VectorData (dydxi)[i])/
                               VectorData (jac)[i];

         MatrixData (dNdy)[j][i] = -(MatrixData (dNdxi)[j][i]*VectorData (dxde)[i] -
                                MatrixData (dNde)[j][i]*VectorData (dxdxi)[i])/
                                VectorData (jac)[i]; 
      }
   }

   return jac;
}

/*****************************************************************************
*
* Function:	LocalQuadShapeFunctions
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
LocalQuadShapeFunctions(Element element, unsigned int ninteg, Matrix N, Matrix dNdx, Matrix dNde, Vector weights, unsigned int force_init)
{
   unsigned		i,j,k;
   double		eta,xi;
   double		Nt[5];
   double		de[5],dx[5];
   double		*gauss_points;
   double		*gauss_wts;
   unsigned		numnodes;
   static unsigned	prev_nodes = 0;
   

   if (element -> node[3] -> number == element -> node[4] -> number) 
      numnodes = 3;
   else   
      numnodes = 4;   

   if (numnodes == prev_nodes && !force_init)
      return numnodes;

   ninteg /= 2;	/* how many in each dimension? */

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

         if (numnodes == 3) {
             Nt [3] += Nt [4];
             dx [3] += dx [4];
             de [3] += de [4];
         }

         for (k = 1 ; k <= 4 ; k++) {
            MatrixData (N) [k][i*ninteg + j+1] = Nt [k];
            MatrixData (dNdx) [k][i*ninteg + j+1] = dx [k];
            MatrixData (dNde) [k][i*ninteg + j+1] = de [k];
         }

         VectorData (weights) [i*ninteg + j+1] = gauss_wts [j];
      }
   }

   prev_nodes = numnodes;
   return numnodes;
}

static Vector
IsoQuadEquivNodalForces(Element element, int *err_count)
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
      equiv = CreateVector (8);

   count = 0;
   force1 = force2 = 0; /* gcc -Wall */
 
   if (element -> numdistributed > 2) {
      error ("quad element %d can have at most two distributed loads",
              element -> number);
      count++;
   }

   thick = element -> material -> t;

   for (i = 1 ; i <= 8 ; i++)
      VectorData (equiv) [i] = 0.0;

   for (i = 1 ; i <= element -> numdistributed ; i++) {

      if (element -> distributed[i] -> value.size() != 2) {
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

      if (node_a < 1 || node_a > 4 || node_b < 1 || node_b > 4) {
         error ("incorrect node numbering for load %s (element %d)", 
                element -> distributed[i] -> name,element -> number);
         count++;
      }

      if (node_a == node_b) {
         error ("incorrect node numbering for load %s (element %d)", 
                element -> distributed[i] -> name,element -> number);
         count++;
      }

      xc1 = element -> node[node_a] -> x;
      xc2 = element -> node[node_b] -> x;
      yc1 = element -> node[node_a] -> y;
      yc2 = element -> node[node_b] -> y;

      L = sqrt ((xc1 - xc2)*(xc1 - xc2) + (yc1 - yc2)*(yc1 - yc2));

      if (L <= TINY) {
         error ("length of side of element %d is zero to machine precision",
                 element -> number);
         count ++;
      } 

	/* 
	 * Thats all the error checking, bail out if we've had any
	 */

      if (count) {
         *err_count = count;
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
