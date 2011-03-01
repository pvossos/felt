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
* File:		htk.c
* 
* Description:	contains element code for a selective reduced integration
*		plate bending element (ref. Hughes et al, 1977).
*
* Algorithm:	Basically we compute shear and bending stiffness 
*		contributions separately and them add them together. 
*		The notation is a bit clumsy because we are under-integrating
*		the shear term ... so we have to have two complete sets
*		of all the shape function information around (one using
*		1x1 quadrature and one using 2x2 quadrature).
*
******************************************************************************/

# include <math.h>
# include "fe.h"
# include "error.h"
# include "misc.h"

static int htkEltSetup (Element element, char mass_mode, int tangent);
static int htkEltStress (Element element);

struct definition htkDefinition = {
   "htk", htkEltSetup, htkEltStress, 
   Planar, 4, 4, 5, 3, {0, 3, 4, 5, 0, 0, 0}, 0
};

# define TRIANGLE	3
# define QUADRILATERAL	4

static void     OnePointLocalShapeFunctions  (Element element, Matrix N, Matrix dNdxi, Matrix dNde, unsigned int shape);
static void     TwoPointLocalShapeFunctions  (Element element, Matrix N, Matrix dNdxi, Matrix dNde, unsigned int shape);
static void	HTKLumpedMassMatrix (Element element, unsigned int shape);
static Vector   GlobalShapeFunctions (Element element, Matrix dNdxi, Matrix dNde, Matrix dNdx, Matrix dNdy, unsigned int ninteg, unsigned int shape);
static void	AddContribution (Matrix K, Matrix B, Matrix D, double jac, unsigned int shape);
static void 	MultiplyDBd (Matrix D, Matrix B, Vector d, Vector res);
static int	EquivNodalForces (Element e, Matrix N, unsigned int shape, unsigned int ninteg);
static Matrix   FormBsMatrix (Element element, Matrix N, Matrix dNdx, Matrix dNdy, unsigned int numnodes, unsigned int point);
static Matrix   FormBbMatrix (Element element, Matrix dNdx, Matrix dNdy, unsigned int numnodes, unsigned int point);
static Matrix   FormDsMatrix  (Element element);
static Matrix   FormDbMatrix  (Element element);

	/*
	 * these are variables that we use for both Setup and Stress
	 * calculations -- why duplicate them?
	 * The 1 and the 2 refer to the number of quadrature points
	 * we defined them at.
	 */

static Matrix	N1;/* shape functions 		     */
static Matrix	dNde1;		/* shape func derivs in local coord  */
static Matrix	dNdxi1;		/* shape func derivs in local coord  */
static Matrix	dNdx1;		/* shape func derivs in global coord */
static Matrix 	dNdy1;		/* shape func derivs in global coord */
static Matrix	N2;		/* shape functions 		     */
static Matrix	dNde2;		/* shape func derivs in local coord  */
static Matrix	dNdxi2;		/* shape func derivs in local coord  */
static Matrix	dNdx2;		/* shape func derivs in global coord */
static Matrix 	dNdy2;		/* shape func derivs in global coord */

static int
htkEltSetup(Element element, char mass_mode, int tangent)
{
   unsigned		i;
   Matrix		Ds, Db;
   Matrix		Bs, Bb;		
   Vector		jac1;		/* vector of Jacobian determinants   */
   Vector		jac2;		/* vector of Jacobian determinants   */
   unsigned		shape;		/* triangle or quadrilateral ?	     */
   int			count;

	/*
	 * some one time only initializations
	 */

   if (!N1) {
      N1 = CreateMatrix (4,4);
      dNde1  = CreateMatrix (4,4);
      dNdxi1 = CreateMatrix (4,4);
      dNdx1  = CreateMatrix (4,4);
      dNdy1  = CreateMatrix (4,4);
      N2 = CreateMatrix (4,4);
      dNde2  = CreateMatrix (4,4);
      dNdxi2 = CreateMatrix (4,4);
      dNdx2  = CreateMatrix (4,4);
      dNdy2  = CreateMatrix (4,4);
   } 

   count = 0;

   if (element -> material -> E == 0) {
      error ("htk element %d has 0.0 for Young's modulus (E)", element -> number);
      count++;
   }

   if (element -> material -> nu == 0) {
      error ("htk element %d has 0.0 for Poisson's ratio (nu)", element -> number);
      count++;
   }

   if (element -> material -> t == 0) {
      error ("htk element %d has 0.0 for thickness (t)", element -> number);
      count++;
   }

   if (element -> material -> kappa == 0)
      element -> material -> kappa = 5.0/6.0;

   if (element -> material -> G == 0)
      element -> material -> G = 0.5*element -> material -> E /
                                 (1.0 + element -> material -> nu);

   if (count)
      return count;

   if (element -> node[3] -> number == element -> node[4] -> number)
      shape = TRIANGLE;
   else
      shape = QUADRILATERAL;

	/*
	 * we have two functions (rather than one that takes a ninteg
	 * parameter) for efficiency ... if we changed the ninteg
	 * parameter each time we would have to do all the computations
	 * for each element.  This way we probably only have to do them
	 * once each (unless the shape changes ...)
	 */

   OnePointLocalShapeFunctions (element, N1, dNdxi1, dNde1, shape);
   TwoPointLocalShapeFunctions (element, N2, dNdxi2, dNde2, shape);

   jac1 = GlobalShapeFunctions (element, dNdxi1, dNde1, dNdx1, dNdy1, 1, shape);
   jac2 = GlobalShapeFunctions (element, dNdxi2, dNde2, dNdx2, dNdy2, 4, shape);

	/*
	 * check our element distortion criteria
	 */

   for (i = 1 ; i <= 4 ; i++) {
      if (VectorData (jac2) [i] <= 0.0) {
         error ("det |J| for elt %d is <= 0.0, check distortion", element -> number);
         return 1;
      }
   }

   Ds = FormDsMatrix (element);
   Db = FormDbMatrix (element);

   if (!element -> K)
      element -> K = CreateMatrix (12,12);

   ZeroMatrix (element -> K); 

	/*
	 * there is only one integration point for the shear term -
	 * if we wanted to investigate the locking phenomenon, all we'd
  	 * have to do is put this in a loop (assuming we had built the
	 * shape functions properly!) ...  for both contributions
	 * remember that if the number of quadrature points was to go
	 * up the weights might change and the Jacobian terms would
	 * need to be scaled appropriately.
	 */

   Bs = FormBsMatrix (element, N1, dNdx1, dNdy1, shape, 1); 
   AddContribution (element -> K, Bs, Ds, 4*VectorData (jac1) [1], shape);

	/* 
	 * there were four integration points (2 x 2) for bending
	 */

   for (i = 1 ; i <= 4 ; i++) {
      Bb = FormBbMatrix (element, dNdx2, dNdy2, shape, i); 
      AddContribution (element -> K, Bb, Db, VectorData (jac2)[i], shape);
   }

	/*
	 * resolve any distributed loads
	 */

   if (element -> numdistributed != 0) {
      count = EquivNodalForces (element, N2, shape, 4);
      if (count)
         return count;
   }

	/*
	 * build a mass matrix if necessary
	 */

   if (mass_mode) {
      if (!element -> M)
         element -> M = CreateMatrix (12, 12);

      if (mass_mode == 'l')
         HTKLumpedMassMatrix (element, shape);
      else /* I suppose we really should form a consistent, but oh well */
         HTKLumpedMassMatrix (element, shape);
   }

   return 0;
}

static void
AddContribution(Matrix K, Matrix B, Matrix D, double jac, unsigned int shape)
{
   double	temp [5];
   double	result;
   unsigned	i,j,k;

   for (j = 1 ; j <= 3*shape ; j++) { 
      for (i = 1 ; i <= MatrixCols (D) ; i++) {
         temp [i] = 0;
         for (k = 1 ; k <= MatrixRows (D) ; k++)
            temp [i] += MatrixData (D) [k][i] * MatrixData (B) [k][j];
      }

      for (i = 1 ; i <= 3*shape ; i++) {
         result = 0;
         for (k = 1 ; k <= MatrixCols (D) ; k++)
            result += temp [k] * MatrixData (B) [k][i];

         MatrixData (K) [j][i] += jac*result;
      }
   }
}

static Vector
GlobalShapeFunctions(Element element, Matrix dNdxi, Matrix dNde, Matrix dNdx, Matrix dNdy, unsigned int ninteg, unsigned int shape)
{
   unsigned		i,j;
   static Vector	jac;
   double		dxdxi [5];
   double		dxde [5];
   double		dydxi [5];
   double		dyde [5];
	
   if (!jac) 
      jac = CreateVector (4);

   for (i = 1 ; i <= 4 ; i++) 
      dxdxi [i] = dxde [i] = dydxi [i] = dyde [i] = 0.0; 

   for (i = 1 ; i <= ninteg ; i++) {
      for (j = 1 ; j <= shape ; j++) {

         dxdxi [i] += MatrixData (dNdxi) [j][i] * element -> node[j] -> x;
         dxde [i]  += MatrixData (dNde) [j][i] * element -> node[j] -> x;
         dydxi [i] += MatrixData (dNdxi) [j][i] * element -> node[j] -> y;
         dyde [i]  += MatrixData (dNde) [j][i] * element -> node[j] -> y;

      }

      VectorData (jac) [i] = dxdxi[i] * dyde[i] - dxde[i] * dydxi[i];

      for (j = 1 ; j <= shape ; j++) {
         MatrixData (dNdx) [j][i] = (MatrixData (dNdxi) [j][i] * dyde [i] -
                                     MatrixData (dNde) [j][i] * dydxi [i])/
                                    VectorData (jac) [i];
         MatrixData (dNdy) [j][i] = (-MatrixData (dNdxi) [j][i] * dxde [i] +
                                     MatrixData (dNde) [j][i] * dxdxi [i])/
                                    VectorData (jac) [i];
      }
   }

   return jac;
}

static void
TwoPointLocalShapeFunctions(Element element, Matrix N, Matrix dNdxi, Matrix dNde, unsigned int shape)
{
   static double 	points [] = {-0.57735026918962, 0.57735026918962};
   unsigned		i, j;
   double		eta;
   double		xi;
   unsigned		p;
   static unsigned	prev_shape = 0;

   if (shape == prev_shape)
      return;
   else
      prev_shape = shape;

   for (i = 0 ; i < 2 ; i++) {
      eta = points [i];
      for (j = 0 ; j < 2 ; j++) {
         p = 2*i + j+1;
         xi = points [j];  
       
         MatrixData (N) [1][p]     = 0.25*(1 - eta)*(1 - xi);
         MatrixData (dNdxi) [1][p] = 0.25*(-1 + eta);
         MatrixData (dNde) [1][p]  = 0.25*(-1 + xi);
         MatrixData (N) [2][p]     = 0.25*(1 - eta)*(1 + xi);
         MatrixData (dNdxi) [2][p] = 0.25*(1 - eta);
         MatrixData (dNde) [2][p]  = 0.25*(-1 - xi);
         MatrixData (N) [3][p]     = 0.25*(1 + eta)*(1 + xi);
         MatrixData (dNdxi) [3][p] = 0.25*(1 + eta);
         MatrixData (dNde) [3][p]  = 0.25*(1 + xi);
         MatrixData (N) [4][p]     = 0.25*(1 + eta)*(1 - xi);
         MatrixData (dNdxi) [4][p] = 0.25*(-1 - eta);
         MatrixData (dNde) [4][p]  = 0.25*(1 - xi);

         if (shape == TRIANGLE) {
             MatrixData (N) [3][p]     += MatrixData (N) [4][p];
             MatrixData (dNdxi) [3][p] += MatrixData (dNdxi) [4][p];
             MatrixData (dNde) [3][p]  += MatrixData (dNde) [4][p];
         }
      }
   }
}

static void
OnePointLocalShapeFunctions(Element element, Matrix N, Matrix dNdxi, Matrix dNde, unsigned int shape)
{
   static double 	points [] = {0.0};
   unsigned		i, j;
   double		eta;
   double		xi;
   unsigned		p;
   static unsigned	prev_shape = 0;

   if (shape == prev_shape)
      return;
   else
      prev_shape = shape;

   for (i = 0 ; i < 1 ; i++) {
      xi = points [i];
      for (j = 0 ; j < 1 ; j++) {
         p = 2*i + j+1;
         eta = points [j];  
       
         MatrixData (N) [1][p]     = 0.25*(1 - eta)*(1 - xi);
         MatrixData (dNdxi) [1][p] = 0.25*(-1 + eta);
         MatrixData (dNde) [1][p]  = 0.25*(-1 + xi);
         MatrixData (N) [2][p]     = 0.25*(1 - eta)*(1 + xi);
         MatrixData (dNdxi) [2][p] = 0.25*(1 - eta);
         MatrixData (dNde) [2][p]  = 0.25*(-1 - xi);
         MatrixData (N) [3][p]     = 0.25*(1 + eta)*(1 + xi);
         MatrixData (dNdxi) [3][p] = 0.25*(1 + eta);
         MatrixData (dNde) [3][p]  = 0.25*(1 + xi);
         MatrixData (N) [4][p]     = 0.25*(1 + eta)*(1 - xi);
         MatrixData (dNdxi) [4][p] = 0.25*(-1 - eta);
         MatrixData (dNde) [4][p]  = 0.25*(1 - xi);

         if (shape == TRIANGLE) {
             MatrixData (N) [3][p]     += MatrixData (N) [4][p];
             MatrixData (dNdxi) [3][p] += MatrixData (dNdxi) [4][p];
             MatrixData (dNde) [3][p]  += MatrixData (dNde) [4][p];
         }
      }
   }
}

static Matrix
FormBsMatrix(Element element, Matrix N, Matrix dNdx, Matrix dNdy, unsigned int numnodes, unsigned int point)
{
   unsigned		i;
   static Matrix 	B;

   if (!B) 
      B = CreateMatrix (2, 12);

   ZeroMatrix (B);

   for (i = 1 ; i <= numnodes ; i++) {
      MatrixData (B) [1][3*i - 2] = MatrixData (dNdx) [i][point];
      MatrixData (B) [2][3*i - 2] = MatrixData (dNdy) [i][point];

      MatrixData (B) [2][3*i - 1] = -MatrixData (N) [i][point];

      MatrixData (B) [1][3*i]     = MatrixData (N) [i][point];
   }

   return B;
}
    
static Matrix
FormBbMatrix(Element element, Matrix dNdx, Matrix dNdy, unsigned int numnodes, unsigned int point)
{
   unsigned		i;
   static Matrix 	B;

   if (!B) 
      B = CreateMatrix (3, 12);

   ZeroMatrix (B);

   for (i = 1 ; i <= numnodes ; i++) {
      MatrixData (B) [2][3*i - 1] = MatrixData (dNdy) [i][point];
      MatrixData (B) [3][3*i - 1] = MatrixData (dNdx) [i][point];

      MatrixData (B) [1][3*i]     = -MatrixData (dNdx) [i][point];
      MatrixData (B) [3][3*i]     = -MatrixData (dNdy) [i][point];
   }

   return B;
}

static Matrix
FormDsMatrix(Element element)
{
   static Material	prev_material;
   static Matrix	D;

   if (!D) {
      D = CreateMatrix (2,2);

      MatrixData (D) [1][2] = 0.0;
      MatrixData (D) [2][1] = 0.0;
   }

   if (prev_material == element -> material)
      return D;
   else
      prev_material = element -> material;

   MatrixData (D) [1][1] = element -> material -> t*
                           element -> material -> G*
                           element -> material -> kappa;

   MatrixData (D) [2][2] = MatrixData (D) [1][1];

   return D;
}

static Matrix
FormDbMatrix(Element element)
{
   static Material	prev_material;
   static Matrix	D;
   double		c1, c2;
   double		t;

   if (!D) 
      D = CreateMatrix (3,3);

   if (prev_material == element -> material)
      return D;
   else
      prev_material = element -> material;

   t = element -> material -> t;
   c1 = t*t*t/12.0 * element -> material -> G;
   c2 = t*t*t/12.0 * element -> material -> E * element -> material -> nu /
        (1.0 - element -> material -> nu * element -> material -> nu);

   MatrixData (D) [1][1] = 2*c1 + c2;
   MatrixData (D) [1][2] = c2;
   MatrixData (D) [1][3] = 0.0;
   
   MatrixData (D) [2][2] = 2*c1 + c2; 
   MatrixData (D) [2][3] = 0.0;

   MatrixData (D) [3][3] = c1;

   MirrorMatrix (D);

   return D;
}

static int 
htkEltStress(Element element)
{
   unsigned		i;
   Matrix		Ds, Db;
   Matrix		Bs, Bb;		
   Vector		jac1;		/* vector of Jacobian determinants   */
   Vector		jac2;		/* vector of Jacobian determinants   */
   static Vector	d;
   static Vector	m;
   static Vector	q;
   unsigned		shape;		/* triangle or quadrilateral ?	     */
   double		xsum, ysum;

	/*
	 * some one time only initializations
	 */

   if (!d) {
      d = CreateVector (12);
      q = CreateVector (2);
      m = CreateVector (3);
   } 

	/*
	 * all this look a lot like the stiffness - except we already did
	 * all our error checking
	 */

   if (element -> node[3] -> number == element -> node[4] -> number)
      shape = TRIANGLE;
   else
      shape = QUADRILATERAL;

   OnePointLocalShapeFunctions (element, N1, dNdxi1, dNde1, shape);
   TwoPointLocalShapeFunctions (element, N2, dNdxi2, dNde2, shape);

   jac1 = GlobalShapeFunctions (element, dNdxi1, dNde1, dNdx1, dNdy1, 1, shape);
   jac2 = GlobalShapeFunctions (element, dNdxi2, dNde2, dNdx2, dNdy2, 4, shape);
   
   Ds = FormDsMatrix (element);
   Db = FormDbMatrix (element);

	/*
	 * now build the local displacement vector
	 */

   xsum = ysum = 0;

   for (i = 1 ; i <= shape ; i++) {
      VectorData (d) [3*i - 2] = element -> node[i] -> dx[3];
      VectorData (d) [3*i - 1] = element -> node[i] -> dx[4];
      VectorData (d) [3*i]     = element -> node[i] -> dx[5];

      xsum += element -> node[i] -> x;
      ysum += element -> node[i] -> y;
   }

   ZeroMatrix (q);
   Bs = FormBsMatrix (element, N1, dNdx1, dNdy1, shape, 1); 
   MultiplyDBd (Ds, Bs, d, q);

	/* 
	 * there were four integration points (2 x 2) for bending
	 */

   ZeroMatrix (m);
   for (i = 1 ; i <= 4 ; i++) {
      Bb = FormBbMatrix (element, dNdx2, dNdy2, shape, i); 
      MultiplyDBd (Db, Bb, d, m);
   }
   
   element -> ninteg = 1;
   SetupStressMemory (element);
 
   element -> stress[1] -> x = xsum / (double) shape;
   element -> stress[1] -> y = ysum / (double) shape;

   for (i = 1 ; i <= 3 ; i++)
      element -> stress[1] -> values [i] = -VectorData (m) [i]/4.0;

   element -> stress[1] -> values[4] = VectorData (q) [2];
   element -> stress[1] -> values[5] = VectorData (q) [1];

   return 0;
}

static void
MultiplyDBd(Matrix D, Matrix B, Vector d, Vector res)
{
   unsigned	i, j;
   double	temp [5];

   for (i = 1 ; i <= MatrixRows (B) ; i++) {
      temp [i] = 0.0;
      for (j = 1 ; j <= VectorSize (d) ; j++) 
         temp [i] += MatrixData (B) [i][j] * VectorData (d) [j];
   }

   for (i = 1 ; i <= MatrixRows (D) ; i++) {
      for (j = 1 ; j <= MatrixCols (D) ; j++) 
         VectorData (res) [i] += MatrixData (D) [i][j] * temp [j];
   }
}

static void
HTKLumpedMassMatrix(Element element, unsigned int shape)
{
   double	factor;
   double	area;
   unsigned	i;

   ZeroMatrix (element -> M);

   area = ElementArea (element, shape);
   factor = (element -> material -> t*element -> material -> rho*area)/shape;

   for (i = 1 ; i <= shape ; i++) 
      MatrixData (element -> M) [(i-1)*3 + 1][(i-1)*3 + 1] = factor; 

   return;
}

static int
EquivNodalForces(Element e, Matrix N, unsigned int shape, unsigned int ninteg)
{
   int		  count;
   static Vector  equiv;
   unsigned	  i,j;
   double	  area;
   double	  w[5];

   if (!equiv) 
     equiv = CreateVector (12);
   
   ZeroMatrix (equiv);

   count = 0;

   if (e -> numdistributed > 1) {
      error ("htk element %d has more than one distributed load.", e -> number);
      count ++;
   }  

   if (e -> distributed[1] -> value.size() != shape) {
      error ("htk element %d has a load without %d values.", e -> number, shape);
      count ++;
   }

   if (e -> distributed[1] -> direction != GlobalZ) {
      error ("htk element %d has load not in the GlobalZ direction.", e -> number);
      count ++;
   }
  
   for (i = 1 ; i <= shape ; i++) {
      w[i] = e -> distributed[1] -> value[i].magnitude;
      if (i > 1 && w[i] != w[1]) {
         error ("htk element %d has non-uniform loading.", e -> number);
         count ++;
      }
   }
  
   if (count)
      return count;
 
   area = ElementArea (e, shape);

   SetEquivalentForceMemory (e);
 
   for (i = 1 ; i <= shape ; i++) {
      for (j = 1 ; j <= ninteg ; j++) {
         VectorData (equiv) [3*i - 2] += MatrixData (N) [i][j] * w[i];
      }
   }

   ScaleMatrix (equiv, equiv, area/4, 0.0);

   for (i = 1 ; i <= shape ; i++) 
      e -> node [i] -> eq_force[Tz] = VectorData (equiv) [3*i - 2];

   return 0;
}
