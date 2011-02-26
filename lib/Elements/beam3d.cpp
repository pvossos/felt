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
 * File:	beam3d.c						*
 *									*
 * Description:	This file contains the definition structure and		*
 *		stiffness function for a 3-dimensional beam element.	*
 ************************************************************************/

# include <math.h>
# include <stdio.h>
# include "fe.h"
# include "error.h"
# include "misc.h"


static int beam3dEltSetup (Element element, char mass_mode, int tangent);
static int beam3dEltStress (Element element);

struct definition beam3dDefinition = {
    "beam3d", beam3dEltSetup, beam3dEltStress,
    Linear, 2, 2, 6, 6, {0, 1, 2, 3, 4, 5, 6}, 1
};

static Matrix Beam3dLumpedMassMatrix  (Element element);
static Matrix Beam3dLocalK            (Element element);
static Matrix Beam3dTransformMatrix   (Element element);
static Vector Beam3dEquivNodalForces  (Element element, int *err_count);

static void ResolveEndForces (Vector equiv, double wa, double wb, Direction direction, double L);

static int
beam3dEltSetup(Element element, char mass_mode, int tangent)
{
   Matrix		T,
			me,
			ke;
   int			count;
   Vector		equiv;
   unsigned		i;

   if (element -> material -> A == 0) {
      error ("3d beam element %d has 0.0 for cross-sectional area (A)", element -> number);
      return 1;
   }
   if (element -> material -> E == 0) {
      error ("3d beam element %d has 0.0 for Young's modulus (E)", element -> number);
      return 1;
   }
   if (element -> material -> Iy == 0) {
      error ("3d beam element %d has 0.0 for Iyy moment of inertia (Iy)", element -> number);
      return 1;
   }
   if (element -> material -> Iz == 0) {
      error ("3d beam element %d has 0.0 for Izz moment of inertia (Iz)", element -> number);
      return 1;
   }
   if (element -> material -> J == 0) {
      error ("3d beam element %d has 0.0 for polar moment of inertia (J)", element -> number);
      return 1;
   }
   if (element -> material -> G == 0) {
      error ("3d beam element %d has 0.0 for shear modulus (G)", element -> number);
      return 1;
   }

   ke = Beam3dLocalK (element);
   if (ke == NullMatrix)
      return 1;
   
   T = Beam3dTransformMatrix (element);
   if (T == NullMatrix)
      return 1; 

   if (element -> K == NullMatrix)
      element -> K = CreateMatrix (12,12);

   MultiplyAtBA (element -> K, T, ke);

	/*
	 * deal with hinges
	 */

   ResolveHingeConditions (element);

	/*
	 * deal with distributed loads
	 */

   if (element -> numdistributed > 0) {
      equiv = Beam3dEquivNodalForces (element, &count);
      if (equiv == NullMatrix)
         return count;

      for (i = 1 ; i <= 6 ; i++) {
         element -> node[1] -> eq_force[i] += VectorData (equiv) [i];
         element -> node[2] -> eq_force[i] += VectorData (equiv) [i+6];
      }
   }

	/*
	 * form the mass matrix if desired ... only a lumped formulation
	 * is currently available
	 */

   if (mass_mode) {
      if (mass_mode == 'l')
         me = Beam3dLumpedMassMatrix (element);
      else 
         me = Beam3dLumpedMassMatrix (element);

      if (me == NullMatrix)
         return 1;

      if (element -> M == NullMatrix)
         element -> M = CreateMatrix (12,12);

      MultiplyAtBA (element -> M, T, me);
   }

   return 0;
}

static int
beam3dEltStress(Element element)
{
   unsigned		i;
   int			count;
   static Vector	f,
			eq_local,
			dlocal,
			d = NullMatrix;
   Vector		equiv;
   static Matrix	ke = NullMatrix;
   static Matrix	Tt;
   Matrix		T;

   if (d == NullMatrix) {
      ke = CreateMatrix (12,12);
      Tt = CreateMatrix (12,12);
      d = CreateVector (12);
      f = CreateVector (12);
      dlocal = CreateVector (12);
      eq_local = CreateVector (12);
   } 

   for (i = 1 ; i <= 6 ; i++) {
      VectorData (d) [i] = element -> node[1] -> dx[i];
      VectorData (d) [i+6] = element -> node[2] -> dx[i]; 
   }

   T = Beam3dTransformMatrix (element);
   if (T == NullMatrix)
      return 1;

   TransposeMatrix (Tt, T);

   MultiplyAtBA (ke, Tt, element -> K);

   delete (element -> K);
   element -> K = NullMatrix;

   MultiplyMatrices (dlocal, T, d);
   MultiplyMatrices (f, ke, dlocal);
   
   if (element -> numdistributed > 0) {
      equiv = Beam3dEquivNodalForces (element, &count);
      if (equiv == NullMatrix)
         return count;

      MultiplyMatrices (eq_local, T, equiv);

      for (i = 1 ; i <= 12 ; i++)
         VectorData (f) [i] -= VectorData (eq_local) [i];
   } 

   element -> ninteg = 2;
   SetupStressMemory (element);

   element -> stress [1] -> x = element -> node[1] -> x;
   element -> stress [1] -> y = element -> node[1] -> y;
   element -> stress [2] -> x = element -> node[2] -> x;
   element -> stress [2] -> y = element -> node[2] -> y;

   for (i = 1 ; i <= 6 ; i++) {
      element -> stress [1] -> values [i] = VectorData (f) [i];
      element -> stress [2] -> values [i] = VectorData (f) [i+6];
   }

   return 0;          
} 

static Matrix
Beam3dLocalK(Element element)
{
   double		L,
			L3,
			L2;
   double		EIz,
			EIy,
			GJ,
			AEonL;
   static Matrix	ke = NullMatrix;

   if (ke == NullMatrix) 
      ke = CreateMatrix (12,12);

   ZeroMatrix (ke);

   L = ElementLength (element, 3);

   if (L <= TINY) {
      error ("length of element %d is zero to machine precision",element -> number);
      return NullMatrix;
   } 

   L2 = L*L;
   L3 = L2*L;

   EIy = element -> material -> E * element -> material -> Iy;
   EIz = element -> material -> E * element -> material -> Iz;
   GJ = element -> material -> J * element -> material -> G;
   AEonL = (element -> material -> E * element -> material -> A)/L;

   MatrixData (ke) [1][1]  = MatrixData (ke) [7][7]   = AEonL;
   MatrixData (ke) [2][2]  = MatrixData (ke) [8][8]   = 12*EIz/L3;
   MatrixData (ke) [3][3]  = MatrixData (ke) [9][9]   = 12*EIy/L3;
   MatrixData (ke) [4][4]  = MatrixData (ke) [10][10] = GJ/L;
   MatrixData (ke) [5][5]  = MatrixData (ke) [11][11] = 4*EIy/L;
   MatrixData (ke) [6][6]  = MatrixData (ke) [12][12] = 4*EIz/L;
   MatrixData (ke) [1][7]  = -AEonL;
   MatrixData (ke) [2][6]  = MatrixData (ke) [2][12]  = 6*EIz/L2;
   MatrixData (ke) [2][8]  = -12*EIz/L3;
   MatrixData (ke) [3][5]  = MatrixData (ke) [3][11]  = -6*EIy/L2;
   MatrixData (ke) [3][9]  = -12*EIy/L3;
   MatrixData (ke) [4][10] = -GJ/L;
   MatrixData (ke) [5][9]  = MatrixData (ke) [9][11]  = 6*EIy/L2;
   MatrixData (ke) [5][11] = 2*EIy/L;
   MatrixData (ke) [6][8]  = MatrixData (ke) [8][12]  = -6*EIz/L2;
   MatrixData (ke) [6][12] = 2*EIz/L;

   MirrorMatrix (ke);
   return ke;
}

static Matrix
Beam3dLumpedMassMatrix(Element element)
{
   static Matrix	me = NullMatrix;
   double		L;
   double		factor;
   double		I_factor;

   if (me == NullMatrix) {
      me = CreateMatrix (12,12);
      ZeroMatrix (me);
   }

   L = ElementLength (element, 3); 
   factor = (element -> material -> A * element -> material -> rho * L)/2.0;
   I_factor = factor*L*L/12.0;

   MatrixData (me) [1][1] = factor;
   MatrixData (me) [2][2] = factor;
   MatrixData (me) [3][3] = factor;
   MatrixData (me) [4][4] = I_factor;
   MatrixData (me) [5][5] = I_factor;
   MatrixData (me) [6][6] = I_factor;
   MatrixData (me) [7][7] = factor;
   MatrixData (me) [8][8] = factor;
   MatrixData (me) [9][9] = factor;
   MatrixData (me) [10][10] = I_factor;
   MatrixData (me) [11][11] = I_factor;
   MatrixData (me) [12][12] = I_factor;

   return me;
}

static Matrix
Beam3dTransformMatrix(Element element)
{
   unsigned		i;
   double	   	cl,	
			cm,
			cn,
			d,	
			L;
   static Matrix	T = NullMatrix;

   if (T == NullMatrix) 
      T = CreateMatrix (12,12);

   ZeroMatrix (T);

   L = ElementLength (element, 3);

   if (L <= TINY) {
      error ("length of element %d is zero to machine precision",element -> number);
      return NullMatrix;
   } 

   cl = (element -> node[2] -> x - element -> node[1] -> x)/L;
   cm = (element -> node[2] -> y - element -> node[1] -> y)/L;
   cn = (element -> node[2] -> z - element -> node[1] -> z)/L;

   d = sqrt (cl*cl + cm*cm);

   for (i = 0 ; i <= 9 ; i += 3) {
      MatrixData (T) [i+1][i+1] = cl;
      MatrixData (T) [i+1][i+2] = cm;
      MatrixData (T) [i+1][i+3] = cn;
      if (d <= TINY) {
         MatrixData (T) [i+2][i+1] = 0;
         MatrixData (T) [i+2][i+2] = 1;
         MatrixData (T) [i+3][i+1] = 1;
         MatrixData (T) [i+3][i+2] = 0;
      }
      else {
         MatrixData (T) [i+2][i+1] = -cm/d;
         MatrixData (T) [i+2][i+2] = cl/d;
         MatrixData (T) [i+3][i+1] = -cl*cn/d;
         MatrixData (T) [i+3][i+2] = -cm*cn/d;
      }
      MatrixData (T) [i+3][i+3] = d;
   }

   return T;
}

static Vector
Beam3dEquivNodalForces(Element element, int *err_count)
{
   double		L;
   double		wa,wb;
   int			count;
   unsigned		i,j;
   Matrix		T;
   double		cxx,cyx,czx,
			cxy,cyy,czy,
			cxz,cyz,czz;
   double		l,m,n,d;
   static Matrix	Tt;
   static Vector 	equiv = NullMatrix;
   static Vector	result;
 
   if (equiv == NullMatrix) {
      equiv = CreateVector (12);
      result = CreateVector (12);
      Tt = CreateMatrix (12,12);
   }

   ZeroMatrix (equiv);

   count = 0;
   wa = wb = 0;	/* gcc -Wall */
 
   if (element -> numdistributed > 3) {
      error ("beam3d elt %d can have at most three distributed loads", 
             element -> number);
      count++;
   }

   L = ElementLength (element, 3);

   if (L <= TINY) {
      error ("length of element %d is zero to machine precision",
              element -> number);
      count ++;
   } 

   for (i = 1 ; i <= element -> numdistributed ; i++) {

      if (element -> distributed[i] -> value.size() != 2) {
         error ("beam3d elt %d must have 2 values for distributed load %s", 
                 element -> number, element -> distributed[i] -> name.c_str());
         count++;
      }

      if (element -> distributed[i] -> direction == Perpendicular) {
          error ("invalid direction for beam3d elt %d distributed load %s", 
                  element -> number, element -> distributed[i] -> name.c_str());
          count++;
      }

      for (j = 1 ; j <= element -> distributed[i] -> value.size() ; j++) {
         if (element -> distributed[i] -> value[j].node < 1 || 
             element -> distributed[i] -> value[j].node > 2) {

             error ("invalid node numbering for beam3d elt %d distrib load %s",
                     element -> number, element -> distributed[i] -> name.c_str());
             count++;
         }
      }

      if (element -> distributed[i] -> value[1].node == 
          element -> distributed[i] -> value[2].node) {

          error ("incorrect node numbering for elt %d distributed load %s", 
                  element -> number, element -> distributed[i] -> name.c_str());
          count++;
      }
   }

	/* 
	 * Thats all the error checking, bail out if we've had any
	 */

   if (count) {
      *err_count = count;
      return NullMatrix;
   }

	/*
	 * set-up the direction cosines to go from global to 
	 * local coordinates.  The first subscript is the global
	 * axis and the second is the local axis.  These values
	 * are the cosines of the angles between those two axes.
 	 * The coordinate systems are both right-hand, x-y in plane,
	 * z-out of plane.  z cross xhat = yhat.  This same
	 * basic transformation is set-up with the regular 
	 * transformation matrix.  
	 */

   l = (element -> node[2] -> x - element -> node[1] -> x)/L;
   m = (element -> node[2] -> y - element -> node[1] -> y)/L;
   n = (element -> node[2] -> z - element -> node[1] -> z)/L;
   cxx = l; cyx = m; czx = n;

   d = sqrt(l*l + m*m);
   
   if (d <= TINY) {
      cxy = 0.0;
      cyy = 1.0;
      cxz = 1.0;
      cyz = 0.0;  
   }
   else {
      cxy = -m/d;
      cyy = l/d;
      cxz = -l*n/d;
      cyz = -m*n/d;
   }

   czy = 0;
   czz = d;

	/*
	 * looking at this scheme it seems awfully brutish with the
	 * addition of the global directions ... ah well, at least it
	 * works (I think ...)
	 */

   for (i = 1 ; i <= element -> numdistributed ; i++) {
      if (element -> distributed[i] -> direction == GlobalX) {
         if (element -> distributed[i] -> value[1].node == 1) {
            wa = element -> distributed[i] -> value[1].magnitude * cxy;
            wb = element -> distributed[i] -> value[2].magnitude * cxy;
            ResolveEndForces (equiv, wa, wb, LocalY, L);
            wa = element -> distributed[i] -> value[1].magnitude * cxz;
            wb = element -> distributed[i] -> value[2].magnitude * cxz;
            ResolveEndForces (equiv, wa, wb, LocalZ, L);
            wa = element -> distributed[i] -> value[1].magnitude * cxx;
            wb = element -> distributed[i] -> value[2].magnitude * cxx;
            ResolveEndForces (equiv, wa, wb, Parallel, L);
         }
         else if (element -> distributed[i] -> value[1].node == 2) {
            wb = element -> distributed[i] -> value[1].magnitude * cxy;
            wa = element -> distributed[i] -> value[2].magnitude * cxy;
            ResolveEndForces (equiv, wa, wb, LocalY, L);
            wb = element -> distributed[i] -> value[1].magnitude * cxz;
            wa = element -> distributed[i] -> value[2].magnitude * cxz;
            ResolveEndForces (equiv, wa, wb, LocalZ, L);
            wb = element -> distributed[i] -> value[1].magnitude * cxx;
            wa = element -> distributed[i] -> value[2].magnitude * cxx;
            ResolveEndForces (equiv, wa, wb, Parallel, L);
         }
      }
      else if (element -> distributed[i] -> direction == GlobalY) {
         if (element -> distributed[i] -> value[1].node == 1) {
            wa = element -> distributed[i] -> value[1].magnitude * cyy;
            wb = element -> distributed[i] -> value[2].magnitude * cyy;
            ResolveEndForces (equiv, wa, wb, LocalY, L);
            wa = element -> distributed[i] -> value[1].magnitude * cyz;
            wb = element -> distributed[i] -> value[2].magnitude * cyz;
            ResolveEndForces (equiv, wa, wb, LocalZ, L);
            wa = element -> distributed[i] -> value[1].magnitude * cyx;
            wb = element -> distributed[i] -> value[2].magnitude * cyx;
            ResolveEndForces (equiv, wa, wb, Parallel, L);
         }
         else if (element -> distributed[i] -> value[1].node == 2) {
            wb = element -> distributed[i] -> value[1].magnitude * cyy;
            wa = element -> distributed[i] -> value[2].magnitude * cyy;
            ResolveEndForces (equiv, wa, wb, LocalY, L);
            wb = element -> distributed[i] -> value[1].magnitude * cyz;
            wa = element -> distributed[i] -> value[2].magnitude * cyz;
            ResolveEndForces (equiv, wa, wb, LocalZ, L);
            wb = element -> distributed[i] -> value[1].magnitude * cyx;
            wa = element -> distributed[i] -> value[2].magnitude * cyx;
            ResolveEndForces (equiv, wa, wb, Parallel, L);
         }
      }
      else if (element -> distributed[i] -> direction == GlobalZ) {
         if (element -> distributed[i] -> value[1].node == 1) {
            wa = element -> distributed[i] -> value[1].magnitude * czy;
            wb = element -> distributed[i] -> value[2].magnitude * czy;
            ResolveEndForces (equiv, wa, wb, LocalY, L);
            wa = element -> distributed[i] -> value[1].magnitude * czz;
            wb = element -> distributed[i] -> value[2].magnitude * czz;
            ResolveEndForces (equiv, wa, wb, LocalZ, L);
            wa = element -> distributed[i] -> value[1].magnitude * czx;
            wb = element -> distributed[i] -> value[2].magnitude * czx;
            ResolveEndForces (equiv, wa, wb, Parallel, L);
         }
         else if (element -> distributed[i] -> value[1].node == 2) {
            wb = element -> distributed[i] -> value[1].magnitude * czy;
            wa = element -> distributed[i] -> value[2].magnitude * czy;
            ResolveEndForces (equiv, wa, wb, LocalY, L);
            wb = element -> distributed[i] -> value[1].magnitude * czz;
            wa = element -> distributed[i] -> value[2].magnitude * czz;
            ResolveEndForces (equiv, wa, wb, LocalZ, L);
            wb = element -> distributed[i] -> value[1].magnitude * czx;
            wa = element -> distributed[i] -> value[2].magnitude * czx;
            ResolveEndForces (equiv, wa, wb, Parallel, L);
         }
      }
      else {
         if (element -> distributed[i] -> value[1].node == 1) {
            wa = element -> distributed[i] -> value[1].magnitude;
            wb = element -> distributed[i] -> value[2].magnitude;
         }
         else if (element -> distributed[i] -> value[1].node == 2) {
            wb = element -> distributed[i] -> value[1].magnitude;
            wa = element -> distributed[i] -> value[2].magnitude;
         }
         ResolveEndForces (equiv, wa, wb, 
                           element -> distributed[i] -> direction, L);
      }
   }

	/*
	 * Now that we know all is okay, allocate some memory if we
	 * haven't already done so for some other element
	 */

   SetEquivalentForceMemory (element);

   T = Beam3dTransformMatrix (element);
   TransposeMatrix (Tt, T);
   MultiplyMatrices (result, Tt, equiv);

   *err_count = 0;
   return result; 
}

static void
ResolveEndForces(Vector equiv, double wa, double wb, Direction direction, double L)
{
   double	force1, force2;
   double	moment1, moment2;

   force1 = force2 = moment1 = moment2 = 0; /* gcc -Wall */

   if (direction != Parallel && direction != LocalX) {
      if (wa == wb) {		         /* uniform distributed load    */
         force1 = force2 = wa*L/2.0;
         moment1 = wa*L*L/12.0;
         moment2 = -moment1;
      } 
      else if (fabs(wa) > fabs(wb)) {     /* load sloping node1 to node2 */
         force1 = wb*L/2.0 + 7.0*(wa - wb)*L/20.0; 
         force2 = wb*L/2.0 + 3.0*(wa - wb)*L/20.0;
         moment1 = wb*L*L/12.0 + (wa - wb)*L*L/20.0;
         moment2 = -wb*L*L/12.0 - (wa - wb)*L*L/30.0;
      }
      else if (fabs(wa) < fabs(wb)) {     /* load sloping node2 to node1 */
         force1 = wa*L/2.0 + 3.0*(wb - wa)*L/20.0; 
         force2 = wa*L/2.0 + 7.0*(wb - wa)*L/20.0;
         moment1 = wa*L*L/12.0 + (wb - wa)*L*L/30.0;
         moment2 = -wa*L*L/12.0 - (wb - wa)*L*L/20.0;
      } 

      if (direction == LocalY) {
         VectorData (equiv) [2] += force1;
         VectorData (equiv) [8] += force2;
         VectorData (equiv) [6] += moment1;
         VectorData (equiv) [12] += moment2;
      }
      else if (direction == LocalZ) {
         VectorData (equiv) [3] += force1;
         VectorData (equiv) [9] += force2;
         VectorData (equiv) [5] += moment1;
         VectorData (equiv) [11] += moment2;
      }

   } else if (direction == Parallel || direction == LocalX) {
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
      VectorData (equiv) [1] += force1;
      VectorData (equiv) [7] += force2;
   }
   return;
}
