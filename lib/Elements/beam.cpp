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
 * File:	beam.c							*
 *									*
 * Description:	This file contains the definition structure and		*
 *		stiffness function for a beam element.			*
 ************************************************************************/

# include <math.h>
# include "fe.h"
# include "error.h"
# include "misc.h"


static int beamEltSetup (Element element, char mass_mode, int tangent);
static int beamEltStress (Element element);

struct definition beamDefinition = {
    "beam", beamEltSetup, beamEltStress, 
    Linear, 2, 2, 3, 3, {0, 1, 2, 6, 0, 0, 0}, 1
};

static Matrix BeamLocalK           (Element element);
static Matrix BeamTransformMatrix  (Element element);
static Vector BeamEquivNodalForces (Element element, int *err_count);
static Matrix BeamConsistentMassMatrix (Element element);
static Matrix BeamLumpedMassMatrix (Element element);
static void   ResolveEndForces	    (Vector equiv, double wa, double wb, Direction direction, double L);

static int
beamEltSetup(Element element, char mass_mode, int tangent)
{
   Matrix		T,
			me,
			ke;
   Vector		equiv;
   int      		count;

   if (element -> material -> A == 0) {
      error ("beam element %d has 0.0 for cross-sectional area (A)",
              element -> number);
      return 1;
   }
   if (element -> material -> E == 0) {
      error ("beam element %d has 0.0 for Young's modulus (E)", 
              element -> number);
      return 1;
   }
   if (element -> material -> Ix == 0) {
      error ("beam element %d has 0.0 for Ixx moment of inertia (Ix)", 
              element -> number);
      return 1;
   }

   ke = BeamLocalK (element);
   if (ke == NullMatrix)
      return 1;

   T = BeamTransformMatrix (element);
   if (T == NullMatrix)
      return 1;

   if (element -> K == NullMatrix) 
      element -> K = CreateMatrix (6,6);
  
   MultiplyAtBA (element -> K, T, ke);

	/*
	 * deal with the possibility of hinged boundary conditions
	 */
  
   ResolveHingeConditions (element);

	/*
	 * deal with the possibility of distributed loads
	 */

   if (element -> numdistributed > 0) {
      equiv = BeamEquivNodalForces (element, &count);
      if (equiv == NullMatrix)
         return count;

      element -> node[1] -> eq_force[1] += VectorData (equiv) [1];
      element -> node[1] -> eq_force[2] += VectorData (equiv) [2];
      element -> node[1] -> eq_force[6] += VectorData (equiv) [3];
      element -> node[2] -> eq_force[1] += VectorData (equiv) [4];
      element -> node[2] -> eq_force[2] += VectorData (equiv) [5];
      element -> node[2] -> eq_force[6] += VectorData (equiv) [6];
   }

 	/*
	 * do we need to form the mass matrix? If so which kind?
	 */

    if (mass_mode) {
       if (mass_mode == 'c')
          me = BeamConsistentMassMatrix (element);
       else if (mass_mode == 'l')
          me = BeamLumpedMassMatrix (element); 
       else
	  me = NullMatrix;

       if (me == NullMatrix)
          return 1;

       if (element -> M == NullMatrix)
          element -> M = CreateMatrix (6,6);
       
       MultiplyAtBA (element -> M, T, me);
   }
 
   return 0;
}

static int
beamEltStress(Element element)
{
   unsigned		i;
   int			count;
   static Vector	f,
			dlocal,
			d = NullMatrix;
   Matrix		T;
   static Matrix	ke = NullMatrix;
   static Matrix	Tt;
   Vector		equiv;
   static Vector	eq_local;

   if (d == NullMatrix) {
      ke = CreateMatrix (6,6);
      d = CreateVector (6);
      f = CreateVector (6);
      Tt = CreateMatrix (6,6);
      dlocal = CreateVector (6);
      eq_local = CreateVector (6);
   } 

   VectorData (d) [1] = element -> node[1] -> dx[1];
   VectorData (d) [2] = element -> node[1] -> dx[2]; 
   VectorData (d) [3] = element -> node[1] -> dx[6]; 
   VectorData (d) [4] = element -> node[2] -> dx[1];
   VectorData (d) [5] = element -> node[2] -> dx[2];
   VectorData (d) [6] = element -> node[2] -> dx[6]; 

   T = BeamTransformMatrix (element);
   if (T == NullMatrix)
      return 1;

	/*
	 * We already have the element stiffness matrix (we saved it
	 * by setting element -> retainK = True).  We do have to transform
	 * it back to local coordinates however.  Once we're done with
	 * it, we should go ahead and destroy it.
	 */

   TransposeMatrix (Tt, T);
   MultiplyAtBA (ke, Tt, element -> K);

   DestroyMatrix (element -> K);
   element -> K = NullMatrix;

   MultiplyMatrices (dlocal, T, d);
   MultiplyMatrices (f, ke, dlocal);

   if (element -> numdistributed > 0) {
      equiv = BeamEquivNodalForces (element, &count);
      if (equiv == NullMatrix)
         return count;

      MultiplyMatrices (eq_local, T, equiv);

      for (i = 1 ; i <= 6 ; i++)
         VectorData (f) [i] -= VectorData (eq_local) [i];
   } 

   element -> ninteg = 2;
   SetupStressMemory (element);

   element -> stress [1] -> x = element -> node[1] -> x;
   element -> stress [1] -> y = element -> node[1] -> y;
   element -> stress [2] -> x = element -> node[2] -> x;
   element -> stress [2] -> y = element -> node[2] -> y;

   for (i = 1; i <= 3 ; i++) {
      element -> stress [1] -> values [i] = VectorData (f) [i];
      element -> stress [2] -> values [i] = VectorData (f) [i+3];
   }

   return 0;          
} 

	/*
	 * These are the essentially private functions
	 */

static Matrix
BeamLocalK(Element element)
{
   double		L,
			L3,
			L2;
   double		EI,
			AEonL;
   static Matrix	ke = NullMatrix;

   if (ke == NullMatrix) 
      ke = CreateMatrix (6,6);

   ZeroMatrix (ke);

   L = ElementLength (element, 2); 

   if (L <= TINY) {
      error ("length of element %d is zero to machine precision",
              element -> number);

      return NullMatrix;
   } 

   L2 = L*L;
   L3 = L2*L;

   EI = element -> material -> E * element -> material -> Ix;
   AEonL = (element -> material -> E * element -> material -> A)/L;

   MatrixData (ke) [1][1] = AEonL;
   MatrixData (ke) [1][4] = -AEonL;
   MatrixData (ke) [2][2] = 12*EI/L3;
   MatrixData (ke) [2][3] = 6*EI/L2;
   MatrixData (ke) [2][5] = -12*EI/L3;
   MatrixData (ke) [2][6] = 6*EI/L2;
   MatrixData (ke) [3][3] = 4*EI/L;
   MatrixData (ke) [3][5] = -6*EI/L2;
   MatrixData (ke) [3][6] = 2*EI/L;
   MatrixData (ke) [4][4] = AEonL;
   MatrixData (ke) [5][5] = 12*EI/L3;
   MatrixData (ke) [5][6] = -6*EI/L2;
   MatrixData (ke) [6][6] = 4*EI/L;

   MirrorMatrix (ke);

   return ke;
}

static Matrix
BeamLumpedMassMatrix(Element element)
{
   static Matrix	me = NullMatrix;
   double		L;
   double		factor;
   double		I_factor;

   if (me == NullMatrix) {
      me = CreateMatrix (6,6);
      ZeroMatrix (me);
   }

   L = ElementLength (element, 2);
   factor = (element -> material -> A * element -> material -> rho * L)/2.0;
   I_factor = factor*L*L/12.0;

   MatrixData (me) [1][1] = factor;
   MatrixData (me) [2][2] = factor;
   MatrixData (me) [3][3] = I_factor;
   MatrixData (me) [4][4] = factor;
   MatrixData (me) [5][5] = factor;
   MatrixData (me) [6][6] = I_factor;

   return me;
} 

static Matrix
BeamConsistentMassMatrix(Element element)
{
   static Matrix	me = NullMatrix;
   double		L;
   double		f1,f2;

   if (me == NullMatrix) {
      me = CreateMatrix (6,6);
      
      ZeroMatrix (me);
   }

   L = ElementLength (element, 2);
   f1 = (element -> material -> A * element -> material -> rho * L)/6.0;
   f2 = f1/70.0;

   MatrixData (me) [1][1] = 2.0*f1;
   MatrixData (me) [1][4] = f1;
   MatrixData (me) [2][2] = 156.0*f2;
   MatrixData (me) [2][3] = 22.0*L*f2;
   MatrixData (me) [2][5] = 54.0*f2;
   MatrixData (me) [2][6] = -13.0*L*f2;
   MatrixData (me) [3][3] = 4.0*L*L*f2;
   MatrixData (me) [3][5] = 13.0*L*f2;
   MatrixData (me) [3][6] = -3.0*L*L*f2;
   MatrixData (me) [4][4] = 2.0*f1;
   MatrixData (me) [5][5] = 156.0*f2;
   MatrixData (me) [5][6] = -22.0*L*f2;
   MatrixData (me) [6][6] = 4.0*L*L*f2;

   MirrorMatrix (me);

   return me;
}

static Matrix
BeamTransformMatrix(Element element)
{
   double		cx,cy,
			L;
   static Matrix	T = NullMatrix;

   if (T == NullMatrix) {
      T = CreateMatrix (6,6);
      ZeroMatrix (T);
   }

   L = ElementLength (element, 2);

   if (L <= TINY) {
      error ("length of element %d is zero to machine precision",
              element -> number);
      return NullMatrix;
   } 

   cx = (element -> node[2] -> x - element -> node[1] -> x)/L;
   cy = (element -> node[2] -> y - element -> node[1] -> y)/L;

   MatrixData (T) [1][1] = cx;
   MatrixData (T) [1][2] = cy;
   MatrixData (T) [2][1] = -cy;
   MatrixData (T) [2][2] = cx;
   MatrixData (T) [3][3] = 1;
   MatrixData (T) [4][4] = cx;
   MatrixData (T) [4][5] = cy;
   MatrixData (T) [5][4] = -cy;
   MatrixData (T) [5][5] = cx;
   MatrixData (T) [6][6] = 1;

   return T;
}

static Vector
BeamEquivNodalForces(Element element, int *err_count)
{
   double		L;
   double		wa,wb;
   int			count;
   unsigned		i,j;
   Matrix		T;
   static Matrix	Tt;
   static Vector 	equiv = NullMatrix;
   static Vector	result;
   double		theta;
 
   if (equiv == NullMatrix) {
      equiv = CreateVector (6);
      result = CreateVector (6);
      Tt = CreateMatrix (6,6);
   }

   ZeroMatrix (equiv);

   count = 0;
   wa = wb = 0;		/* gcc -Wall */
 
   if (element -> numdistributed > 2) {
      error ("beam elt %d can only have one distributed load", 
              element -> number);
      count++;
   }
 
   L = ElementLength (element, 2);

   if (L <= TINY) {
      error ("length of element %d is zero to machine precision",
              element -> number);
      count ++;
   } 

   for (i = 1 ; i <= element -> numdistributed ; i++) {
      if (element -> distributed[i] -> value.size() != 2) {
         error ("beam elt %d must have 2 values for a distributed load %s",
                element -> number, element -> distributed[i] -> name.c_str());
         count++;
      }

      if (element -> distributed[i] -> direction == GlobalZ || 
         element -> distributed[i] -> direction == LocalZ) {
         error ("invalid direction specified for beam elt %d distrib load %s",
                 element -> number, element -> distributed[i] -> name.c_str());
         count++;
      }

      for (j = 1 ; j <= element -> distributed[i] -> value.size() ; j++) {
         if (element -> distributed[i] -> value[j].node < 1 || 
            element -> distributed[i] -> value[j].node > 2) {

            error ("invalid node numbering for beam elt %d distrib load %s", 
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

   if (element -> node[2] -> y - element -> node[1] -> y == 0 &&
       element -> node[2] -> x - element -> node[1] -> x == 0) 
      theta = 0.0;
   else
      theta = atan2 (element -> node[2] -> y - element -> node[1] -> y,
                     element -> node[2] -> x - element -> node[1] -> x);

   for (i = 1 ; i <= element -> numdistributed ; i++) {
      if (element -> distributed[i] -> direction == GlobalX) {
         if (element -> distributed[i] -> value[1].node == 1) {
            wa = element -> distributed[i] -> value[1].magnitude*cos(-theta);
            wb = element -> distributed[i] -> value[2].magnitude*cos(-theta);
            ResolveEndForces (equiv, wa, wb, Parallel, L); 
            wa = element -> distributed[i] -> value[1].magnitude*sin(-theta);
            wb = element -> distributed[i] -> value[2].magnitude*sin(-theta);
            ResolveEndForces (equiv, wa, wb, Perpendicular, L); 
         }
         else if (element -> distributed[i] -> value[1].node == 2) {
            wb = element -> distributed[i] -> value[1].magnitude*cos(theta);
            wa = element -> distributed[i] -> value[2].magnitude*cos(theta);
            ResolveEndForces (equiv, wa, wb, Parallel, L); 
            wb = element -> distributed[i] -> value[1].magnitude*sin(theta);
            wa = element -> distributed[i] -> value[2].magnitude*sin(theta);
            ResolveEndForces (equiv, wa, wb, Perpendicular, L); 
         }
      }
      else if (element -> distributed[i] -> direction == GlobalY) {
         if (element -> distributed[i] -> value[1].node == 1) {
            wa = element -> distributed[i] -> value[1].magnitude*sin(theta);
            wb = element -> distributed[i] -> value[2].magnitude*sin(theta);
            ResolveEndForces (equiv, wa, wb, Parallel, L); 
            wa = element -> distributed[i] -> value[1].magnitude*cos(theta);
            wb = element -> distributed[i] -> value[2].magnitude*cos(theta);
            ResolveEndForces (equiv, wa, wb, Perpendicular, L); 
         }
         else if (element -> distributed[i] -> value[1].node == 2) {
            wb = element -> distributed[i] -> value[1].magnitude*sin(theta);
            wa = element -> distributed[i] -> value[2].magnitude*sin(theta);
            ResolveEndForces (equiv, wa, wb, Parallel, L); 
            wb = element -> distributed[i] -> value[1].magnitude*cos(theta);
            wa = element -> distributed[i] -> value[2].magnitude*cos(theta);
            ResolveEndForces (equiv, wa, wb, Perpendicular, L); 
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

   T = BeamTransformMatrix (element);
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

   force1 = force2 = moment1 = moment2 = 0;	/* gcc -Wall */

   if (direction == Perpendicular || direction == LocalY) {
      if (wa == wb) {		          /* uniform distributed load    */
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
      VectorData (equiv) [2] += force1;
      VectorData (equiv) [3] += moment1;
      VectorData (equiv) [5] += force2;
      VectorData (equiv) [6] += moment2;
   } else if (direction == LocalX || direction == Parallel) {
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
      VectorData (equiv) [4] += force2;
   }
}
