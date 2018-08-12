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
 * File:	rod.c							*
 *									*
 * Description: This file contains the definition structure and the	*
 *		stiffness and stress functions for the one-dimensional	*
 *		heat transfer (thermal analysis) element.		*
 *		The assumed element shape is a simple circular rod.	*
 ************************************************************************/

# include <math.h>
# include <stdio.h>
# include "fe.h"
# include "error.h"
# include "misc.h"
# include "definition.h"

static int RodLumpedCapacityMatrix (Element e);
static int RodConsistentCapacityMatrix (Element e);
static int rodEltSetup  (Element element, char mass_mode, int tangent);
static int rodEltStress (Element element);

void rodInit()
{
    Definition dd(new definition_t("rod"));
    dd->setup = rodEltSetup;
    dd->stress = rodEltStress;
    dd->shape = Linear;
    dd->numnodes = 2;
    dd->shapenodes = 2;
    dd->numstresses = 0;
    dd->numdofs = 1;
    int dofsarr[] = {0,1,0,0,0,0,0};
    for (int i=0; i<7; i++)
    dd->dofs[i] = dofsarr[i];
    dd->retainK = 0;
    AddDefinition(dd);
}

static Vector RodResolveConvection (Element element, int *err_count);

static int
rodEltSetup(Element element, char mass_mode, int tangent)
{
   unsigned		i;
   Vector		equiv;
   int			count;
   double		factor;
   double		length;

   count = 0;
   if (element -> material -> Kx == 0) {
      error ("Rod element %d has 0.0 for x-conductivity (Kx)", element -> number);
      count ++;
   }
   if (element -> material -> A == 0) {
      error ("Rod element %d has 0.0 for cross-sectional area (A)", element -> number);
      count ++;
   }
   if (mass_mode && element -> material -> c == 0) {
      error ("Rod element %d has 0.0 for heat capacitance (c)", element -> number);
      count ++;
   }

   length = ElementLength (element, 3);
   if (length <= TINY) {
      error ("length of element %d is zero to machine precision", element -> number);
      count ++;
   }

   if (count)
      return count; 

   factor = element -> material -> A * element -> material -> Kx / length;

   if (!element -> K)
      element -> K = CreateMatrix (2,2);

   MatrixData (element -> K) [1][1] = factor;
   MatrixData (element -> K) [1][2] = -factor;
   MatrixData (element -> K) [2][1] = -factor;
   MatrixData (element -> K) [2][2] = factor;

   if (element -> numdistributed > 0) {
      equiv = RodResolveConvection (element, &count);
      if (!equiv)
         return count;

       for (i = 1; i <= 2 ; i++) 
          element -> node[i] -> eq_force[1] += VectorData (equiv) [i];
   }

   if (mass_mode) {
      if (!element -> M)
         element -> M = CreateMatrix (2,2);
     
      if (mass_mode == 'l') 
         RodLumpedCapacityMatrix (element);
      else if (mass_mode == 'c') 
         RodConsistentCapacityMatrix (element);
   }

   return 0;
}

static int
rodEltStress(Element element)
{
   element -> ninteg = 0;
   return 0;
} 

static int
RodLumpedCapacityMatrix(Element e)
{
   double	factor;
   double	L;

   L = ElementLength (e, 3);

   factor = e -> material -> A * e -> material -> c *
            e -> material -> rho * L / 2.0;

   MatrixData (e -> M) [1][1] = factor;
   MatrixData (e -> M) [2][2] = factor;
   MatrixData (e -> M) [1][2] = 0.0;
   MatrixData (e -> M) [2][1] = 0.0;

   return 0;
}

static int
RodConsistentCapacityMatrix(Element e)
{
   double	factor;
   double	L;

   L = ElementLength (e, 3);

   factor = e -> material -> A * e -> material -> c *
            e -> material -> rho * L / 6.0;

   MatrixData (e -> M) [1][1] = 2*factor;
   MatrixData (e -> M) [1][2] = factor;
   MatrixData (e -> M) [2][1] = factor;
   MatrixData (e -> M) [2][2] = 2*factor;

   return 0;
}

static Vector
RodResolveConvection(Element element, int *err_count)
{
   double		length;
   double		factor;
   int			count;
   double		end_area;
   double		surface_area;
   double		conv_coeff;
   double		Tinf;
   unsigned		node_a,
			node_b;
   unsigned		i;
   static Vector 	equiv;
   static Matrix	convK;
 
   if (!equiv) {
      equiv = CreateVector (2);
      convK = CreateMatrix (2,2);
   }

   count = 0;
 
   if (element -> numdistributed > 3) {
      error ("rod element %d can have at most three convecting surfaces",
              element -> number);
      count++;
   }

   end_area     = element -> material -> A;
   length       = ElementLength (element, 3);
   surface_area = 2.0*sqrt(M_PI*end_area)*length;

   ZeroMatrix (convK);

   for (i = 1 ; i <= 2 ; i++)
      VectorData (equiv) [i] = 0.0;

   for (i = 1 ; i <= element -> numdistributed ; i++) {

      if (element -> distributed[i] -> value.size() != 2) {
         error ("convection %s does not have 2 nodal values (element %d)",
                element -> distributed[i] -> name.c_str(), element -> number);
         count++;
      }

      node_a = element -> distributed[i] -> value[1].node;
      node_b = element -> distributed[i] -> value[2].node;

      if (node_a < 1 || node_a > 2 || node_b < 1 || node_b > 2) {
         error ("incorrect node numbering for convection %s (element %d)", 
                element -> distributed[i] -> name.c_str(),element -> number);
         count++;
      }

	/* 
	 * Thats all the error checking we can do right now, 
	 * bail out if we've had any
	 */

      if (count) {
         *err_count = count;
         return Matrix();
      }

	/*
	 * calculate the additional "force" that we will store in the
	 * nodes eq_force structure
	 */

      conv_coeff = element -> distributed[i] -> value[1].magnitude;
      Tinf = element -> distributed[i] -> value[2].magnitude;

      if (node_a == node_b) {
         factor = conv_coeff*Tinf*end_area;
         VectorData (equiv) [node_a] += factor;
      }
      else { 
         factor = conv_coeff*Tinf*surface_area/2.0;
         VectorData (equiv) [node_a] += factor;
         VectorData (equiv) [node_b] += factor;
      }

	/*
 	 * calculate the contribution of this convecting edge to
	 * the overall element stiffness matrix
	 */

      if (node_a == node_b) {
         factor = conv_coeff*end_area;
         MatrixData (convK) [node_a][node_a] += factor;
      }
      else {
         factor = conv_coeff*surface_area/6.0;
         MatrixData (convK) [1][1] += 2.0*factor;
         MatrixData (convK) [1][2] += factor;
         MatrixData (convK) [2][1] += factor;       
         MatrixData (convK) [2][2] += 2.0*factor;       
      }
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
