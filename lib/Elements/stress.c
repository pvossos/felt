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
 * File:	stress.c
 * 
 * Description:	contains routines to find principal stresses for solid
 *		elements
 *
 ******************************************************************************/

# include <math.h>
# include "fe.h"
# include "misc.h"
# include "error.h"
# include "allocate.h"

static int cubic(a, b, c, d, x)
   double	 a, b, c, d;
   double	*x;
{
  int	   nsol;
  double   a1 = b/a, a2 = c/a, a3 = d/a;
  double   Q = (a1*a1 - 3.0*a2)/9.0;
  double   R = (2.0*a1*a1*a1 - 9.0*a1*a2 + 27.0*a3)/54.0;
  double   R2_Q3 = R*R - Q*Q*Q;
  double   theta;

  if (R2_Q3 <= 0) {
     nsol = 3;
     theta = acos(R/sqrt(Q*Q*Q));
     x[0] = -2.0*sqrt(Q)*cos(theta/3.0) - a1/3.0;
     x[1] = -2.0*sqrt(Q)*cos((theta+2.0*M_PI)/3.0) - a1/3.0;
     x[2] = -2.0*sqrt(Q)*cos((theta+4.0*M_PI)/3.0) - a1/3.0;
  }
  else { 
     nsol = 1;
     x[0] = pow(sqrt(R2_Q3)+fabs(R), 1/3.0);
     x[0] += Q/x[0];
     x[0] *= (R < 0.0) ? 1 : -1;
     x[0] -= a1/3.0;
  }

  return nsol;
}

void PrincipalStresses3D(stress)
   double	*stress;
{
   double	I1, I2, I3;
   double	sx, sy, sz, txy, tyz, txz;
   double 	s1, s2, s3;
   double	sVM;
   double	x [3];
   int		i1, i2, i3;
   int		n;

   sx = stress [1];
   sy = stress [2];
   sz = stress [3];
   txy = stress [4];
   txz = stress [5];
   tyz = stress [6];

   I1 = sx + sy + sz;
   I2 = sx*sy + sx*sz + sy*sz - txy*txy - tyz*tyz - txz*txz;
   I3 = sx*sy*sz + 2*txy*tyz*txz - (sx*tyz*tyz + sy*txz*txz + sz*txy*txy);

   n = cubic(1.0, -I1, I2, -I3, x);
   fprintf (stderr,"%g %g %g\n", x[0], x[1], x[2]);
   i1 = 0;
   i3 = 2;
   if (x [1] > x [i1]) i1 = 1;
   if (x [2] > x [i1]) i1 = 2;
   if (x [0] < x [i3]) i3 = 0;
   if (x [1] < x [i3]) i3 = 1;
   i2 = 3 - i1 - i3;
    
   s1 = x [i1];
   s2 = x [i2];
   s3 = x [i3];
  
   sVM = sqrt(0.5*((s1 - s2)*(s1 - s2) 
	           + (s1 - s3)*(s1 - s3) + (s2 - s3)*(s2 - s3)));

   stress [7] = s1;
   stress [8] = s2;
   stress [9] = s3;
   stress [10] = sVM;

   return;
}

void PrincipalStresses2D(stress)
   double *stress;
{
   double	sx, sy, txy;
   double 	s1, s2, s3;
   double	sVM;
   double	diameter;
   double       x [2];
   int		i1, i2;

   sx = stress [1];
   sy = stress [2];
   txy = stress [4];

   diameter = sqrt((sx - sy)*(sx - sy)/4 + txy*txy);

   x[0] = (sx + sy)/2 + diameter;
   x[1] = (sx + sy)/2 - diameter;

   i1 = 0;
   i2 = 1;
   if (x [i1] < x [i2]) {
      i1 = 1;
      i2 = 0;
   } 
   
   s1 = x [i1];
   s2 = x [i2];
   s3 = 0.0;

   sVM = sqrt(0.5*((s1 - s2)*(s1 - s2) 
	           + (s1 - s3)*(s1 - s3) + (s2 - s3)*(s2 - s3)));

   stress [7] = s1;
   stress [8] = s2;
   stress [9] = s3;
   stress [10] = sVM;

   return;
}

/*****************************************************************************
 *
 * Function:	 SetupStressMemory 
 *
 * Return value: none
 *
 *****************************************************************************/

void SetupStressMemory (element)
    Element	element;
{
    unsigned	i;

    element -> stress = Allocate (Stress, element -> ninteg);
    if (element -> stress == NULL)
        Fatal ("allocation error setting up stress memory\n");

    UnitOffset (element -> stress);

    for (i = 1 ; i <= element -> ninteg  ; i++) {

	/*
	 * now allocate space for each actual stress structure
	 */

        element -> stress[i] = Allocate (struct stress, 1);
        if (element -> stress [i] == NULL)
           Fatal ("allocation error setting up stress memory\n");
        
	/*
	 * followed by space for each actual stress value (fy and mz)
	 */

        element -> stress[i] -> values = 
                 Allocate (double, element -> definition -> numstresses);
  
        if (element -> stress[i] -> values == NULL)
           Fatal ("allocation error setting up stress memory\n");

        UnitOffset (element -> stress[i] -> values);
    }
    
    return;
}


void AllocateNodalStress(node)
   Node		node;
{
   int		j;

   if (node -> stress)
      return;

   node -> stress = Allocate(double, 10);
   if (!node -> stress)
      Fatal("error allocating memory for nodal stresses");

   UnitOffset(node -> stress);
 
   for (j = 1 ; j <= 10 ; j++)
      node -> stress [j] = 0.0;

   return;
}
