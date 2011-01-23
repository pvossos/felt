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
 * File:	draw3d.c						*
 ************************************************************************/

# include <math.h>
# include <X11/Intrinsic.h>
# include "Solution.h"
# include "draw3d.h"

static double x_min3d, y_min3d, z_min3d;
static double xscale3d, yscale3d, zscale3d;

static double transform [4][4];

static void CreateUnitMatrix (double (*mat)[4])
{
   unsigned	i, j;

   for (i = 0; i < 4; i++) {
      for (j = 0; j < 4; j++) {

         if (i == j)
            mat[i][j] = 1.0;
         else
            mat[i][j] = 0.0;
      }
   }
}

static void CreateScaleMatrix (double sx, double sy, double sz, double (*mat)[4])
{
    CreateUnitMatrix(mat);                              

    mat[0][0] = sx;
    mat[1][1] = sy;
    mat[2][2] = sz;
}

static void MatrixRotationX (double theta, double (*mat)[4])
{
   theta *= M_PI / 180.0;

   CreateUnitMatrix(mat);                              
   mat[1][1] = cos(theta);
   mat[1][2] = -sin(theta);
   mat[2][1] = sin(theta);
   mat[2][2] = cos(theta);
}

static void MatrixRotationY (double theta, double (*mat)[4])
{
   theta *= M_PI / 180.0;

   CreateUnitMatrix(mat);                             
   mat[0][0] = cos(theta);
   mat[0][2] = -sin(theta);
   mat[2][0] = sin(theta);
   mat[2][2] = cos(theta);
}

static void MatrixRotationZ (double theta, double (*mat)[4])
{
   theta *= M_PI / 180.0;

   CreateUnitMatrix(mat);                            
   mat[0][0] = cos(theta);
   mat[0][1] = -sin(theta);
   mat[1][0] = sin(theta);
   mat[1][1] = cos(theta);
}

static void VisMultiplyMatrices (double (*result)[4], double (*mat_a)[4], double (*mat_b)[4])
{
   unsigned	i,j,k;
   double 	temp[4][4];

   for (i = 0; i < 4; i++) {
      for (j = 0; j < 4; j++) {
         temp[i][j] = 0;
         for (k = 0; k < 4; k++) 
            temp[i][j] += mat_a[i][k] * mat_b[k][j];
      }
   }

   for (i = 0; i < 4; i++) 
      for (j = 0; j < 4; j++) 
         result[i][j] = temp[i][j];
}

void Convert3Dto2D (double x, double y, double z, float xdiff, float ydiff, float *xt, float *yt)
{
   int 		i,j;
   double 	v[4], res[4],		     
	  	w = transform[3][3];

   v[0] = (x - x_min3d)*xscale3d - 1.0;
   v[1] = (y - y_min3d)*yscale3d - 1.0;
   v[2] = (z - z_min3d)*zscale3d - 1.0;
   v[3] = 1.0;

   for (i = 0; i < 2; i++) {	             
       res[i] = transform[3][i];     
       for (j = 0; j < 3; j++) 
          res[i] += v[j] * transform[j][i];
   }

   for (i = 0; i < 3; i++) 
      w += v[i] * transform[i][3];

   if (w == 0) 
      w = 0.00001;

   *xt = ((res[0] * 10.0 / w));
   *yt = ((res[1] * 10.0*ydiff/xdiff / w));
}

void Setup3D (double min_x, double max_x, double min_y, double max_y, double min_z, double max_z)
{
   double 	ztemp, temp;
   double 	mat [4][4];

	/*
	 * perform the rotations 
	 */

   MatrixRotationY (solution -> yrot, transform);
   MatrixRotationX (solution -> xrot, mat);
   VisMultiplyMatrices (transform, transform, mat);
   MatrixRotationZ (solution -> zrot, mat);
   VisMultiplyMatrices (transform, transform, mat);
   CreateScaleMatrix (0.5, 0.5, 0.5, mat);
   VisMultiplyMatrices (transform, transform, mat);

	/*
	 * fudge min / max so the z direction will scale
	 */

   ztemp = (max_z - min_z) / (2.0 * solution -> zscale);
   temp = (max_z + min_z) / 2.0;
   min_z = temp - ztemp;
   max_z = temp + ztemp;

	/* 
	 * set the global variables 
	 */

   x_min3d = min_x;
   y_min3d = min_y;
   z_min3d = min_z;

   xscale3d = 2.0/(max_x - min_x);
   yscale3d = 2.0/(max_y - min_y);
   if (max_z - min_z > 0)
      zscale3d = 2.0/(max_z - min_z);
   else
      zscale3d = 0.0;
}
