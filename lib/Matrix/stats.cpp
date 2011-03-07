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

# include <stdio.h>
# include <math.h>
# include <string.h>
# include "matrix.h"

int MaximumMatrix (const Matrix &a, double *x)
{
   double	max;
   unsigned	m, n;
   unsigned	i, j;

   n = Mrows(a);
   m = Mcols(a);

   max = mdata(a,1,1);
   for (i = 1 ; i <= n ; i++)
      for (j = 1 ; j <= m ; j++)
         if (mdata(a,i,j) > max)
            max = mdata(a,i,j);

   *x = max;

   return 0;
}

int MinimumMatrix (const Matrix &a, double *x)
{
   double	min;
   unsigned	m, n;
   unsigned	i, j;

   n = Mrows(a);
   m = Mcols(a);

   min = mdata(a,1,1);
   for (i = 1 ; i <= n ; i++)
      for (j = 1 ; j <= m ; j++)
         if (mdata(a,i,j) < min)
            min = mdata(a,i,j);

   *x = min;

   return 0;
}

int SumMatrix (const Matrix &a, double *x)
{
   double	sum;
   unsigned	m, n;
   unsigned	i,j;

   n = Mrows(a);
   m = Mcols(a);

   sum = 0.0;
   for (i = 1 ; i <= n ; i++)
      for (j = 1 ; j <= m ; j++)
         sum += mdata(a,i,j);

   *x = sum;

   return 0;
}

int MeanMatrix (const Matrix &a, double *x)
{
   double	mean;

   SumMatrix (a, &mean);
   *x = mean / (double) (Mrows(a) * Mcols(a));

   return 0;
}

int StddevMatrix (const Matrix &a, double *x)
{
   double	sum, sum2;
   unsigned	m, n;
   unsigned	i, j;
   double	d, nm;

   n = Mrows(a);
   m = Mcols(a);
   nm = n*m;

   if (nm < 2) {
      *x = 0.0;
      return 0;
   }
      
   sum = sum2 = 0.0;
   for (i = 1 ; i <= n ; i++) {
      for (j = 1 ; j <= m ; j++) {
         d = mdata(a,i,j);
         sum += d;
         sum2 += d*d;
      }
   }

   *x = sqrt (1.0/(nm - 1.0)*(sum2 - sum*sum/nm));

   return 0;
}
