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

double	atof(const char *);

int FrobeniusNormMatrix (double *result, Matrix a)
{
   unsigned	i, j;
   double	data;
   double	sum;
  
   sum = 0;
   for (i = 1 ; i <= Mrows(a) ; i++) {
      for (j = 1 ; j <= Mcols(a) ; j++) {
         data = mdata(a, i, j);
         sum += data*data;
      }
   }

   sum = sqrt (sum);
   *result = sum;

   return 0;
}

int PNormMatrix (double *result, Matrix a, char *p)
{
   unsigned	i, j;
   double	x;
   double	max;
  
   x = 0;

   if (strcmp (p,"inf") == 0) {
      max = 0;
      for (i = 1 ; i <= Mrows(a) ; i++) {
         x = 0;
         for (j = 1 ; j <= Mcols(a) ; j++)
            x += fabs (mdata(a, i, j));

         if (x > max)
            max = x;
      }
   }
   else if (strcmp (p, "1") == 0) {
      max = 0;
      for (j = 1 ; j <= Mcols(a) ; j++) {
         x = 0;
         for (i = 1 ; i <= Mrows(a) ; i++)
            x += fabs(mdata(a, i, j));

         if (x > max)
            max = x;
      }
   } 
   else
      return M_UNKNOWNNORM;

   *result = x;
   return 0;
}

int PNormVector (double *result, Matrix a, char *p)
{
   unsigned	i;
   double	data;
   double	x;
  
   if (Mcols(a) != 1)
      return M_NOTCOLUMN;
 
   if (strcmp (p,"inf") == 0) {
      x = fabs (mdata(a, 1, 1));
      for (i = 2 ; i <= Mrows(a) ; i++) {
         if (fabs (mdata(a, i, 1)) > x)
            x = fabs (mdata(a, i, 1));
      }
   }
   else if (strcmp (p, "1") == 0) {
      x = 0;
      for (i = 1 ; i <= Mrows(a) ; i++)        
         x += fabs (mdata(a, i, 1));
   } 
   else if (strcmp (p, "2") == 0) {
      x = 0;
      for (i = 1 ; i <= Mrows(a) ; i++) {
         data = mdata (a, i, 1);
         x += data*data;
      }

      x = sqrt(x);
   }
   else
      return M_UNKNOWNNORM;

   *result = x;
   return 0;
}
