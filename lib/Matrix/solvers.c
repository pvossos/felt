/*
    This file is part of the FElt finite element analysis package.
    Copyright (C) 1993-1997 Jason I. Gobat and Darren C. Atkinson

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
 *
 * File:	solvers.c
 *	
 * Description:	
 *		
 ************************************************************************/

# include <stdio.h>
# include <math.h>
# include <string.h>
# include "matrix.h"

int GaussSeidel(x, A, b)
   Matrix	x;
   Matrix	A;
   Matrix	b;
{
   static int	   maxits = 5000;
   static double   tol = 0.0001;
   int		   iter;
   int		   i, j;
   int		   n;
   double	   sum1, sum2;
   int		   converged;
   double	   new_x;
   double	   diff, base;;

   if (!IsSquare(A))
      return M_NOTSQUARE;

   if (Mrows(b) != Mrows(A) || Mrows(x) != Mrows(A))
      return M_SIZEMISMATCH;

   if (!IsColumnVector(x) || !IsColumnVector(b))
      return M_NOTCOLUMN;

   n = Mrows(A);

   for (i = 1 ; i <= n ; i++) 
      if (mdata(A,i,i) == 0.0)
         return M_SINGULAR;

   converged = 0;

   for (iter = 1 ; iter <= maxits ; iter++) {

      diff = 0.0;
      base = 0.0;
      for (i = 1 ; i <= n ; i++) {

         sum1 = sum2 = 0.0;
         for (j = 1 ; j <= i-1 ; j++)
            sum1 += mdata(A,i,j)*mdata(x,j,1);

         for (j = i+1 ; j <= n ; j++)
            sum2 += mdata(A,i,j)*mdata(x,j,1);

         new_x = (mdata(b,i,1) - sum1 - sum2) / mdata(A,i,i);

         diff += (new_x - mdata(x,i,1))*(new_x - mdata(x,i,1));
         base += new_x*new_x;

         sdata(x, i, 1) = new_x;
      }

      if (base > 0 && diff/base < tol) {
         converged = 1;
         break;
      }
   }

   if (!converged)
      return M_NOTCONVERGED;

   return 0;
}
