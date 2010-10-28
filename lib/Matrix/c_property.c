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
# include "matrix.h"
# include "cmatrix.h"

int IsSymmetricComplexMatrix (const ComplexMatrix a)
{
   unsigned	i, j;
   int		status;
   
   if (!IsSquare(a))
      return 0;

   status = 1;
   for (i = 1 ; i <= Mrows(a) ; i++) {
      for (j = i ; j <= Mcols(a) ; j++) {
         if (re(cmdata(a,i,j)) != re(cmdata(a,j,i)) ||
             im(cmdata(a,i,j)) != im(cmdata(a,j,i))) {
            status = 0;
            break;
         }
      }
      if (!status)
         break;
   }

   return status;
}
