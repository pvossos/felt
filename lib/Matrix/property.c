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
 * File:	property.c
 *	
 * Description:	
 *		
 ************************************************************************/

# include <stdio.h>
# include "matrix.h"

int IsSymmetricMatrix (a)	/* Aij == Aji ? 1 : 0		*/
   Matrix	a;		/* matrix to check for symmetry */
{
   unsigned	i, j;
   int		status;
   
   if (!IsSquare(a))
      return 0;

   status = 1;
   for (i = 1 ; i <= Mrows(a) ; i++) {
      for (j = i ; j <= Mcols(a) ; j++) {
         if (mdata(a, i, j) != mdata(a, j, i)) {
            status = 0;
            break;
         }
      }
      if (!status)
         break;
   }

   return status;
}

int IsZeroMatrix (a)		/* Aij == 0 ? 1 : 0		*/
   Matrix	a;		/* matrix to check for symmetry */
{
   unsigned	i, j;
   int		status;

   status = 1;
   for (i = 1 ; i <= Mrows(a) ; i++) {
      for (j = 1 ; j <= Mcols(a) ; j++) {
         if (mdata(a,i,j) != 0) {
            status = 0;
            break;
         }
      }
      if (!status)
         break;
   }

   return status;
}
