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
 * File:	basic.c	       
 *	
 * Description:	
 *		
 ************************************************************************/

# include <time.h>
# include <math.h>
# include <stdio.h>
# include "matrix.h"
# include "cmatrix.h"

#undef srand48
#undef drand48
extern void srand48 ();
extern double drand48 ();

# if (defined DOS || defined __CYGWIN32__)
# define srand48 srand
# define drand48 rand
# endif

# define PRINT_TOL 1.0e-12

int ZeroComplexMatrix (a)		/* a = 0			*/
   ComplexMatrix	a;		/* matrix to fill with zeros	*/
{
   unsigned	i,j;

   if (IsCompact(a)) 
      for (i = 1 ; i <= Msize(a) ; i++) {
         rdata(a, i, 1) = 0.0;
         idata(a, i, 1) = 0.0;
      }
   else {
      for (i = 1 ; i <= Mrows(a) ; i++) {
         for (j = 1 ; j <= Mcols(a) ; j++) {
            rdata(a, i, j) = 0.0;
            idata(a, i, j) = 0.0;
         }
      }
   }

   return 0;
}

int MirrorComplexMatrix (a)
   ComplexMatrix	a;		/* matrix to mirror		   */
{
   unsigned	i, j;

   if (IsCompact (a))
      return M_COMPACT;

   for (i = 2 ; i <= Mrows(a) ; i++) {
      for (j = 1 ; j < i ; j++) {
         rdata (a, i, j) = re(cmdata (a, j, i));
         idata (a, i, j) = im(cmdata (a, j, i));
      }
   }

   return 0;
}

int CopyComplexMatrix (b, a)		/* b = a			   */
   ComplexMatrix	a;		/* source matrix		   */
   ComplexMatrix	b;		/* destination matrix		   */
{
   unsigned	i, j;

   if (IsCompact(b) && IsFull(a))
       return M_COMPACT;

   if (Mrows(a) != Mrows(b) || Mcols(a) != Mcols(b))
      return M_SIZEMISMATCH;

   if (IsCompact(b)) {
      for (i = 1 ; i <= Msize(a) ; i++) {
         b -> data [i][1].r = a -> data [i][1].r;
         b -> diag [i] = a -> diag [i];
         b -> data [i][1].i = a -> data [i][1].i;
         b -> diag [i] = a -> diag [i];
      }
   }
   else {
      for (i = 1 ; i <= Mrows(a) ; i++) {
         for (j = 1 ; j <= Mcols(a) ; j++) {
            rdata(b, i, j) = re(cmdata(a,i,j));
            idata(b, i, j) = im(cmdata(a,i,j));
         }
      }
    }

   return 0;
}

int RandomComplexMatrix (a, seed)	/* a(i,j) = rand()		*/
   ComplexMatrix	a;		/* matrix to randomize		*/
   int		seed;		/* optional seed		*/
{
   unsigned	i, j;
   static int	seeded = 0;

   if (IsCompact(a))
      return M_COMPACT;

   if (!seeded) {
      srand48(time(NULL));
      seeded = 1;
   }

   if (seed)
      srand48 (seed);

   for (i = 1 ; i <= Mrows(a) ; i++) {
      for (j = 1 ; j <= Mcols(a) ; j++) {
         rdata(a,i,j) = drand48();
         idata(a,i,j) = drand48();
      }
   }

   return 0;
}

int MultiplyComplexMatrices (c, a, b)	/* c = ab			*/
   ComplexMatrix	c;		/* destination matrix		*/
   ComplexMatrix 	a;		/* source matrix 1		*/
   ComplexMatrix	b;		/* source matrix 2		*/
{
   unsigned	i,j,k;

   if (IsCompact(c))
      return M_COMPACT;

   if (c == a || c == b)
      return M_NOOVERWRITE;

   if (Mrows(a) != Mrows(c) || Mcols(b) != Mcols(c) || Mcols(a) != Mrows(b))
      return M_SIZEMISMATCH;
   
   ZeroComplexMatrix (c);

   for (i = 1 ; i <= Mrows(a) ; i++) 
      for (j = 1 ; j <= Mcols(b) ; j++) 
         for (k = 1 ; k <= Mcols(a) ; k++) 
            sdata(c,i,j) = add(cmdata(c,i,j), mult(cmdata(a,i,k), cmdata(b,k,j)));

   return 0;
}

int AddComplexMatrices (c, a, b)	/* c = a + b			*/
   ComplexMatrix	c;		/* destination matrix		*/
   ComplexMatrix	a;		/* source matrix 1		*/
   ComplexMatrix	b;		/* source matrix 2		*/
{
   unsigned	i,j;

   if (IsCompact(c))
      return M_COMPACT;

   if (Mrows(a) != Mrows(b) || Mrows(b) != Mrows(c))
      return M_SIZEMISMATCH;
  
   if (Mcols(a) != Mcols(b) || Mcols(b) != Mcols(c))
      return M_SIZEMISMATCH;

   for (i = 1 ; i <= Mrows(a) ; i++)
      for (j = 1 ; j <= Mcols(a) ; j++)
         sdata(c,i,j) = add(cmdata(a, i, j), cmdata(b, i, j));

   return 0;
}

int SubtractComplexMatrices (c, a, b)	/* c = a - b 			*/
   ComplexMatrix	c;		/* destination matrix		*/
   ComplexMatrix	a;		/* source matrix 1		*/
   ComplexMatrix	b;		/* source matrix 2		*/
{
   unsigned	i,j;

   if (IsCompact(c))
      return M_COMPACT;

   if (Mrows(a) != Mrows(b) || Mrows(b) != Mrows(c))
      return M_SIZEMISMATCH;
  
   if (Mcols(a) != Mcols(b) || Mcols(b) != Mcols(c))
      return M_SIZEMISMATCH;

   for (i = 1 ; i <= Mrows(a) ; i++)
      for (j = 1 ; j <= Mcols(a) ; j++)
         sdata(c,i,j) = sub(cmdata(a, i, j), cmdata(b, i, j));

   return 0;
}

int ScaleComplexMatrix(b, a, factor, offset)	/* b(i,j) = factor*a(i,j) + offset  */
   ComplexMatrix	b;			/* destination matrix		    */
   ComplexMatrix	a;			/* source matrix		    */
   complex		factor;			/* multiplicative scale factor	    */
   complex		offset;			/* additive offset		    */
{
   unsigned	i,j;

   if (IsCompact(b))
      return M_COMPACT;

   if (Mrows(a) != Mrows(b) || Mcols(a) != Mcols(b))
      return M_SIZEMISMATCH;
 
   for (i = 1 ; i <= Mrows(a) ; i++)
      for (j = 1 ; j <= Mcols(a) ; j++)
         sdata(b, i, j) = add(mult(cmdata(a,i,j),factor), offset);

   return 0;
}

int ModulusComplexMatrix(b, a)
   Matrix		b;
   ComplexMatrix	a;
{
   unsigned	i,j;
   
   if (IsCompact(b))
      return M_COMPACT;

   if (Mrows(a) != Mrows(b) || Mcols(a) != Mcols(b))
      return M_SIZEMISMATCH;

   for (i = 1 ; i <= Mrows(a) ; i++) 
      for (j = 1 ; j <= Mcols(a) ; j++)
         sdata(b, i, j) = modulus(cmdata(a,i,j)); 

   return 0;
}

int TransposeComplexMatrix(b, a)	/* b = aT			*/
   ComplexMatrix	b;		/* destination matrix		*/
   ComplexMatrix	a;		/* source matrix		*/
{
   unsigned	i, j;

   if (IsCompact(b))
      return M_COMPACT;

   if (b == a)
      return M_NOOVERWRITE;

   if (Mrows(a) != Mcols(b) || Mcols(a) != Mrows(b))
      return M_SIZEMISMATCH;

   for (i = 1 ; i <= Mrows(a) ; i++)
      for (j = 1 ; j <= Mcols(a) ; j++)
         sdata(b,j,i) = cmdata(a, i, j);

   return 0;
}

int PrintComplexMatrix (m, fp)		/* print matrix m to file fp	*/
   ComplexMatrix	m;		/* matrix to print		*/
   FILE		*fp;			/* file pointer for output	*/
{
   complex	val;
   unsigned	start, end;
   unsigned     i, j;
   

   for (start = 1 ; start <= Mcols(m) ; start += 7) {
      end = start + 6 > Mcols(m) ? Mcols(m) : start + 6;
   
      if (Mcols(m) > 7)
         fprintf (fp, "\nColumns %d through %d\n\n", start, end);
      else
         fprintf (fp, "\n");

      for (j = 1 ; j <= Mrows(m) ; j++) {
         for (i = start ; i <= end ; i++) {
            val = cmdata(m,j,i);
            if (modulus(val) < PRINT_TOL) {
               val.r = 0.0;
               val.i = 0.0;
            }

            fprintf (fp, "%s ", cprint(val));
         }
         fprintf (fp, "\n");
      }
   }
     
   fprintf (fp, "\n");
   
   return 0;
}
