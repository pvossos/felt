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
 * File:	basic.c	       
 *	
 * Description:	
 *		
 ************************************************************************/

# include <time.h>
# include <math.h>
# include <stdio.h>
# include "matrix.h"

#undef srand48
#undef drand48
extern void srand48 ();
extern double drand48 ();

# ifdef DOS
# define srand48 srand
# define drand48 rand
# endif

# define PRINT_TOL 1.0e-12

int ZeroMatrix (a)		/* a = 0			*/
   Matrix	a;		/* matrix to fill with zeros	*/
{
   unsigned	i,j;

   if (IsCompact(a)) 
      for (i = 1 ; i <= Msize(a) ; i++)
         sdata(a, i, 1) = 0.0;
   else {
      for (i = 1 ; i <= Mrows(a) ; i++)
         for (j = 1 ; j <= Mcols(a) ; j++)
            sdata(a, i, j) = 0;
   }

   return 0;
}

int MirrorMatrix (a)
   Matrix	a;		/* matrix to mirror		   */
{
   unsigned	i, j;

   if (IsCompact (a))
      return M_COMPACT;

   for (i = 2 ; i <= Mrows(a) ; i++)
      for (j = 1 ; j < i ; j++)
         sdata (a, i, j) = mdata (a, j, i);

   return 0;
}

int CopyMatrix (b, a)		/* b = a			   */
   Matrix	a;		/* source matrix		   */
   Matrix	b;		/* destination matrix		   */
{
   unsigned	i, j;

   if (IsCompact(b) && IsFull(a))
       return M_COMPACT;

   if (Mrows(a) != Mrows(b) || Mcols(a) != Mcols(b))
      return M_SIZEMISMATCH;

   if (IsCompact(b)) {
      for (i = 1 ; i <= Msize(a) ; i++) {
         b -> data [i][1] = a -> data [i][1];
         b -> diag [i] = a -> diag [i];
      }
   }
   else {
      for (i = 1 ; i <= Mrows(a) ; i++)
         for (j = 1 ; j <= Mcols(a) ; j++)
            sdata(b,i,j) = mdata(a, i, j);
    }

   return 0;
}

int IdentityMatrix (a)		/* a = [I]			   */
   Matrix	a;		/* destination matrix for identity */
{
   unsigned	i;
   int		status;

   if (IsCompact(a))
      return M_COMPACT;

   status = ZeroMatrix (a);
   if (status)
      return status;

   if (Mrows(a) < Mcols(a))
      for (i = 1 ; i <= Mrows(a) ; i++)
         sdata(a, i, i) = 1;
   else
      for (i = 1 ; i <= Mcols(a) ; i++)
         sdata(a, i, i) = 1;

   return 0;
}

int RandomMatrix (a, seed)	/* a(i,j) = rand()		*/
   Matrix	a;		/* matrix to randomize		*/
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

   for (i = 1 ; i <= Mrows(a) ; i++)
      for (j = 1 ; j <= Mcols(a) ; j++)
         sdata(a,i,j) = drand48();

   return 0;
}

int MultiplyMatrices (c, a, b)	/* c = ab			*/
   Matrix	c;		/* destination matrix		*/
   Matrix 	a;		/* source matrix 1		*/
   Matrix	b;		/* source matrix 2		*/
{
   unsigned	i,j,k;

   if (IsCompact(c))
      return M_COMPACT;

   if (c == a || c == b)
      return M_NOOVERWRITE;

   if (Mrows(a) != Mrows(c) || Mcols(b) != Mcols(c) || Mcols(a) != Mrows(b))
      return M_SIZEMISMATCH;
   
   ZeroMatrix (c);

   for (i = 1 ; i <= Mrows(a) ; i++) 
      for (j = 1 ; j <= Mcols(b) ; j++) 
         for (k = 1 ; k <= Mcols(a) ; k++)
            sdata(c,i,j) += mdata(a,i,k) * mdata(b,k,j);

   return 0;
}

int AddMatrices (c, a, b)	/* c = a + b			*/
   Matrix	c;		/* destination matrix		*/
   Matrix	a;		/* source matrix 1		*/
   Matrix	b;		/* source matrix 2		*/
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
         sdata(c,i,j) = mdata(a, i, j) + mdata(b, i, j);

   return 0;
}

int SubtractMatrices (c, a, b)	/* c = a - b 			*/
   Matrix	c;		/* destination matrix		*/
   Matrix	a;		/* source matrix 1		*/
   Matrix	b;		/* source matrix 2		*/
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
         sdata(c,i,j) = mdata(a, i, j) - mdata(b, i, j);

   return 0;
}

int ModMatrices (c, a, b)	/* c = a % b			*/
   Matrix	c;		/* destination matrix		*/
   Matrix	a;		/* source matrix 1		*/
   Matrix	b;		/* source matrix 2		*/
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
         sdata(c,i,j) = fmod (mdata(a, i, j), mdata(b, i, j));

   return 0;
}

int Saxpy (c, a, b, alpha)	/* c = b + alpha*a		*/	
   Matrix	c;		/* destination vector		*/
   Matrix	a;		/* source 1 vector		*/
   Matrix	b;		/* source 2 vector		*/
   double	alpha;		/* scale factor			*/
{
   unsigned	i;

   if (IsCompact(c))
      return M_COMPACT;

   if (Mcols(a) != 1 || Mcols(b) != 1 || Mcols(c) != 1)
      return M_SIZEMISMATCH;

   if (Mrows(a) != Mrows(b) || Mrows(b) != Mrows(c))
      return M_SIZEMISMATCH;

   for (i = 1 ; i <= Mrows(a) ; i++)
      sdata(c,i,1) = alpha*mdata(a, i, 1) + mdata(b, i, 1);

   return 0;
}

int Gaxpy (c, a, b, A)		/* c = b + Aa 			*/
   Matrix	c;		/* destination vector		*/
   Matrix	a;		/* source 1 vector		*/
   Matrix	b;		/* source 2 vector		*/
   Matrix	A;		/* source matrix		*/
{
   unsigned	i,j;

   if (IsCompact(c))
      return M_COMPACT;

   if (a == c)
      return M_NOOVERWRITE;

   if (Mcols(a) != 1 || Mcols(b) != 1 || Mcols(c) != 1)
      return M_SIZEMISMATCH;

   if (Mrows(a) != Mcols(A) || Mrows(b) != Mrows(A) || Mrows(c) != Mrows(b))
      return M_SIZEMISMATCH;

   for (i = 1 ; i <= Mrows(b) ; i++)
      sdata(c,i,1) = mdata(b, i, 1);

   for (i = 1 ; i <= Mcols(A) ; i++)
      for (j = 1 ; j <= Mrows(A) ; j++)
         sdata(c,j,1) += mdata(a, i, 1) * mdata(A, j, i); 

   return 0;
}

int ScaleMatrix(b, a, factor, offset)	/* b(i,j) = factor*a(i,j) + offset  */
   Matrix	b;			/* destination matrix		    */
   Matrix	a;			/* source matrix		    */
   double	factor;			/* multiplicative scale factor	    */
   double	offset;			/* additive offset		    */
{
   unsigned	i,j;

   if (IsCompact(b))
      return M_COMPACT;

   if (Mrows(a) != Mrows(b) || Mcols(a) != Mcols(b))
      return M_SIZEMISMATCH;
 
   if (factor == 1) {
      for (i = 1 ; i <= Mrows(a) ; i++)
         for (j = 1 ; j <= Mcols(a) ; j++)
            sdata(b,i,j) = mdata(a, i, j) + offset;
   }
   else if (offset == 0) {
      for (i = 1 ; i <= Mrows(a) ; i++)
         for (j = 1 ; j <= Mcols(a) ; j++)
            sdata(b,i,j) = mdata(a, i, j)*factor;
   }
   else { 
      for (i = 1 ; i <= Mrows(a) ; i++)
         for (j = 1 ; j <= Mcols(a) ; j++)
            sdata(b,i,j) = mdata(a, i, j)*factor + offset;
   }

   return 0;
}

int SqrtMatrix(b, a)		/* b(i,j) = sqrt(a(i,j))        */
   Matrix	b;		/* destination matrix	   	*/
   Matrix	a;		/* source matrix	        */
{
   unsigned	i,j;

   if (IsCompact(b))
      return M_COMPACT;

   if (Mrows(a) != Mrows(b) || Mcols(a) != Mcols(b))
      return M_SIZEMISMATCH;
 
    for (i = 1 ; i <= Mrows(a) ; i++)
       for (j = 1 ; j <= Mcols(a) ; j++)
          sdata(b,i,j) = sqrt (fabs (mdata(a, i, j)));

   return 0;
}

int DotBProduct(x, a, b)	/* x = aTb			*/
   double	*x;		/* pointer to result location	*/
   Matrix	a;		/* source vector (row) 1	*/
   Matrix	b;		/* source vector 2		*/
{
   unsigned	i;
   double	result;

   if (Mrows(a) != 1 || Mcols(b) != 1 || Mcols(a) != Mrows(b))
      return M_SIZEMISMATCH;

   result = 0;
   for (i = 1 ; i <= Mcols(a) ; i++)
      result += mdata(a,1,i)*mdata(b,i,1);

   *x = result;
   return 0;
}

int TransposeMatrix(b, a)	/* b = aT			*/
   Matrix	b;		/* destination matrix		*/
   Matrix	a;		/* source matrix		*/
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
         sdata(b,j,i) = mdata(a, i, j);

   return 0;
}

int CompareEQMatrices (c, a, b)	/* c = (a == b)		*/
   Matrix	c;			/* destination matrix	*/
   Matrix	a;			/* first RHS matrix	*/
   Matrix	b;			/* second RHS matrix	*/
{
   unsigned	i, j;
  
   if (IsCompact(c))
      return M_COMPACT;

   if (Mrows(a) != Mrows(b) || Mrows(b) != Mrows(c))
      return M_SIZEMISMATCH;

   if (Mcols(a) != Mcols(b) || Mcols(b) != Mcols(c))
      return M_SIZEMISMATCH;

   for (i = 1 ; i <= Mrows(a) ; i++)
      for (j = 1 ; j <= Mcols(a) ; j++)
         sdata(c,i,j) = (mdata(a, i, j) == mdata(b, i, j));

   return 0;
}

int CompareNEQMatrices (c, a, b)	/* c = (a != b)		*/
   Matrix	c;			/* destination matrix	*/
   Matrix	a;			/* first RHS matrix	*/
   Matrix	b;			/* second RHS matrix	*/
{
   unsigned	i, j;
  
   if (IsCompact(c))
      return M_COMPACT;

   if (Mrows(a) != Mrows(b) || Mrows(b) != Mrows(c))
      return M_SIZEMISMATCH;

   if (Mcols(a) != Mcols(b) || Mcols(b) != Mcols(c))
      return M_SIZEMISMATCH;

   for (i = 1 ; i <= Mrows(a) ; i++)
      for (j = 1 ; j <= Mcols(a) ; j++)
         sdata(c,i,j) = (mdata(a, i, j) != mdata(b, i, j));

   return 0;
}

int CompareGTMatrices (c, a, b)	/* c = (a > b)		*/
   Matrix	c;			/* destination matrix	*/
   Matrix	a;			/* first RHS matrix	*/
   Matrix	b;			/* second RHS matrix	*/
{
   unsigned	i, j;
  
   if (IsCompact(c))
      return M_COMPACT;

   if (Mrows(a) != Mrows(b) || Mrows(b) != Mrows(c))
      return M_SIZEMISMATCH;

   if (Mcols(a) != Mcols(b) || Mcols(b) != Mcols(c))
      return M_SIZEMISMATCH;

   for (i = 1 ; i <= Mrows(a) ; i++)
      for (j = 1 ; j <= Mcols(a) ; j++)
         sdata(c,i,j) = (mdata(a, i, j) > mdata(b, i, j));

   return 0;
}

int CompareLTMatrices (c, a, b)	/* c = (a < b)		*/
   Matrix	c;			/* destination matrix	*/
   Matrix	a;			/* first RHS matrix	*/
   Matrix	b;			/* second RHS matrix	*/
{
   unsigned	i, j;
  
   if (IsCompact(c))
      return M_COMPACT;

   if (Mrows(a) != Mrows(b) || Mrows(b) != Mrows(c))
      return M_SIZEMISMATCH;

   if (Mcols(a) != Mcols(b) || Mcols(b) != Mcols(c))
      return M_SIZEMISMATCH;

   for (i = 1 ; i <= Mrows(a) ; i++)
      for (j = 1 ; j <= Mcols(a) ; j++)
         sdata(c,i,j) = (mdata(a, i, j) < mdata(b, i, j));

   return 0;
}

int CompareLTEMatrices (c, a, b)	/* c = (a <= b)		*/
   Matrix	c;			/* destination matrix	*/
   Matrix	a;			/* first RHS matrix	*/
   Matrix	b;			/* second RHS matrix	*/
{
   unsigned	i, j;
  
   if (IsCompact(c))
      return M_COMPACT;

   if (Mrows(a) != Mrows(b) || Mrows(b) != Mrows(c))
      return M_SIZEMISMATCH;

   if (Mcols(a) != Mcols(b) || Mcols(b) != Mcols(c))
      return M_SIZEMISMATCH;

   for (i = 1 ; i <= Mrows(a) ; i++)
      for (j = 1 ; j <= Mcols(a) ; j++)
         sdata(c,i,j) = (mdata(a, i, j) <= mdata(b, i, j));

   return 0;
}

int CompareGTEMatrices (c, a, b)	/* c = (a >= b)		*/
   Matrix	c;			/* destination matrix	*/
   Matrix	a;			/* first RHS matrix	*/
   Matrix	b;			/* second RHS matrix	*/
{
   unsigned	i, j;
  
   if (IsCompact(c))
      return M_COMPACT;

   if (Mrows(a) != Mrows(b) || Mrows(b) != Mrows(c))
      return M_SIZEMISMATCH;

   if (Mcols(a) != Mcols(b) || Mcols(b) != Mcols(c))
      return M_SIZEMISMATCH;

   for (i = 1 ; i <= Mrows(a) ; i++)
      for (j = 1 ; j <= Mcols(a) ; j++)
         sdata(c,i,j) = (mdata(a, i, j) >= mdata(b, i, j));

   return 0;
}

int PrintMatrix (m, fp)		/* print matrix m to file fp	*/
   Matrix	m;		/* matrix to print		*/
   FILE		*fp;		/* file pointer for output	*/
{
   double       val;
   unsigned	start, end;
   unsigned     i, j;
  
   if (fp == NULL)
      fp = stdout; 

   for (start = 1 ; start <= Mcols(m) ; start += 7) {
      end = start + 6 > Mcols(m) ? Mcols(m) : start + 6;
   
      if (Mcols(m) > 7)
         fprintf (fp, "\nColumns %d through %d\n\n", start, end);
      else
         fprintf (fp, "\n");

      for (j = 1 ; j <= Mrows(m) ; j++) {
         for (i = start ; i <= end ; i++) {
            val = mdata(m,j,i);
            if (fabs(val) < PRINT_TOL)
               val = 0.0;

            fprintf (fp, "%10.4g ", val);
         }
         fprintf (fp, "\n");
      }
   }
     
   fprintf (fp, "\n");
   
   return 0;
}

int PrintMatrixSubsection (m, sr, sc, er, ec, fp) /* print matrix m to fp    */
   Matrix	m;				  /* matrix to print	     */
   unsigned	sr;				  /* starting row	     */
   unsigned	sc;				  /* starting column	     */
   unsigned	er;				  /* ending row	     	     */
   unsigned	ec;				  /* ending column	     */
   FILE		*fp;				  /* file pointer for output */
{
   double       val;
   unsigned     i, j;

   if (fp == NULL)
      fp = stdout;

   if (sr == 0)
      sr = 1;
   if (sc == 1)
      sc = 1;
   if (er == 0)
      er = Mrows(m);
   if (ec == 0)
      ec = Mcols(m);

   for (i = sr ; i <= er ; i ++) {
      for (j = sc ; j <= ec ; j ++) {
         val = mdata(m, i, j);
         if (fabs(val) < PRINT_TOL)
            val = 0.0;

         fprintf (fp, "%9.4g ", val);
         if (j % 8 == 0)
            fprintf (fp, "\n> ");
      }
      fprintf (fp, "\n");
   }

   fprintf (fp, "\n");
   
   return 0;
}
