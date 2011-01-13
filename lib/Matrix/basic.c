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

# include <stdlib.h>
# include <time.h>
# include <math.h>
# include <stdio.h>
# include "matrix.h"

# define PRINT_TOL 1.0e-12

int ZeroMatrix (Matrix a)
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

int MirrorMatrix (Matrix a)
{
   unsigned	i, j;

   if (IsCompact (a))
      return M_COMPACT;

   for (i = 2 ; i <= Mrows(a) ; i++)
      for (j = 1 ; j < i ; j++)
         sdata (a, i, j) = mdata (a, j, i);

   return 0;
}

int CopyMatrix (Matrix b, Matrix a)
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

int IdentityMatrix (Matrix a)
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

int RandomMatrix (Matrix a, int seed)
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

int MultiplyMatrices (Matrix c, Matrix a, Matrix b)
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

int AddMatrices (Matrix c, Matrix a, Matrix b)
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

int SubtractMatrices (Matrix c, Matrix a, Matrix b)
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

int ModMatrices (Matrix c, Matrix a, Matrix b)
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

int Saxpy (Matrix c, Matrix a, Matrix b, double alpha)
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

int Gaxpy (Matrix c, Matrix a, Matrix b, Matrix A)
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

int ScaleMatrix(Matrix b, Matrix a, double factor, double offset)
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

int SqrtMatrix(Matrix b, Matrix a)
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

int DotBProduct(double *x, Matrix a, Matrix b)
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

int TransposeMatrix(Matrix b, Matrix a)
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

int CompareEQMatrices (Matrix c, Matrix a, Matrix b)
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

int CompareNEQMatrices (Matrix c, Matrix a, Matrix b)
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

int CompareGTMatrices (Matrix c, Matrix a, Matrix b)
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

int CompareLTMatrices (Matrix c, Matrix a, Matrix b)
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

int CompareLTEMatrices (Matrix c, Matrix a, Matrix b)
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

int CompareGTEMatrices (Matrix c, Matrix a, Matrix b)
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

int PrintMatrix (Matrix m, FILE *fp)
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

int PrintMatrixSubsection (Matrix m, unsigned int sr, unsigned int sc, unsigned int er, unsigned int ec, FILE *fp)
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
