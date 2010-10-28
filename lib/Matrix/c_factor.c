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
# include <stdlib.h>
# include "matrix.h"
# include "cmatrix.h"
# include "error.h"

int InvertComplexMatrix (ComplexMatrix b, const ComplexMatrix a, const Matrix p)
{
   unsigned		i, j;
   unsigned		n;
   ComplexMatrix	work;
   static complex	one = {1.0, 0.0};

   if (IsCompact(b))
      return M_COMPACT;

   if (Mrows(a) != Mcols(a))
      return M_NOTSQUARE;

   if (Mrows(a) != Mrows(p) || Mcols(p) != 1)
      return M_SIZEMISMATCH; 

   if (Mrows(a) != Mrows(b) || Mcols(a) != Mcols(b))
      return M_SIZEMISMATCH;

   if (a == b) 
      return M_NOOVERWRITE;

   n = Mrows(a);
   work = CreateComplexColumnVector (n);

   for (i = 1 ; i <= n ; i++) {
      ZeroComplexMatrix (work);
      sdata(work, i, 1) = one;
      LUBackSolveComplexMatrix(work, a, work, p);
      for (j = 1 ; j <= n ; j++)
         sdata(b,j,i) = cmdata(work,j,1);
   }
   
   DestroyComplexMatrix (work);
   return 0;
}

int LUFactorComplexMatrix (ComplexMatrix b, const ComplexMatrix a, const Matrix p, int *info)
{
   complex	t;
   unsigned	i, j, k;
   unsigned	n;
   double	temp;
   double	max;
   int		status;
   unsigned	mu;

   if (IsCompact(b))
      return M_COMPACT;

   if (Mrows(a) != Mcols(a))
      return M_NOTSQUARE;

   if (Mrows(a) != Mrows(p))
       return M_SIZEMISMATCH;

   if (Mrows(a) != Mrows(b) || Mcols(a) != Mcols(b))
      return M_SIZEMISMATCH;

   if (a != b) {
      status = CopyComplexMatrix (b, a);
      if (status)
         return status;
   }

   *info = 0;

   n = Mrows(b);

   for (j = 1 ; j <= n ; j++)
      sdata(p, j, 1) = j;

   for (j = 1 ; j <= n ; j++) {

      for (k = 1 ; k <= j-1 ; k++) { 
         mu = mdata(p, k, 1);
         if (mu != k) {
            t = cmdata(b, k, j);
            sdata(b,k,j) = cmdata(b,mu,j);
            sdata(b,mu,j) = t;
         }
      }

      for (k = 1 ; k <= j-1 ; k++) 
         for (i = k+1 ; i <= j-1 ; i++) 
            sdata(b,i,j) = sub(cmdata(b,i,j), mult(cmdata(b,i,k),cmdata(b,k,j)));

      for (k = 1 ; k <= j-1 ; k++)
         for (i = j ; i <= n ; i++)
            sdata(b,i,j) = sub(cmdata(b,i,j), mult(cmdata(b,i,k),cmdata(b,k,j)));

      mu = j;
      max = modulus(cmdata(b,j,j));
      for (k = j ; k <= n ; k++) {
         if ((temp = modulus (cmdata(b,k,j))) > max) {
            max = temp;
            mu = k;
         }
      } 

      sdata(p,j,1) = mu;

      if (j != mu) {
         for (k = 1 ; k <= j ; k++) {
            t = cmdata(b, j, k);
            sdata(b,j,k) = cmdata(b,mu,k);
            sdata(b,mu,k) = t;
         }
      }

      if (re(cmdata(b,j,j)) != 0 || im(cmdata(b,j,j)) != 0) {
         t = cmdata(b, j, j);
         for (i = j+1 ; i <= n ; i++)
            sdata(b,i,j) = cdiv(cmdata(b, i, j),t);
      }
      else
         *info = j;
   }
                  
   return 0; 
}

int LUBackSolveComplexMatrix (ComplexMatrix c, ComplexMatrix a, ComplexMatrix b, Matrix p)
{
   unsigned	i, k;
   unsigned	n;
   int		mu;
   complex	t;
   int		status;

   if (Mrows(a) != Mcols(a))
      return M_NOTSQUARE;

   if (Mcols(b) != 1 || Mcols(p) != 1 || Mcols(c) != 1)
      return M_SIZEMISMATCH;

   if (Mrows(a) != Mrows(p) || Mrows(a) != Mrows(b) || Mrows(b) != Mrows(c))
      return M_SIZEMISMATCH; 

   if (b != c) {
      status = CopyComplexMatrix (c, b);
      if (status)
         return status;
   }

   n = Mrows(a);

	/*
	 * pivot the RHS vector to form x = Pb
	 */

   for (i = 1 ; i <= n ; i++) {
      mu = (int) mdata(p, i, 1);
      if (mu != i) {
         t = cmdata(c, i, 1);
         sdata(c,i,1) = cmdata(c, mu, 1);
         sdata(c,mu,1) = t;
      }
   }

	/*
	 * Solve Ly = x and store y in c
	 */

   for (k = 1 ; k <= n ; k++) {
      if (re(cmdata(c,k,1)) != 0 || im(cmdata(c,k,1)) != 0) {
         for (i = k+1 ; i <= n ; i++) 
            sdata(c,i,1) = sub(cmdata(c, i, 1), mult(cmdata(c,k,1),cmdata(a,i,k)));
      }
   }

	/*
	 * Solve Uc = y
	 */

   for (k = n ; k >= 1 ; k--) {
      if (re(cmdata(c, k, 1)) != 0 || im(cmdata(c,k,1)) != 0) 
         sdata(c,k,1) = cdiv(cmdata(c,k,1),cmdata(a, k, k));

      for (i = 1 ; i <= k-1 ; i++)
        sdata(c,i,1) = sub(cmdata(c,i,1), mult(cmdata(c,k,1),cmdata(a,i,k)));
   }
   
   return 0;
}

int DeterminantComplexMatrix(complex *result, const ComplexMatrix a, const Matrix p)
{
   unsigned	i;
   complex	x;
   unsigned	n;

   if (Mrows(a) != Mcols(a))
      return M_NOTSQUARE;

   if (Mrows(a) != Mrows(p) || Mcols(p) != 1)
      return M_SIZEMISMATCH;

   n = Mrows(a);

   x.r = 1.0;
   for (i = 1 ; i <= n ; i++) {
      if ((int) mdata(p, i, 1) != i) 
         x = negate(x);

      x = mult(x,cmdata(a, i, i));
   }

   *result = x;
         
   return 0;
}

int InvertCroutComplexMatrix (ComplexMatrix b, const ComplexMatrix a, unsigned int col)
{
   static complex	one = {1.0, 0.0};

   if (IsCompact(b))
      return M_COMPACT;

   if (Mrows(a) != Mcols(a))
      return M_NOTSQUARE;

   if (Mrows(a) != Mrows(b) || Mcols(b) != 1)
      return M_SIZEMISMATCH; 

   ZeroComplexMatrix (b);
   sdata(b, col, 1) = one;
   CroutBackSolveComplexMatrix(a, b);
   
   return 0;
}

int CroutFactorComplexMatrix (ComplexMatrix A)
{
   unsigned     j,jj,jjlast,jcolht,
          	istart,ij,ii,i,
          	icolht,iilast,
          	length,jtemp,jlngth;
   complex	dot, temp;
   unsigned	n, k;

   if (IsFull(A))
      return M_NOTCOMPACT;
 
   if (Mrows(A) != Mcols(A))
      return M_NOTSQUARE;
  
   if (IsSubsection (A))
      return M_SUBSECTION;

   n = Mrows(A);

   jj = 0;
   for (j = 1; j <= n; j++) {

      jjlast = jj;
      jj = A -> diag [j];
      jcolht = jj - jjlast;

      if (jcolht > 2) {
         
         istart = j - jcolht + 2;
         ij = jjlast + 2;
         ii = A -> diag [istart-1];

         for (i = istart; i <= j - 1 ; i++) {

            iilast = ii;
            ii = A -> diag [i];
            icolht = ii - iilast;
            jlngth = i - istart + 1;
            if (icolht - 1  < jlngth) 
               length = icolht - 1;
            else
               length = jlngth;
            
            if (length > 0) {
               dot.r = 0.0;
               dot.i = 0.0;
               for (k = 0 ; k < length ; k++)
                  dot = add(dot, mult(A -> data [ii-length+k][1],A -> data[ij-length+k][1]));

               A -> data [ij][1] = sub(A -> data [ij][1],dot);
            }

            ij++;
         }
      }

      if (jcolht >= 2) {

         jtemp = j - jj;
         for (ij = jjlast+1 ; ij <= jj-1 ; ij++) {

            ii = A -> diag [jtemp + ij];
           
            if (re(A -> data [ii][1]) != 0.0 || im(A -> data [ii][1]) != 0) {
               temp = A -> data [ij][1];
               A -> data [ij][1] = cdiv(temp,A -> data [ii][1]);
               A -> data [jj][1] = sub(A -> data [jj][1], mult(temp,A -> data [ij][1]));
            }
         }
      }
   }

   return 0;
}

int CroutBackSolveComplexMatrix (const ComplexMatrix A, ComplexMatrix b)
{
   unsigned	 jj,j,jjlast,
		 jcolht,jjnext,
          	 istart,jtemp,i;
   complex	 Ajj; 
   unsigned	 n, k;
   complex	 dot;

   if (IsFull(A))
      return M_NOTCOMPACT;

   if (Mrows(A) != Mcols(A))
      return M_NOTSQUARE;
    
   if (Mrows(A) != Mrows(b))
      return M_SIZEMISMATCH;

   if (IsSubsection (A) || IsSubsection (b))
      return M_SUBSECTION;

   n = Mrows(A);

   jj = 0;
   for (j = 1 ; j <= n ; j++) {

      jjlast = jj;
      jj = A -> diag [j];
      jcolht = jj - jjlast;

      if (jcolht > 1) {
         dot.r = 0;
         dot.i = 0;
         for (k = 0 ; k < jcolht-1 ; k++)
            dot = add(dot, mult(A -> data [jjlast+1+k][1],b -> data [j-jcolht+1+k][1]));

         b -> data [j][1] = sub(b -> data [j][1],dot);
      }
   }

   for (j = 1 ; j <= n ; j++) {
      Ajj = A -> data [A -> diag[j]][1];
      if (re(Ajj) != 0.0 || im(Ajj) != 0)
         b -> data [j][1] = cdiv(b -> data [j][1], Ajj);
   }

   if (n == 1)
      return 0;

   jjnext = A -> diag [n];

   for (j = n ; j >= 2 ; j--) {

      jj = jjnext;
      jjnext = A -> diag [j-1];
      jcolht = jj - jjnext;
      if (jcolht > 1) {

          istart = j - jcolht + 1;
          jtemp = jjnext - istart + 1;

          for (i = istart ; i <= j-1 ; i++) 
             b -> data [i][1] = sub(b -> data [i][1], 
                                    mult(A -> data [jtemp + i][1],b -> data[j][1]));
      }
   }

   return 0;
}
