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
# include <stdlib.h>
# include <string.h>
# include "matrix.h"
# include "cmatrix.h"
# include "error.h"

complex cmdata (ComplexMatrix A, unsigned int row, unsigned int col)
{
   unsigned	height;
   unsigned	temp;

   if (IsFull (A))
      return A -> data [row][col];
   else if (IsCompact (A)) {
      if (row > col) {
         temp = col;
         col = row;
         row = temp;
      }

      if (col == 1)
         height = 1;
      else
         height = A -> diag [col] - A -> diag [col - 1];

      if (row > col - height)
         return A -> data [A -> diag [col] + row - col][1]; 
      else
         return zero();
   }

   return zero();
}

static ComplexMatrix
CreateSubsectionComplexMatrix(ComplexMatrix a, 
                              unsigned sr, unsigned sc,
                              unsigned er, unsigned ec)
{
   ComplexMatrix	b;
   unsigned	i;
   ComplexMatrix	root;

   if (er > a -> nrows || ec > a -> ncols || sc < 1 || sr < 1)
      return NULL;

   if (IsCompact (a))
      return NULL;

   b = (struct complex_matrix *) malloc (sizeof (struct complex_matrix));
   if (b == NULL)
	Fatal ("unable to allocate subsection matrix");

   b -> nrows = er - sr + 1;
   b -> ncols = ec - sc + 1;

   b -> data = (complex **) malloc (sizeof (complex *) * b -> nrows);
   if (b -> data == NULL)
	Fatal ("unable to allocate subsection matrix");
   b -> data --;

   for (i = 1 ; i <= b -> nrows ; i++)
      b -> data [i] = &(a -> data [i + sr - 1][sc - 1]); 

   b -> refcount = 0;
   b -> size = 0;

   root = a;
   while (root -> parent)
      root = root -> parent;

   b -> parent = root;
   root -> refcount ++;

   return b;
}

ComplexMatrix CreateFullComplexMatrix (unsigned int rows, unsigned int cols)
{
   unsigned	i;
   ComplexMatrix	m;

   m = (struct complex_matrix *) malloc (sizeof (struct complex_matrix));
   if (m == NULL)
	Fatal ("unable to allocate full matrix");

   m -> data = (complex **) malloc (sizeof (complex *) * rows);
   if (m -> data == NULL)
	Fatal ("unable to allocate full matrix");
   m -> data --;

   m -> data [1] = (complex *) malloc (sizeof (complex) * rows * cols);
   if (m -> data [1] == NULL)
	Fatal ("unable to allocate full matrix");
   m -> data [1] --;
   for (i = 2 ; i <= rows ; i++)
      m -> data [i] = m -> data [i-1] + cols;

   m -> nrows = rows;
   m -> ncols = cols;
   m -> size = 0;
   m -> refcount = 1;
   m -> parent = NULL;

   return m;
}

ComplexMatrix CreateComplexRowVector (unsigned int size)
{
   return CreateFullComplexMatrix (1, size);
}

ComplexMatrix CreateComplexColumnVector (unsigned int size)
{
   return CreateFullComplexMatrix (size, 1);
}

void DestroyComplexMatrix (ComplexMatrix m)
{
   if (m -> parent != NULL) {
      m -> parent -> refcount --;
 
      if (m -> parent -> refcount == 0)
         DestroyComplexMatrix (m -> parent);
      
      m -> data ++;
      free (m -> data);
      free (m); 

      return;
   } 
   else if (-- m -> refcount)
      return;

   m -> data [1] ++;
   free (m -> data [1]);

   m -> data ++;
   free (m -> data);

   if (IsCompact (m)) {
      m -> diag ++;
      free (m -> diag);
   }
   
   free (m);
}

ComplexMatrix CreateCompactComplexMatrix (unsigned int rows, unsigned int cols, unsigned int size, unsigned int *diag)
{
   ComplexMatrix	A;

   A = CreateFullComplexMatrix (size, 1);

   A -> nrows = rows; 
   A -> ncols = cols;
   A -> size = size;
   A -> parent = NULL;

   if (diag != NULL) {
      A -> diag = (unsigned *) malloc (sizeof (unsigned) * rows);
      if (A -> diag == NULL)
         Fatal ("unable to create compact matrix");

      A -> diag --;

      memcpy ((void *) &(A -> diag[1]), (void *) &(diag[1]), 
              sizeof(unsigned)*rows);
   }

   return A;
}

ComplexMatrix CreateCopyComplexMatrix (ComplexMatrix a)
{
   ComplexMatrix	b;
   unsigned	size;
   unsigned	rows, cols;

   rows = a -> nrows;
   cols = a -> ncols;

   if (IsCompact(a)) {
      size = Msize(a)*sizeof(complex);
      b = CreateCompactComplexMatrix (rows, cols, Msize(a), NULL); 
      b -> diag = (unsigned *) malloc (sizeof(unsigned) * rows);
      if (b -> diag == NULL)
	Fatal ("unable to create compact matrix");
      b -> diag --;
      memcpy ((void *) &(b -> diag[1]), (void *) &(a -> diag[1]), 
              sizeof(unsigned)*rows);
   }
   else { 
      size = rows*cols*sizeof (complex);
      b = CreateFullComplexMatrix (rows, cols);
   }   

   memcpy ((void *) &(b -> data[1][1]),(void *) &(a -> data[1][1]), size);

   return b;
}

ComplexMatrix MakeFullFromCompactComplex (ComplexMatrix A)
{
   unsigned 		i,j;
   ComplexMatrix	B;

   B = CreateFullComplexMatrix (Mrows(A), Mcols(A));

   for (i = 1 ; i <= Mrows(A) ; i++) 
      for (j = 1 ; j <= Mcols(A) ; j++)
         B -> data [i][j] = cmdata(A, i, j);

   return B; 
}

ComplexMatrix MakeCompactFromFullComplex (ComplexMatrix A)
{
   unsigned	*diag;
   ComplexMatrix	compA;
   unsigned	i,j,k,
		curr_row;
   unsigned	rows,cols;
   unsigned	size;
   unsigned	height;

   if (IsCompact(A))
      return (ComplexMatrix) NullMatrix;

   if (!IsSquare(A))
      return (ComplexMatrix) NullMatrix;

   if (!IsSymmetricComplexMatrix(A))
      return (ComplexMatrix) NullMatrix;

   rows = Mrows (A);
   cols = Mcols (A);

   size = 0;
   for (i = 1 ; i <= rows ; i++) {
      if (re(cmdata(A,i,i)) == 0 && im(cmdata(A,i,i)) == 0) {
         size = 1;
         break;
      }
   }

   if (size)
      return (ComplexMatrix) NullMatrix;

   diag = (unsigned *) malloc (sizeof(unsigned) * cols);
   if (diag == NULL)
	Fatal ("unable to create compact matrix");

   diag--;
    
	/*
	 * determine the height of the columns and store in diag
	 */

   for (j = 1 ; j <= cols ; j++) {
      diag [j] = 0;
      for (i = 1 ; i <= rows; i++) {

         if ((re(cmdata(A,i,j)) != 0.0 || im(cmdata(A,i,j)) != 0)  && 
             i <= j && (j - (i-1)) > diag [j]) 

            diag [j] = j - (i-1);
      }
   }

   size = 0;
   for (i = 1 ; i <= cols ; i++) 
      size += diag [i];
   
   compA = CreateCompactComplexMatrix (rows, cols, size, NULL);

   diag [1] = 1;
   compA -> data [1][1] = cmdata (A, 1, 1);
  
   for (i = 2 ; i <= cols ; i++) {
      height = diag [i];
      diag [i] += diag [i-1];
      curr_row = i - height + 1 ;
      for (k = diag [i] - height + 1 ; k <= diag [i] ; k++) 
         compA -> data [k][1] = cmdata (A, curr_row++, i);
   }

   compA -> diag = diag;

   return compA;
} 
