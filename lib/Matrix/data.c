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
# include "error.h"

double mdata (Matrix A, unsigned int row, unsigned int col)
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
         return 0;
   }

   return 0;
}

Matrix CreateSubsectionMatrix (Matrix a, unsigned int sr, unsigned int sc, unsigned int er, unsigned int ec)
{
   Matrix	b;
   unsigned	i;
   Matrix	root;

   if (er > a -> nrows || ec > a -> ncols || sc < 1 || sr < 1)
      return NULL;

   if (IsCompact (a))
      return NULL;

   b = (struct matrix *) malloc (sizeof (struct matrix));
   if (b == NULL)
	Fatal ("unable to allocate subsection matrix");

   b -> nrows = er - sr + 1;
   b -> ncols = ec - sc + 1;

   b -> data = (double **) malloc (sizeof (double *) * b -> nrows);
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

Matrix CreateFullMatrix (unsigned int rows, unsigned int cols)
{
   unsigned	i;
   Matrix	m;

   m = (struct matrix *) malloc (sizeof (struct matrix));
   if (m == NULL) {
        fprintf (stderr,"failure point 1: r = %d, c = %d\n", rows, cols);
	Fatal ("unable to allocate full matrix");
   }

   m -> data = (double **) malloc (sizeof (double *) * rows);
   if (m -> data == NULL) {
        fprintf (stderr,"failure point 2: r = %d, c = %d\n", rows, cols);
	Fatal ("unable to allocate full matrix");
   }

   m -> data --;

   m -> data [1] = (double *) malloc (sizeof (double) * rows * cols);
   if (m -> data [1] == NULL) {
        fprintf (stderr,"failure point 3: r = %d, c = %d\n", rows, cols);
	Fatal ("unable to allocate full matrix");
   }

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

Matrix CreateRowVector (unsigned int size)
{
   return CreateFullMatrix (1, size);
}

Matrix CreateColumnVector (unsigned int size)
{
   return CreateFullMatrix (size, 1);
}

void DestroyMatrix (Matrix m)
{
   if (m -> parent != NULL) {
      m -> parent -> refcount --;
 
      if (m -> parent -> refcount == 0)
         DestroyMatrix (m -> parent);
      
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

Matrix CreateCompactMatrix (unsigned int rows, unsigned int cols, unsigned int size, unsigned int *diag)
{
   Matrix	A;

   A = CreateFullMatrix (size, 1);

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

Matrix CreateCopyMatrix (Matrix a)
{
   Matrix	b;
   unsigned	size;
   unsigned	rows, cols;

   rows = a -> nrows;
   cols = a -> ncols;

   if (IsCompact(a)) {
      size = Msize(a)*sizeof(double);
      b = CreateCompactMatrix (rows, cols, Msize(a), NULL); 
      b -> diag = (unsigned *) malloc (sizeof(unsigned) * rows);
      if (b -> diag == NULL)
	Fatal ("unable to create compact matrix");
      b -> diag --;
      memcpy ((void *) &(b -> diag[1]), (void *) &(a -> diag[1]), 
              sizeof(unsigned)*rows);
   }
   else { 
      size = rows*cols*sizeof (double);
      b = CreateFullMatrix (rows, cols);
   }   

   memcpy ((void *) &(b -> data[1][1]),(void *) &(a -> data[1][1]), size);

   return b;
}

Matrix MakeFullFromCompact (Matrix A)
{
   unsigned 	i,j;
   Matrix	B;

   B = CreateFullMatrix (Mrows(A), Mcols(A));

   for (i = 1 ; i <= Mrows(A) ; i++) 
      for (j = 1 ; j <= Mcols(A) ; j++)
         B -> data [i][j] = mdata(A, i, j);

   return B; 
}

Matrix MakeCompactFromFull (Matrix A)
{
   unsigned	*diag;
   Matrix	compA;
   unsigned	i,j,k,
		curr_row;
   unsigned	rows,cols;
   unsigned	size;
   unsigned	height;

   if (IsCompact(A))
      return NullMatrix;

   if (!IsSquare(A))
      return NullMatrix;

   if (!IsSymmetricMatrix(A))
      return NullMatrix;

   rows = Mrows (A);
   cols = Mcols (A);

   size = 0;
   for (i = 1 ; i <= rows ; i++) {
      if (mdata(A,i,i) == 0) {
         size = 1;
         break;
      }
   }

   if (size)
      return NullMatrix;

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

         if (mdata(A,i,j) != 0.0  && i <= j && (j - (i-1)) > diag [j]) 
            diag [j] = j - (i-1);
      }
   }

   size = 0;
   for (i = 1 ; i <= cols ; i++) 
      size += diag [i];
   
   compA = CreateCompactMatrix (rows, cols, size, NULL);

   diag [1] = 1;
   compA -> data [1][1] = mdata (A, 1, 1);
  
   for (i = 2 ; i <= cols ; i++) {
      height = diag [i];
      diag [i] += diag [i-1];
      curr_row = i - height + 1 ;
      for (k = diag [i] - height + 1 ; k <= diag [i] ; k++) 
         compA -> data [k][1] = mdata (A, curr_row++, i);
   }

   compA -> diag = diag;

   return compA;
} 

int ConvertRowColumn (unsigned int row, unsigned int col, Matrix a)
{
    unsigned	blanks, address;
    unsigned	height;
    unsigned	temp;
    unsigned	*diag;

    diag = a -> diag;

    if (row > col) {
       temp = col;
       col = row;
       row = temp;
    }

    if (col == 1)
       height = 1;
    else
       height = diag [col] - diag [col - 1];

    blanks = col - height;
    if (row > blanks) 
        address = diag [col] + row - col;
    else
        address = 0;

    return address;
} 
