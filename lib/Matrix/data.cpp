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

double mdata (const Matrix &A, unsigned int row, unsigned int col)
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

Matrix CreateFullMatrix (unsigned int rows, unsigned int cols)
{
   unsigned	i;

   Matrix m(new struct matrix);
   
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

matrix::~matrix()
{
    this -> data [1] ++;
    free (this -> data [1]);
    
    this -> data ++;
    free (this -> data);
}

Matrix CreateCompactMatrix (unsigned int rows, unsigned int cols, unsigned int size, const cvector1<unsigned> *diag)
{
   Matrix	A;

   A = CreateFullMatrix (size, 1);

   A -> nrows = rows; 
   A -> ncols = cols;
   A -> size = size;

   if (diag != NULL)
       A->diag = *diag;

   return A;
}

Matrix CreateCopyMatrix (const Matrix &a)
{
   Matrix	b;
   unsigned	size;
   unsigned	rows, cols;

   rows = a -> nrows;
   cols = a -> ncols;

   if (IsCompact(a)) {
      size = Msize(a)*sizeof(double);
      b = CreateCompactMatrix (rows, cols, Msize(a), NULL); 
      b -> diag = a -> diag;
   }
   else { 
      size = rows*cols*sizeof (double);
      b = CreateFullMatrix (rows, cols);
   }   

   memcpy ((void *) &(b -> data[1][1]),(void *) &(a -> data[1][1]), size);

   return b;
}

Matrix MakeFullFromCompact (const Matrix &A)
{
   unsigned 	i,j;
   Matrix	B;

   B = CreateFullMatrix (Mrows(A), Mcols(A));

   for (i = 1 ; i <= Mrows(A) ; i++) 
      for (j = 1 ; j <= Mcols(A) ; j++)
         B -> data [i][j] = mdata(A, i, j);

   return B; 
}

Matrix MakeCompactFromFull (const Matrix &A)
{
   Matrix	compA;
   unsigned	i,j,k,
		curr_row;
   unsigned	rows,cols;
   unsigned	size;
   unsigned	height;

   if (IsCompact(A))
       return Matrix();

   if (!IsSquare(A))
       return Matrix();

   if (!IsSymmetricMatrix(A))
       return Matrix();

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
       return Matrix();

	/*
	 * determine the height of the columns and store in diag
	 */

   cvector1<unsigned> diag(cols);

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

int ConvertRowColumn (unsigned int row, unsigned int col, const Matrix &a)
{
    unsigned	blanks, address;
    unsigned	height;
    unsigned	temp;

    if (row > col) {
       temp = col;
       col = row;
       row = temp;
    }

    if (col == 1)
       height = 1;
    else
       height = a->diag [col] - a->diag [col - 1];

    blanks = col - height;
    if (row > blanks) 
        address = a->diag [col] + row - col;
    else
        address = 0;

    return address;
} 
