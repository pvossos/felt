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

# ifndef _CMATRIX_H
# define _CMATRIX_H

# include <stdio.h>		/* for FILE definition			*/
# include "complex.h"
# include "status.h"

#ifdef __cplusplus
extern "C" {
#endif 

/*----------------------------------------------------------------------*/

typedef struct complex_matrix *ComplexMatrix;

struct complex_matrix {
   unsigned	nrows;		/* number of rows 			 */
   unsigned	ncols;		/* number of columns			 */
   complex	**data;		/* matrix data				 */
   unsigned	*diag;		/* diagonal addresses for compact column */
   unsigned	size;		/* actual size of compact storage	 */
   unsigned	refcount;	/* count of children			 */
   ComplexMatrix parent;	/* parent of possible subsection	 */
};

# define rdata(m,y,x)   (((m) -> data [(y)][(x)]).r)
# define idata(m,y,x)   (((m) -> data [(y)][(x)]).i)
	
typedef ComplexMatrix ComplexVector;

	/*
	 * prototypes for DATA manipulation routines
	 */

/*!
  \param A matrix to fetch data from
  \param row row index
  \param col column index
*/
complex cmdata (const ComplexMatrix A, unsigned int row, unsigned int col);

/*!
  \param rows number of rows
  \param cols number of columns
*/
ComplexMatrix CreateFullComplexMatrix (unsigned int rows, unsigned int cols);

/*!
  \param size vector length
*/
ComplexMatrix CreateComplexRowVector (unsigned int size);

/*!
  \param size vector length
*/
ComplexMatrix CreateComplexColumnVector (unsigned int size);

/*!
  \param m matrix to free
*/
void DestroyComplexMatrix (ComplexMatrix m);

/*!
  \param rows number of rows
  \param cols number of columns
  \param size actual size of storage
  \param diag diagonial index array
 */
ComplexMatrix CreateCompactComplexMatrix (unsigned int rows, unsigned int cols, unsigned int size, unsigned int *diag);

/*!
  \param a matrix to copy data from
*/
ComplexMatrix CreateCopyComplexMatrix (const ComplexMatrix a);

/*!
  \param A compact matrix to expand
*/
ComplexMatrix MakeFullFromCompactComplex (const ComplexMatrix A);

/*!
  \param A full matrix to compact
*/

ComplexMatrix MakeCompactFromFullComplex (const ComplexMatrix A);

	/*
	 * prototypes for the BASIC matrix routines
	 */

/*!
  \brief A = 0 
  \param a the Matrix to fill with zeros
 */
int ZeroComplexMatrix (ComplexMatrix a);


/*!
  \brief B = A
  \param a the source Matrix
  \param b the destination Matrix
 */
int CopyComplexMatrix (ComplexMatrix b, const ComplexMatrix a);

/*!
  \brief a(i,j) = rand()
  \param a Matrix to randomize
  \param seed optional seed
 */
int RandomComplexMatrix (ComplexMatrix a, int seed);

/*!
  \param a the Matrix to mirror
 */
int MirrorComplexMatrix (ComplexMatrix a);

/*!
  \brief c = a * b
  \param c destination Matrix
  \param a source matrix 1
  \param b source matrix 2
 */
int MultiplyComplexMatrices (ComplexMatrix c, const ComplexMatrix a, const ComplexMatrix b);

/*!
  \brief c = a + b
  \param c destination Matrix
  \param a source Matrix 1
  \param b source Matrix 2
 */
int AddComplexMatrices (ComplexMatrix c, const ComplexMatrix a, const ComplexMatrix b);

/*!
  \brief c = a - b
  \param c destination Matrix
  \param a source Matrix 1
  \param b source Matrix 2
*/
int SubtractComplexMatrices (ComplexMatrix c, const ComplexMatrix a, const ComplexMatrix b);

/*!
  \brief b(i,j) = factor*a(i,j) + offset
  \param b destination matrix
  \param a source matrix
  \param factor multiplicative scale factor
  \param offset additive offset
 */
int ScaleComplexMatrix(ComplexMatrix b, const ComplexMatrix a, complex factor, complex offset);

/*!
  \brief b = |a|
  \param b real destination matrix
  \param a complex source matrix
*/
int ModulusComplexMatrix(Matrix b, const ComplexMatrix a);

/*!
  \brief b = aT
  \param b destination matrix
  \param a source matrix
*/
int TransposeComplexMatrix(ComplexMatrix b, const ComplexMatrix a);

/*!
  \brief print matrix m to file fp
  \param m matrix to print
  \param fp file pointer for output
*/
int PrintComplexMatrix (const ComplexMatrix m, FILE *fp);

	/*
	 * protoypes for the FACTORization machine
	 */

/*!
  \brief  compute b = inv(a) where a is LU factored
  \param b destination matrix
  \param a factored source matrix
  \param p pivot vector
*/
int InvertComplexMatrix (ComplexMatrix b, const ComplexMatrix a, const Matrix p);

/*!
  \brief  factor a into LU and store in b
  \param b destination matrix
  \param a source matrix
  \param p permutation vector
  \param info singularity code
*/
int LUFactorComplexMatrix (ComplexMatrix b, const ComplexMatrix a, const Matrix p, int *info);


/*!
  \brief  solve Ax=b and store result in c
  \param c destination vector
  \param a factorized source matrix
  \param b RHS vector
  \param p pivot vector
*/
int LUBackSolveComplexMatrix (ComplexMatrix c, const ComplexMatrix a, const ComplexMatrix b, Matrix p);

/*!
  \brief calculates |a|
  \param result pointer to result location
  \param a LU factorized source matrix
  \param p pivot vector
*/
int DeterminantComplexMatrix(complex *result, const ComplexMatrix a, const Matrix p);

/*!
  \brief compute one column of inv(a)
  \param b destination matrix
  \param a factored source matrix
*/
int InvertCroutComplexMatrix (ComplexMatrix b, const ComplexMatrix a, unsigned int col);

/*!
  \brief Crout factorize A and store in A
  \param A source and destination matrix
*/
int CroutFactorComplexMatrix (ComplexMatrix A);

/*!
  \brief  solve Ax=b and store x in b
  \param A Crout factored LHS matrix
  \param b RHS (and dest) vector
*/
int CroutBackSolveComplexMatrix (const ComplexMatrix A, ComplexMatrix b);

        /*
         * prototypes for the PROPERTY routines
         */

/*!
  \brief Aij == Aji ? 1 : 0
  \param a matrix to check for symmetry
*/
int IsSymmetricComplexMatrix (const ComplexMatrix a);

/*----------------------------------------------------------------------*/

#ifdef __cplusplus
}
#endif 

# endif	/* _CMATRIX_H */

