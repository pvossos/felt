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

# ifndef _MATRIX_H
# define _MATRIX_H

# include <stdio.h>		/* for FILE definition			*/
# include "status.h"
# include "cvector1.hpp"

/*----------------------------------------------------------------------*/

typedef struct matrix *Matrix;

struct matrix {
   unsigned	nrows;		/* number of rows 			 */
   unsigned	ncols;		/* number of columns			 */
   double	**data;		/* matrix data				 */
   cvector1<unsigned> diag; /* diagonal addresses for compact column */
   unsigned	size;		/* actual size of compact storage	 */
};

# define Mrows(m)          ((m) -> nrows)
# define Mcols(m)          ((m) -> ncols)
# define Msize(m)          ((m) -> size ? (m) -> size : Mrows(m) * Mcols(m))

# define IsFull(m)         (!(m) -> size)
# define IsCompact(m)      ((m) -> size)
# define IsSquare(m)       (Mrows(m) == Mcols(m))
# define IsRowVector(m)    (Mrows(m) == 1)
# define IsColumnVector(m) (Mcols(m) == 1)

# define sdata(m,i,j)   ((m) -> data [(i)][(j)])
	
	/*
	 * for backward compatibility with the old matrix routines
	 */

typedef Matrix Vector;

# define MatrixData(m) 	((m) -> data)
# define VectorData(m) 	((m) -> data [1])
# define MatrixRows(m)  ((m) -> nrows)
# define MatrixCols(m)  ((m) -> ncols)
# define VectorSize(m)  (IsColumnVector(m) ? (m) -> nrows : (m) -> ncols)

# define NullMatrix ((Matrix) NULL)
# define NullVector ((Matrix) NULL)

# define CreateMatrix  CreateFullMatrix
# define CreateVector  CreateColumnVector
# define DestroyVector DestroyMatrix

	/*
	 * prototypes for DATA manipulation routines
	 */

/*!
  \param A matrix to fetch data from
  \param row row index
  \param col column index
*/
double mdata (const Matrix A, unsigned int row, unsigned int col);

/*!
  \param rows number of rows
  \param cols number of columns
*/
Matrix CreateFullMatrix (unsigned int rows, unsigned int cols);

/*!
  \param size vector length
*/
Matrix CreateRowVector (unsigned int size);

/*!
  \param size vector length
*/
Matrix CreateColumnVector (unsigned int size);

/*!
  \param m matrix to free
*/
void DestroyMatrix (Matrix m);

/*!
  \param rows number of rows
  \param cols number of columns
  \param size actual size of storage
  \param diag diagonial index array
 */
Matrix CreateCompactMatrix (unsigned int rows, unsigned int cols, 
                            unsigned int size, const cvector1<unsigned> *diag);

/*!
  \param a matrix to copy data from
*/
Matrix CreateCopyMatrix (const Matrix a);

/*!
  \param A compact matrix to expand
*/
Matrix MakeFullFromCompact (const Matrix A);

/*!
  \param A full matrix to compact
*/
Matrix MakeCompactFromFull (const Matrix A);

/*!
  Given a compact column storage scheme described by diag
  and convert row and column into an address into
  a compact column matrix representation

  \param row the row of what would be the full matrix
  \param col the column of what would be the full matrix
  \param a matrix with array of diagonal address
  \return a valid address if there is one, 0 if there's no
  need to worry about what would be in that spot of
  the full matrix.
 */
int ConvertRowColumn (unsigned int row, unsigned int col, const Matrix a);

	/*
	 * prototypes for the BASIC matrix routines
	 */

/*!
  \brief A = 0 
  \param a the Matrix to fill with zeros
 */
int ZeroMatrix (Matrix a);

/*!
  \brief B = A
  \param a the source Matrix
  \param b the destination Matrix
 */
int CopyMatrix (Matrix b, const Matrix a);

/*!
  \brief A = [I]
  \param a destination matrix for identity
 */
int IdentityMatrix (Matrix a);

/*!
  \brief a(i,j) = rand()
  \param a Matrix to randomize
  \param seed optional seed
 */
int RandomMatrix (Matrix a, int seed);

/*!
  \param a the Matrix to mirror
 */
int MirrorMatrix (Matrix a);

/*!
  \brief c = a * b
  \param c destination Matrix
  \param a source matrix 1
  \param b source matrix 2
 */
int MultiplyMatrices (Matrix c, const Matrix a, const Matrix b);

/*!
  \brief c = a + b
  \param c destination Matrix
  \param a source Matrix 1
  \param b source Matrix 2
 */
int AddMatrices (Matrix c, const Matrix a, const Matrix b);

/*!
  \brief c = a - b
  \param c destination Matrix
  \param a source Matrix 1
  \param b source Matrix 2
*/
int SubtractMatrices (Matrix c, const Matrix a, const Matrix b);

/*!
  \brief c = a % b
  \param c destination Matrix
  \param a source Matrix 1
  \param b source Matrix 2
 */
int ModMatrices (Matrix c, const Matrix a, const Matrix b);

/*!
  \brief c = b + alpha * a
  \param c destination vector
  \param a source vector 1
  \param b source vector 2
  \param alpha scale factor
*/
int Saxpy (Matrix c, const Matrix a, const Matrix b, double alpha);

/*!
  \brief c = b + A * a
  \param c destination vector
  \param a source vector 1
  \param b source vector 2
  \param A source Matrix
*/
int Gaxpy (Matrix c, const Matrix a, const Matrix b, const Matrix A);

/*!
  \brief b(i,j) = factor*a(i,j) + offset
  \param b destination matrix
  \param a source matrix
  \param factor multiplicative scale factor
  \param offset additive offset
 */
int ScaleMatrix(Matrix b, const Matrix a, double factor, double offset);

/*!
  \brief b(i,j) = sqrt(a(i,j))
  \param b destination matrix
  \param a source matrix
 */
int SqrtMatrix(Matrix b, const Matrix a);

/*!
  \brief x = aTb
  \param x pointer to result location
  \param a source vector (row) 1
  \param b sourve vector 2
 */
int DotBProduct(double *x, const Matrix a, const Matrix b);

/*!
  \brief b = aT
  \param b destination matrix
  \param a source matrix
*/
int TransposeMatrix(Matrix b, const Matrix a);

/*!
  \brief  c = (a == b)
  \param c destination matrix
  \param a first RHS matrix
  \param b second RHS matrix
*/
int CompareEQMatrices (Matrix c, const Matrix a, const Matrix b);

/*!
  \brief c = (a != b)
  \param c destination matrix
  \param a first RHS matrix
  \param b second RHS matrix
*/
int CompareNEQMatrices (Matrix c, const Matrix a, const Matrix b);

/*!
  \brief c = (a > b)
  \param c destination matrix
  \param a first RHS matrix
  \param b second RHS matrix
*/
int CompareGTMatrices (Matrix c, const Matrix a, const Matrix b);

/*!
  \brief c = (a < b)
  \param c destination matrix
  \param a first RHS matrix
  \param b second RHS matrix
*/
int CompareLTMatrices (Matrix c, const Matrix a, const Matrix b);

/*!
  \brief c = (a <= b)
  \param c destination matrix
  \param a first RHS matrix
  \param b second RHS matrix
*/
int CompareLTEMatrices (Matrix c, const Matrix a, const Matrix b);

/*!
  \brief c = (a >= b)
  \param c destination matrix
  \param a first RHS matrix
  \param b second RHS matrix
*/
int CompareGTEMatrices (Matrix c, const Matrix a, const Matrix b);

/*!
  \brief print matrix m to file fp
  \param m matrix to print
  \param fp file pointer for output
*/
int PrintMatrix (const Matrix m, FILE *fp);


	/*
	 * protoypes for the FACTORization machine
	 */

/*!
  \brief factor a such that qTa = r
  \param q destination matrix for q
  \param r destination matrix for r
  \param a source matrix
*/
int QRFactorMatrix (Matrix q, Matrix r, const Matrix a);


/*!
  \brief factor a such that bbT=a
  \param b destination matrix
  \param a source matrix
*/
int CholeskyFactorMatrix (Matrix b, const Matrix a);


/*!
  \brief  compute b = inv(a) where a is LU factored
  \param b destination matrix
  \param a factored source matrix
  \param p pivot vector
*/
int InvertMatrix (Matrix b, const Matrix a, const Matrix p);

/*
  \brief result = |a|
  \param result pointer to result location
  \param a factorized source matrix
  \param p pivot vector
*/
int DeterminantMatrix (double *result, const Matrix a, const Matrix p);

/*!
  \brief  factor a into LU and store in b
  \param b destination matrix
  \param a source matrix
  \param p permutation vector
  \param info singularity code
*/
int LUFactorMatrix (Matrix b, const Matrix a, const Matrix p, int *info);

/*!
  \brief  solve Ax=b and store result in c
  \param c destination vector
  \param a factorized source matrix
  \param b RHS vector
  \param p pivot vector
*/
int LUBackSolveMatrix (Matrix c, const Matrix a, const Matrix b, const Matrix p);

/*!
  \brief separate matrices from factors
  \param L return matrix for L
  \param U return matrix for U
  \param P return matrix for P
  \param a factored form of matrix
  \param p pivot vector
*/
int FormLUPMatrices (Matrix L, Matrix U, Matrix P, const Matrix a, const Matrix p);

/*!
  \brief crout factorize A and store in A
  \param A source and destination matrix
*/
int CroutFactorMatrix (Matrix A);

/*!
  \brief  solve Ax=b and store x in b
  \param A Crout factored LHS matrix
  \param b RHS (and dest) vector
*/
int CroutBackSolveMatrix (const Matrix A, Matrix b);

	/*
 	 * prototypes for the EIGEN routines
	 */

/*!
  \param a input matrix
  \param lambda output vector of eigenvalues
  \param tol convergence tolerance
  \param maxit limiting number of iterations
*/
int GeneralMatrixEigenModes (const Matrix a, Matrix lambda, double tol, unsigned int maxit);

/*!
  \param diag vector of diagonal elements
  \param sub_diag vector of sub-diag elements
  \param lambda vector of eigenevalues
  \param x eigenvectors output
  \param maxit iteration limit
*/
int TridiagSymmMatrixEigenModes (const Matrix diag, const Matrix sub_diag, const Matrix lambda, Matrix x, unsigned int maxit);

/*!
  \param a source matrix
  \param lambda vector for eigenvalues
  \param x matrix for eigenvectors
  \param maxit iteration limit
*/
int SymmetricMatrixEigenModes (const Matrix a, const Matrix lambda, Matrix x, unsigned int maxit);

/*!
  \param a symmetric, tri-diagonal input
  \param diag output vector of diag elements
  \param sub_diag vector of sub-diagonal elements
 */
int BuildTridiagonalVectors (const Matrix a, Matrix diag, Matrix sub_diag);

/*!
  \param b destination matrix
  \param a eigenvectors to normalize
  \param keep_sign flag to preserve sign
*/
int NormalizeByMaximum (Matrix b, const Matrix a, unsigned int keep_sign);

/*!
  \param b destination matrix
  \param a eigenvectors to normalize
*/
int NormalizeByFirst (Matrix b, const Matrix a);

/*!
  \param b destination matrix
  \param a eigenvectors to normalize
*/
int NormalizeByLength (Matrix b, const Matrix a);

/*!
  \param a source matrix
  \param diag dest vector for diag elements
  \param sub_diag dest vector for sub-diag elements
  \param z accumulated orthog. transforms
 */
int TridiagonalReduction (const Matrix a, Matrix diag, Matrix sub_diag, Matrix z);

	/*
	 * prototypes for the NORM routines
	 */

/*!
  \param result returns ||a||_f
  \param a matrix to take norm of
*/
int FrobeniusNormMatrix (double *result, const Matrix a);

/*!
  \param result pointer to space for result, ||a||_p
  \param a source matrix
  \param p "1", "inf" type of norm
*/
int PNormMatrix (double *result, const Matrix a, const char *p);

/*!
  \param result pointer to space for result, ||a||_p
  \param a source vector
  \param p "1", "2", "inf" type of norm
*/
int PNormVector (double *result, const Matrix a, const char *p);

	/*
	 * prototypes for the PROPERTY routines
	 */

/*!
  \brief Aij == Aji ? 1 : 0
  \param a Matrix to check for symmetry
*/
int IsSymmetricMatrix (const Matrix a);

/*!
  \brief Aij == 0 ? 1 : 0
  \param a Matrix to check for symmetry
*/
int IsZeroMatrix (const Matrix a);

	/*
	 * prototypes for the STATISTICS routines
	 */

int MaximumMatrix (const Matrix a, double *x);

int MinimumMatrix (const Matrix a, double *x);

int SumMatrix (const Matrix a, double *x);

int MeanMatrix (const Matrix a, double *x);

int StddevMatrix (const Matrix a, double *x);

	/*
	 * prototypes for the IO routines
	 */

int MatrixToMatlab (const Matrix a, FILE *fp, const char *name);

int MatricesToMatlab (const Matrix *a, unsigned int n, FILE *fp, const char **name);

Matrix MatlabToMatrix (FILE *fp);

	/*
	 * prototypes for the SOLVER routines
	 */

int GaussSeidel(Matrix x, const Matrix A, const Matrix b);

/*----------------------------------------------------------------------*/

# endif	/* _MATRIX_H */
