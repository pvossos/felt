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
 * File:	matrix.h    
 *	
 * Description:	
 ************************************************************************/

# ifndef _MATRIX_H
# define _MATRIX_H
# include <stdio.h>		/* for FILE definition			*/
# include "status.h"
# include "proto.h"

typedef struct matrix *Matrix;

struct matrix {
   unsigned	nrows;		/* number of rows 			 */
   unsigned	ncols;		/* number of columns			 */
   double	**data;		/* matrix data				 */
   unsigned	*diag;		/* diagonal addresses for compact column */
   unsigned	size;		/* actual size of compact storage	 */
   unsigned	refcount;	/* count of children			 */
   Matrix	parent;		/* parent of possible subsection	 */
};

# define Mrows(m)          ((m) -> nrows)
# define Mcols(m)          ((m) -> ncols)
# define Msize(m)          ((m) -> size ? (m) -> size : Mrows(m) * Mcols(m))

# define IsFull(m)         (!(m) -> size)
# define IsCompact(m)      ((m) -> size)
# define IsSquare(m)       (Mrows(m) == Mcols(m))
# define IsRowVector(m)    (Mrows(m) == 1)
# define IsColumnVector(m) (Mcols(m) == 1)
# define IsSubsection(m)   ((m) -> parent == NULL ? 0 : 1)

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

double mdata PROTO ((     
   Matrix,			/* matrix to fetch data from	*/
   unsigned,	    		/* row index			*/
   unsigned	    		/* column index			*/
));

Matrix CreateSubsectionMatrix PROTO (( 
   Matrix,			/* matrix to subsection		*/
   unsigned,			/* starting row			*/
   unsigned,			/* starting column		*/
   unsigned,			/* ending row			*/
   unsigned			/* ending column		*/
));

Matrix CreateFullMatrix PROTO ((
   unsigned,			/* number of rows		*/
   unsigned			/* number of columns		*/	
));

Matrix CreateRowVector PROTO ((
   unsigned			/* vector length		*/	
));

Matrix CreateColumnVector PROTO ((
   unsigned			/* vector length		*/
));

void DestroyMatrix PROTO ((
   Matrix			/* matrix to free		*/
));

Matrix CreateCompactMatrix PROTO ((
   unsigned,			/* number of rows		*/
   unsigned, 			/* number of columns		*/	
   unsigned,			/* actual size of storage	*/	
   unsigned *			/* diagonal index array		*/
));

Matrix CreateCopyMatrix PROTO ((
   Matrix			/* matrix to copy data from	*/	
));

Matrix MakeFullFromCompact PROTO ((
   Matrix			/* compact matrix to expand 	*/	
));

Matrix MakeCompactFromFull PROTO ((
   Matrix			/* full matrix to compact	*/	
)); 

int ConvertRowColumn PROTO ((
   unsigned,			/* row index			*/
   unsigned,			/* column index			*/
   Matrix			/* compact matrix		*/
));

	/*
	 * prototypes for the BASIC matrix routines
	 */

int ZeroMatrix PROTO ((		/* a = 0			   */
   Matrix	 		/* matrix to fill with zeros	   */
));

int CopyMatrix PROTO ((		/* b = a			   */
   Matrix,			/* source matrix		   */
   Matrix			/* destination matrix		   */
));

int IdentityMatrix PROTO ((	/* a = [I]			   */
   Matrix	 		/* destination matrix for identity */
));

int RandomMatrix PROTO ((	/* a(i,j) = rand()		   */
   Matrix,			/* matrix to randomize		   */
   int		     		/* optional seed		   */
));

int MirrorMatrix PROTO ((
   Matrix			/* matrix to complete		*/
));

int MultiplyMatrices PROTO ((	/* c = ab			*/
   Matrix,			/* destination matrix		*/
   Matrix, 			/* source matrix 1		*/
   Matrix			/* source matrix 2		*/
));

int AddMatrices PROTO ((	/* c = a + b			*/
   Matrix, 			/* destination matrix		*/
   Matrix,	  		/* source matrix 1		*/
   Matrix    			/* source matrix 2		*/
));

int SubtractMatrices PROTO ((	/* c = a - b 			*/
   Matrix,	  		/* destination matrix		*/
   Matrix,	  		/* source matrix 1		*/
   Matrix	  		/* source matrix 2		*/
));

int ModMatrices PROTO ((	/* c = a % b			*/
   Matrix, 			/* destination matrix		*/
   Matrix,	  		/* source matrix 1		*/
   Matrix    			/* source matrix 2		*/
));

int Saxpy PROTO ((        	/* c = b + alpha*a		*/	
   Matrix,	  		/* destination vector		*/
   Matrix,			/* source 1 vector		*/
   Matrix,	  		/* source 2 vector		*/
   double	      		/* scale factor			*/
));

int Gaxpy PROTO ((    		/* c = b + Aa 			*/
   Matrix,	  		/* destination vector		*/
   Matrix,	  		/* source 1 vector		*/
   Matrix,	  		/* source 2 vector		*/
   Matrix	  		/* source matrix		*/
));

int ScaleMatrix PROTO ((       /* b(i,j) = factor*a(i,j) + offset  */
   Matrix,	  		/* destination matrix		    */
   Matrix,	  		/* source matrix		    */
   double,	       		/* multiplicative scale factor	    */
   double	       		/* additive offset		    */
));

int SqrtMatrix PROTO ((         /* b(i,j) = sqrt (a(i,j))           */
   Matrix,	  		/* destination matrix		    */
   Matrix 	  		/* source matrix		    */
));

int DotBProduct PROTO ((	/* x = aTb			*/
   double *,	   		/* pointer to result location	*/
   Matrix,	  		/* source vector (row) 1	*/
   Matrix	  		/* source vector 2		*/
));

int TransposeMatrix PROTO (( 	/* b = aT			*/
   Matrix,	  		/* destination matrix		*/
   Matrix	  		/* source matrix		*/
));

int CompareEQMatrices PROTO (( 	/* c = (a == b)		*/
   Matrix,	  		/* destination matrix	*/
   Matrix,	  		/* first RHS matrix	*/
   Matrix	  		/* second RHS matrix	*/
));

int CompareNEQMatrices PROTO (( /* c = (a != b)		*/
   Matrix,	  		/* destination matrix	*/
   Matrix,	  		/* first RHS matrix	*/
   Matrix	  		/* second RHS matrix	*/
));

int CompareGTMatrices PROTO (( /* c = (a > b)		*/
   Matrix,	  		/* destination matrix	*/
   Matrix,	  		/* first RHS matrix	*/
   Matrix	  		/* second RHS matrix	*/
));

int CompareLTMatrices PROTO ((	/* c = (a < b)		*/
   Matrix,	  		/* destination matrix	*/
   Matrix,	  		/* first RHS matrix	*/
   Matrix	  		/* second RHS matrix	*/
));

int CompareLTEMatrices PROTO (( /* c = (a <= b)	*/
   Matrix,	  		 /* destination matrix	*/
   Matrix,	  		 /* first RHS matrix	*/
   Matrix	  		 /* second RHS matrix	*/
));

int CompareGTEMatrices PROTO (( /* c = (a >= b)	*/
   Matrix,	  		 /* destination matrix	*/
   Matrix,	  		 /* first RHS matrix	*/
   Matrix	  		 /* second RHS matrix	*/
));

int PrintMatrix PROTO (( 	/* print matrix m to file fp	*/
   Matrix,	  		/* matrix to print		*/
   FILE	*	    		/* file pointer for output	*/
));

int PrintMatrixSubsection PROTO (( 	/* print matrix m to file fp	*/
   Matrix,	  			/* matrix to print		*/
   unsigned,				/* starting row			*/
   unsigned,				/* starting column		*/
   unsigned,				/* ending row			*/
   unsigned,				/* ending column		*/
   FILE	*	    			/* file pointer for output	*/
));

	/*
	 * protoypes for the FACTORization machine
	 */

int QRFactorMatrix PROTO ((	/* factor a such that qTa = r		*/
   Matrix,	  		/* destination matrix for q		*/
   Matrix,	  		/* destination matrix for r		*/
   Matrix	  		/* source matrix			*/
));

int CholeskyFactorMatrix PROTO ((	/* factor a such that bbT=a	*/
   Matrix,	  			/* destination matrix		*/
   Matrix	  			/* source matrix		*/
));

int InvertMatrix PROTO (( 	/* compute b = inv(a) where a is LU factored */
   Matrix,	  		/* destination matrix			     */
   Matrix,	  		/* factored source matrix  		     */
   Matrix	  		/* pivot vector				     */
));

int DeterminantMatrix PROTO ((    /* result = |a|			*/
   double *,       		   /* pointer to result location	*/
   Matrix,	  		   /* factorized source matrix		*/
   Matrix	  		   /* pivot vector			*/
));

int LUFactorMatrix PROTO ((         /* factor a into LU and store in b 	*/
   Matrix,	  		    /* destination matrix		*/
   Matrix, 	  	  	    /* source matrix			*/
   Matrix, 	   		    /* permutation vector		*/
   int *     		    	    /* singularity code			*/
));

int LUBackSolveMatrix PROTO ((	    /* solve Ax=b and store result in c */	
   Matrix,	  		    /* destination vector	        */
   Matrix,	  		    /* factorized source matrix         */	
   Matrix,	  		    /* right hand side vector 	        */
   Matrix	  		    /* pivot vector		        */
));

int FormLUPMatrices PROTO ((        /* separate matrices from factors    */
   Matrix,			    /* return matrix for L		 */
   Matrix,			    /* return matrix for U		 */
   Matrix,			    /* return matrix for P		 */
   Matrix,			    /* factored form of matrix	         */
   Matrix			    /* pivot vector		         */
));

int CroutFactorMatrix PROTO ((      /* Crout factorize A and store in A	*/
   Matrix	  		    /* source and destination matrix	*/
));

int CroutBackSolveMatrix PROTO ((   /* solve Ax=b and store x in b	*/
   Matrix,   	   		    /* Crout factored LHS matrix	*/
   Matrix   	   		    /* RHS (and dest) vector		*/
));

	/*
 	 * prototypes for the EIGEN routines
	 */

int GeneralMatrixEigenModes PROTO ((	/* full, non-symmetric matrix	 */
   Matrix,				/* input matrix			 */
   Matrix,				/* output vector of eigenvalues  */
   double,				/* convergence tolerance	 */
   unsigned				/* limiting number of iterations */
));

int TridiagSymmMatrixEigenModes PROTO ((	
   Matrix,				  /* vector of diagonal elements */
   Matrix,				  /* vector of sub-diag elements */
   Matrix,				  /* vector of eigenvalues       */
   Matrix,				  /* eigenvectors output         */
   unsigned				  /* iteration limit	         */
));

int SymmetricMatrixEigenModes PROTO ((
   Matrix,				/* source matrix		   */
   Matrix,				/* vector for eigenvalues	   */
   Matrix,				/* matrix for eigenvectors	   */
   unsigned				/* iteration limit		   */
));

int BuildTridiagonalVectors PROTO ((
   Matrix,				/* symmetric, tri-diagonal input   */
   Matrix,				/* output vector of diag elements  */
   Matrix				/* vector of sub-digaonal elements */
));

int NormalizeByMaximum PROTO ((	
   Matrix,				/* destination matrix		*/
   Matrix,				/* eigenvectors to normalize	*/
   unsigned				/* flag to preserve sign	*/
));

int NormalizeByFirst PROTO ((	
   Matrix,				/* destination matrix		*/
   Matrix 				/* eigenvectors to normalize	*/
));

int NormalizeByLength PROTO ((	
   Matrix,				/* destination matrix		*/
   Matrix 				/* eigenvectors to normalize	*/
));

int TridiagonalReduction PROTO ((	
   Matrix,				/* source matrix		     */
   Matrix,				/* dest vector for diag elements     */
   Matrix,				/* dest vector for sub-diag elements */
   Matrix				/* accumulated orthog. transforms    */
));

	/*
	 * prototypes for the NORM routines
	 */

int FrobeniusNormMatrix PROTO ((    /* result = ||a||_f			*/
   double *,	        	    /* pointer to result location	*/
   Matrix	  		    /* matrix to take norm of		*/
));

int PNormMatrix PROTO ((      	/* result = ||a||_p			*/
   double *,      		/* pointer to space for result		*/
   Matrix,	  		/* source matrix			*/
   char	*  			/* "1", "inf" type of norm		*/
));

int PNormVector PROTO ((      	/* result = ||a||_p			*/
   double *,	        	/* pointer to space for result		*/
   Matrix,	  		/* source vector			*/
   char	*  			/* "1", "2", "inf" type of norm		*/
));

	/*
	 * prototypes for the PROPERTY routines
	 */

int IsSymmetricMatrix PROTO ((
   Matrix
));

int IsZeroMatrix PROTO ((
   Matrix
));

	/*
	 * prototypes for the STATISTICS routines
	 */

int MaximumMatrix PROTO ((
   Matrix,
   double *
));

int MinimumMatrix PROTO ((
   Matrix,
   double *
));

int SumMatrix PROTO ((
   Matrix,
   double *
));

int MeanMatrix PROTO ((
   Matrix,
   double *
));

int StddevMatrix PROTO ((
   Matrix,
   double *
));

	/*
	 * prototypes for the IO routines
	 */

int MatrixToMatlab PROTO ((
   Matrix,
   FILE *,
   char *
));

int MatricesToMatlab PROTO ((
   Matrix *,
   unsigned,
   FILE *,
   char **
));

Matrix MatlabToMatrix PROTO ((
   FILE *
));

	/*
	 * prototypes for the SOLVER routines
	 */

int GaussSeidel PROTO ((
   Matrix,
   Matrix,
   Matrix
));

# endif	/* _MATRIX_H */
