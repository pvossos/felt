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
 * File:	cmatrix.h    
 *	
 * Description:	
 ************************************************************************/

# ifndef _CMATRIX_H
# define _CMATRIX_H

# include <stdio.h>		/* for FILE definition			*/
# include "complex.h"
# include "status.h"
# include "proto.h"

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

complex cmdata PROTO ((     
   ComplexMatrix,		/* matrix to fetch data from	*/
   unsigned,	    		/* row index			*/
   unsigned	    		/* column index			*/
));

ComplexMatrix CreateFullComplexMatrix PROTO ((
   unsigned,			/* number of rows		*/
   unsigned			/* number of columns		*/	
));

ComplexMatrix CreateComplexRowVector PROTO ((
   unsigned			/* vector length		*/	
));

ComplexMatrix CreateComplexColumnVector PROTO ((
   unsigned			/* vector length		*/
));

void DestroyComplexMatrix PROTO ((
   ComplexMatrix		/* matrix to free		*/
));

ComplexMatrix CreateCompactComplexMatrix PROTO ((
   unsigned,			/* number of rows		*/
   unsigned, 			/* number of columns		*/	
   unsigned,			/* actual size of storage	*/	
   unsigned *			/* diagonal index array		*/
));

ComplexMatrix CreateCopyComplexMatrix PROTO ((
   ComplexMatrix			/* matrix to copy data from	*/	
));

ComplexMatrix MakeFullFromCompactComplex PROTO ((
   ComplexMatrix			/* compact matrix to expand 	*/	
));

ComplexMatrix MakeCompactFromFullComplex PROTO ((
   ComplexMatrix			/* full matrix to compact	*/	
)); 

	/*
	 * prototypes for the BASIC matrix routines
	 */

int ZeroComplexMatrix PROTO ((		/* a = 0			   */
   ComplexMatrix	 		/* matrix to fill with zeros	   */
));

int CopyComplexMatrix PROTO ((		/* b = a			   */
   ComplexMatrix,			/* source matrix		   */
   ComplexMatrix			/* destination matrix		   */
));

int RandomComplexMatrix PROTO ((	/* a(i,j) = rand()		   */
   ComplexMatrix,			/* matrix to randomize		   */
   int		     			/* optional seed		   */
));

int MirrorComplexMatrix PROTO ((
   ComplexMatrix			/* matrix to complete		*/
));

int MultiplyComplexMatrices PROTO ((	/* c = ab			*/
   ComplexMatrix,			/* destination matrix		*/
   ComplexMatrix, 			/* source matrix 1		*/
   ComplexMatrix			/* source matrix 2		*/
));

int AddComplexMatrices PROTO ((		/* c = a + b			*/
   ComplexMatrix, 			/* destination matrix		*/
   ComplexMatrix,	  		/* source matrix 1		*/
   ComplexMatrix    			/* source matrix 2		*/
));

int SubtractComplexMatrices PROTO ((	/* c = a - b 			*/
   ComplexMatrix,	  		/* destination matrix		*/
   ComplexMatrix,	  		/* source matrix 1		*/
   ComplexMatrix	  		/* source matrix 2		*/
));

int ScaleComplexMatrix PROTO (( 	/* b = a*y + x			*/
   ComplexMatrix,	  		/* destination matrix		*/
   ComplexMatrix,	  		/* source matrix		*/
   complex,
   complex
));

int ModulusComplexMatrix PROTO ((	/* b = |a|			*/
   Matrix,				/* real destination matrix	*/
   ComplexMatrix			/* complex source matrix	*/
));

int TransposeComplexMatrix PROTO (( 	/* b = aT			*/
   ComplexMatrix,	  		/* destination matrix		*/
   ComplexMatrix	  		/* source matrix		*/
));

int PrintComplexMatrix PROTO (( 	/* print matrix m to file fp	*/
   ComplexMatrix,	  		/* matrix to print		*/
   FILE	*	    			/* file pointer for output	*/
));

	/*
	 * protoypes for the FACTORization machine
	 */

int InvertComplexMatrix PROTO (( /* compute b = inv(a) where a is LU factored */
   ComplexMatrix,	  	 /* destination matrix		              */
   ComplexMatrix,	  	 /* factored source matrix  		      */
   Matrix	  		 /* pivot vector			      */
));

int LUFactorComplexMatrix PROTO ((  /* factor a into LU and store in b 	*/
   ComplexMatrix,	  	    /* destination matrix		*/
   ComplexMatrix, 	  	    /* source matrix			*/
   Matrix, 	   		    /* permutation vector		*/
   int *     		    	    /* singularity code			*/
));

int LUBackSolveComplexMatrix PROTO ((	/* solve Ax=b and store result in c */	
   ComplexMatrix,	  		/* destination vector	            */
   ComplexMatrix,	  		/* factorized source matrix         */	
   ComplexMatrix,	  		/* right hand side vector 	    */
   Matrix	  		    	/* pivot vector		            */
));

int DeterminantComplexMatix PROTO ((	/* x = |a|			*/
   complex *,				/* pointer to result location	*/
   ComplexMatrix,			/* LU factored source matrix	*/
   Matrix				/* pivot vector			*/
));

int InvertCroutComplexMatrix PROTO ((
   ComplexMatrix,
   ComplexMatrix,
   unsigned
));

int CroutFactorComplexMatrix PROTO ((   /* Crout factorize A and store in A */
   ComplexMatrix	  		/* source and destination matrix    */
));

int CroutBackSolveComplexMatrix PROTO ((   /* solve Ax=b and store x in b  */
   ComplexMatrix,   	   		   /* Crout factored LHS matrix	   */
   ComplexMatrix   	   		   /* RHS (and dest) vector	   */
));

        /*
         * prototypes for the PROPERTY routines
         */

int IsSymmetricComplexMatrix PROTO ((
   ComplexMatrix
));

# endif	/* _CMATRIX_H */

