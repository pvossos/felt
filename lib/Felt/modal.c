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

/***************************************************************************
 *
 * File:	modal.c
 *
 * Description:	Contains code to implement modal analysis features of 
 *		the finite element method.
 *
 ***************************************************************************/

# include <stdio.h>
# include <math.h>
# include "fe.h"
# include "allocate.h"
# include "error.h"
# include "problem.h"

/****************************************************************************
 *
 * Function:	MultiplyQtKQ
 *
 ***************************************************************************/

static void MultiplyQtKQ (x, Q, K)
   Matrix	x;
   Matrix	K;
   Matrix	Q;
{
   Matrix	temp;
   double	result;
   unsigned	i,j,k;

   temp = CreateColumnVector (Mrows(K));

   for (j = 1 ; j <= Mcols (Q) ; j++) {
      for (i = 1 ; i <= Mcols (K) ; i++) {

         sdata(temp, i, 1) = 0;
         for (k = 1 ; k <= Mrows (K) ; k++) 
            sdata(temp, i, 1) += mdata(K,k,i) * mdata(Q,k,j);
      }

      for (i = 1 ; i <= Mcols (Q) ; i++) {
         result = 0;
         for (k = 1 ; k <= Mcols (K) ; k++) 
            result += mdata(temp,k,1) * mdata(Q,k,i);

         sdata(x, j, i) = result;
      }                  
   }

   DestroyMatrix (temp);
}

/****************************************************************************
 *
 * Function:	 ComputeEigenModes 
 *
 * Description: 
 *
 ****************************************************************************/

int ComputeEigenModes (K, M, lambda_r, x_r)
   Matrix	K;
   Matrix	M;
   Matrix	*lambda_r;
   Matrix	*x_r;
{
   int		status;
   Matrix	Q;
   Matrix	A;
   Matrix	p;
   Matrix	x_tran;
   Matrix	x_orig;
   Matrix	lambda;
   int		singular;

   Q    = CreateMatrix (Mrows(M), Mcols(M));
   A    = CreateMatrix (Mrows(M), Mcols(M));
   p    = CreateColumnVector (Mrows(M));
   x_tran = CreateMatrix (Mrows(M), Mcols(M));
   x_orig = CreateMatrix (Mrows(M), Mcols(M));
   lambda = CreateColumnVector (Mrows(M));

	/*
	 * form the Cholesky factorization of the mass matrix M = QQ^T
 	 */

   status = CholeskyFactorMatrix (Q, M);
   if (status)
      return status;

	/*
	 * invert the Cholesky factorization
	 */

   status = LUFactorMatrix (A, Q, p, &singular);
   if (status)
      return status;

   if (singular)
      return M_SINGULAR;

   status = InvertMatrix (Q, A, p);
   if (status)
      return status;

	/*
	 * form the coefficient matrix A = Qinv^T*K*Qinv
	 */

   MultiplyQtKQ (A, Q, K);

	/*
	 * get the eigenvalues and eigenvectors of the transformed problem
	 */

   status = SymmetricMatrixEigenModes (A, lambda, x_tran, analysis.iterations);
   if (status)
      return status;

	/*
	 * back transform the eigenvectors to get the eigenvectors of
	 * the original problem
	 */

   MultiplyMatrices (x_orig, Q, x_tran);

	/*
	 * take the square root of the eigenvalues to turn them into 
	 * natural frequencies (radians per second)
	 */

   status = SqrtMatrix (lambda, lambda);
   if (status)
      return status;

   DestroyMatrix (Q);
   DestroyMatrix (p);
   DestroyMatrix (x_tran);
   DestroyMatrix (A);

   *x_r = x_orig;
   *lambda_r = lambda;

   return 0;
}

/***************************************************************************
 *
 * Function:	ModalNodalDisplacements
 *
 * Description:	Given a table of mode shapes and a list of nodes and active
 *		dofs, put together a table of nodal displacements at each
 *		node (including constrained nodes) for translational DOFs
 *		in each separate mode of vibration.  The resulting table
 *		should be semi-analogous to the displacement table in transient
 *		analysis. 
 *
 ***************************************************************************/

Matrix ModalNodalDisplacements (x)
   Matrix	x;
{
   Node		*node;
   unsigned	numnodes;
   unsigned	*dofs;
   unsigned	numdofs;
   unsigned	i, m, n;
   unsigned	nummodes;
   unsigned	count;
   unsigned	numtrans;
   unsigned	numrot;
   unsigned	trans_dofs [4];
   unsigned	rot_dofs [4];
   Matrix	d;

   node     = problem.nodes;
   numnodes = problem.num_nodes;
   dofs     = problem.dofs_pos;
   numdofs  = problem.num_dofs;

   numtrans = 0;
   numrot = 0;
   for (i = 1 ; i <= 3 ; i++) {
      if (dofs [i]) 
         trans_dofs [++ numtrans] = i;
      if (dofs [i+3])
         rot_dofs [++numrot] = i+3;
   }
 

   if (numtrans == 0)
      return NullMatrix;

   nummodes = Mcols(x);

   d = CreateMatrix (nummodes, numnodes*numtrans); 

   for (m = 1 ; m <= nummodes ; m++) {
      count = 1;
      for (n = 1 ; n <= numnodes ; n++) {
         for (i = 1 ; i <= numtrans ; i++) {
            if (node [n] -> constraint -> constraint [trans_dofs [i]])
               sdata(d, m, (n-1)*numtrans+i) = 0.0;
            else {
               sdata(d, m, (n-1)*numtrans+i) = mdata(x,count,m);
               count ++;
            }
         }
         for (i = 1 ; i <= numrot ; i++) {
            if (!node [n] -> constraint -> constraint [rot_dofs [i]])
               count++;
         }
      }
   }

   return d;
}

/****************************************************************************
 *
 * Function:	MultiplyUtMU
 *
 ***************************************************************************/

static void MultiplyUTmU (M, u, m)
   Matrix	M;
   Matrix	u;
   Matrix	m;
{
   Matrix	temp;
   double	result;
   unsigned	i,j,k;
   unsigned	n;

   n = Mrows(u);

   temp = CreateColumnVector (Mrows(m));

   for (j = 1 ; j <= n ; j++) {
      for (i = 1 ; i <= n ; i++) {

         sdata(temp, i, 1) = 0;
         for (k = 1 ; k <= n ; k++) 
            sdata(temp, i, 1) += mdata(m,k,i) * mdata(u,k,j);
      }

      result = 0;
      for (i = 1 ; i <= n ; i++) 
         result += mdata(temp,i,1) * mdata(u,i,j);

      M -> data [j][1] = result;
   }

   DestroyMatrix (temp);
}

int FormModalMatrices (u, m, c, k, Mr, Cr, Kr, ortho)
   Matrix	u;
   Matrix	m, c, k;
   Matrix 	*Mr, *Cr, *Kr;
   int		ortho;
{
   unsigned	n;
   unsigned	i, j;
   double	factor;
   Matrix	M, C, K;
   unsigned	*diag;

   n = Mrows(m);

   diag = Allocate (unsigned, n);
   UnitOffset (diag);

   for (i = 1 ; i <= n ; i++)
      diag [i] = i;

   M = CreateCompactMatrix (n, n, n, NULL);
   M -> diag = diag;

   C = CreateCompactMatrix (n, n, n, diag);
   K = CreateCompactMatrix (n, n, n, diag);

   if (ortho) {
      MultiplyUTmU (M, u, m);
      for (j = 1 ; j <= n ; j++) {
         factor = M -> data [j][1];

         for (i = 1 ; i <= n ; i++) 
            sdata(u, i, j) /= factor;

         M -> data [j][1] = 1.0;
      }
   }
   else {
      NormalizeByFirst (u, u);
      MultiplyUTmU (M, u, m);
   }

   MultiplyUTmU (C, u, c);
   MultiplyUTmU (K, u, k);

   *Mr = M;
   *Cr = C;
   *Kr = K;

   return 0;   
}	
