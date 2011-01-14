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
# include "cvector1.hpp"
# include "matrix.h"
# include "error.h"

# define DEFAULT_TOLERANCE 1.0e-6
# define DEFAULT_MAXIT 1000

# define SIGN(x,y) ((y) > 0.0 ? fabs(x) : -fabs(x))
# define MAX(x,y) ((x) > (y) ? (x) : (y))

static void HessenbergReduction (Matrix a)
{
   unsigned	n;
   unsigned	i,j,k;
   double	sigma, rho, eta, pi;
   double	sum;

   n = Mrows(a) - 2;
   sigma = 0.0;

   cvector1d v(n);

   for (k = 1 ; k <= n-2 ; k++) {
      eta = fabs (mdata(a,k+1,k));

      for (i = k+1 ; i <= n ; i++) {
         if (eta < fabs (mdata(a,i,k))) 
            eta = fabs (mdata(a,i,k));
      }

      if (eta != 0.0) {
         sum = 0.0;

         for (i = k+1 ; i <= n ; i++) {
            v [i] = mdata(a,i,k) / eta;
            sdata(a, i, k) = v [i];
          
            sum += v[i] * v[i];
         }

         if (v [k+1] != 0.0) 
            sigma = sqrt (sum)*v [k+1] / fabs (v [k+1]);

         v [k+1] = v [k+1] + sigma;
         pi = sigma*v [k+1];
         
         sdata (a, n+1, k) = pi;

	/*
	 * pre-multiply by the Householder reflector
	 */

         for (j = k+1 ; j <= n ; j++) {
            sum = 0.0;

            for (i = k+1 ; i <= n ; i++) 
               sum += v [i]*mdata(a,i,j);

            rho = sum / pi;

            for (i = k+1 ; i <= n ; i++)
               sdata(a, i, j) = mdata(a,i,j) - rho*v [i];
         }

	/*
	 * post-multiply by that same Householder reflector
	 */

         for (i = 1 ; i <= n ; i++) {
            sum = 0.0;
 
            for (j = k+1 ; j <= n ; j++)
               sum += mdata(a,i,j)*v [j];

            rho = sum / pi;

            for (j = k+1 ; j <= n ; j++)
               sdata(a, i, j) = mdata(a,i,j) - rho*v [j];
         }

         sdata(a, n+2, k) = v [k+1];
         sdata(a, k+1, k) = -eta*sigma;
      }
      else
         sdata(a, n+1, k) = 0.0;
   }

	/*
	 * zero out everything below the sub-diagonal
	 */

   for (j = 1 ; j <= n-2 ; j++) 
      for (i = j+2 ; i <= n ; i++)
         sdata(a, i, j) = 0.0; 

   return;
}

static void GeneralShiftedQR (Matrix a, unsigned int maxit, double tol)
{
   unsigned	i,j,k;
   unsigned	n;
   double	eta, alpha, beta, delta, kappa, nu;
   double	temp;

   n = Mrows(a) - 2;

   cvector1d sigma(n);
   cvector1d gamma(n);

   for (i = 1 ; i <= maxit ; i++) {

      kappa = mdata(a,n,n);
      sdata(a, 1, 1) = mdata(a,1,1) - kappa;

      for (k = 1 ; k <= n ; k++) {

         if (k != n) {

            eta = MAX(fabs (mdata(a,k,k)), fabs (mdata(a,k+1,k)));
            alpha = mdata(a,k,k)/eta;
            beta = mdata(a,k+1,k)/eta;
            delta = sqrt(alpha*alpha + beta*beta);
            gamma [k] = alpha / delta;
            sigma [k] = beta / delta;
            nu = eta*delta;
            
            sdata(a, k, k) = nu;
            sdata(a, k+1, k) = 0.0;
            sdata(a, k+1, k+1) = mdata(a,k+1,k+1) - kappa;

            for (j = k+1 ; j <= n ; j++) {
               temp = mdata(a,k,j);
               sdata(a, k, j) = gamma [k] * mdata(a,k,j) + 
                                sigma [k] * mdata(a,k+1,j);
               sdata(a, k+1, j) = gamma [k] * mdata(a,k+1,j) -
                                  sigma [k] * temp;
            }
         }

         if (k != 1) {
            for (j = 1 ; j <= k ; j++) {
               temp = mdata(a,j,k-1);
               sdata(a,j,k-1) = gamma [k-1] * mdata(a,j,k-1) +
                                sigma [k-1] * mdata(a,j,k);
               sdata(a,j,k) = gamma [k-1] * mdata(a,j,k) - 
                              sigma [k-1] * temp;
            }
        
            sdata(a,k-1,k-1) = mdata(a,k-1,k-1) + kappa;
         }
      }         

      sdata(a, n, n) = mdata(a,n,n) + kappa;

      if (fabs (mdata(a,n,n-1)) < tol)
         n = n - 1;

      if (n == 1) 
         break;
   }

   return;
}

int GeneralMatrixEigenModes (const Matrix a, Matrix lambda, double tol, unsigned int maxit)
{
   unsigned	i, j;
   Matrix	work;

   if (IsCompact(a))
      return M_COMPACT;

   if (!IsSquare(a))
      return M_NOTSQUARE;

   if (Mrows(a) != Mrows(lambda))
      return M_SIZEMISMATCH;

   if (!IsColumnVector(lambda))
      return M_NOTCOLUMN;

   if (tol == 0)
      tol = DEFAULT_TOLERANCE; 

   if (maxit == 0)
      maxit = DEFAULT_MAXIT;
 
   work = CreateMatrix (Mrows(a) + 2, Mcols(a));

   for (i = 1 ; i <= Mrows(a) ; i++)
      for (j = 1 ; j <= Mcols(a) ; j++)
         sdata(work, i, j) = mdata(a,i,j);

   HessenbergReduction (work);
   
   GeneralShiftedQR (work, maxit, tol);

   for (i = 1 ; i <= Mrows(a) ; i++)
      sdata(lambda, i, 1) = mdata(work, i, i);

	/*
	 * we need to copy the overwrite the original a matrix with
	 * the eigenvectors now
	 */

   DestroyMatrix (work);

   return 0;
}

static int SymmetricImplicitQL (Matrix d, Matrix sd, Matrix x, unsigned int maxit)
{
   unsigned	i, j, k, m;
   unsigned	mmj, ii;
   unsigned	iteration;
   unsigned	n;
   unsigned	convergence;
   double	test1, test2;
   double	c, p, s, f, g, h, r;
   double	c2, c3, s2;
   double	dj1, sdj1;

   n = Mrows(x);

   f = 0.0;
   test1 = 0.0;
   c3 = s2 = 0.0;

   for (j = 1 ; j <= n ; j++) {

      h = fabs (mdata(d,j,1)) + fabs (mdata(sd,j,1));
      if (test1 < h)
         test1 = h;

      for (m = j ; m <= n ; m++) {
         test2 = test1 + fabs (mdata(sd,m,1));
         if (test1 == test2)
            break;
      }

      if (m == j) {
         sdata(d, j, 1) += f;
         continue;
      }

      convergence = 0;
      for (iteration = 1 ; iteration <= maxit ; iteration ++) {

         g = mdata(d,j,1);
         p = (mdata(d,j+1,1) - g) / (2.0*mdata(sd,j,1));
         r = hypot (p, 1.0);

         sdata(d, j, 1) = mdata(sd,j,1) / (p + SIGN(r,p));
         sdata(d, j+1, 1) = mdata(sd,j,1) * (p + SIGN(r,p));
         dj1 = mdata(d,j+1,1);

         h = g - mdata(d,j,1);       

         if (j+2 <= n) {
            for (i = j+2 ; i <= n ; i++)
               sdata(d, i, 1) -= h;
         }

         f += h;

         p = mdata(d,m,1);
         c = 1.0;
         c2 = c;
         sdj1 = mdata(sd,j+1,1);
         s = 0.0;
      
         mmj = m - j;

         for (ii = 1 ; ii <= mmj ; ii++) {
            c3 = c2;
            c2 = c;
            s2 = s;

            i = m - ii;
   
            g = c * mdata(sd,i,1);
            h = c * p;
            r = hypot (p, mdata(sd,i,1));
            sdata(sd, i+1, 1) = s * r;
            s = mdata(sd,i,1) / r;
            c = p / r;
            p = c*mdata(d,i,1) - s*g;
            sdata(d, i+1, 1) = h + s * (c*g + s*mdata(d,i,1));

            for (k = 1 ; k <= n ; k++) {
               h = mdata(x,k,i+1);
               sdata(x, k, i+1) = s * mdata(x,k,i) + c*h;
               sdata(x, k, i) = c * mdata(x,k,i) - s*h;
            }
         }

         p = -s * s2 * c3 * sdj1 * mdata(sd,j,1) / dj1;
         sdata(sd, j, 1) = s * p;
         sdata(d, j, 1) = c * p;

         test2 = test1 + fabs (mdata(sd,j,1));

         if (test2 <= test1) {
            sdata(d, j, 1) += f;
            convergence = 1;
            break;
         }
      }

      if (!convergence) 
         return M_NOTCONVERGED;
   }

   for (ii = 2 ; ii <= n ; ii++) {
      i = ii - 1;
      k = i;
      p = mdata(d,i,1);

      for (j = ii ; j <= n ; j++) {
         if (mdata(d,j,1) >= p)
            continue;

         k = j;
         p = mdata(d,j,1);
      }
        
      if (k == i)
         continue;

      sdata(d, k, 1) = mdata(d,i,1);
      sdata(d, i, 1) = p;

      for (j = 1 ; j <= n ; j++) {
         p = mdata(x,j,i);
         sdata(x, j, i) = mdata(x,j,k);
         sdata(x, j, k) = p;
      }
   }

   return 0; 
}

int SymmetricMatrixEigenModes (const Matrix a, const Matrix lambda, Matrix x, unsigned int maxit)
{
   Matrix	diag;
   Matrix	sub_diag;
   unsigned	n;
   int		status;

   n = Mrows(a);

   if (IsCompact(x))
      return M_COMPACT;

   if (!IsSquare(x))
      return M_NOTSQUARE;

   if (Mrows(a) != Mrows(x) || Mcols(a) != Mcols(x))
      return M_SIZEMISMATCH;

   if (!IsColumnVector(lambda))
      return M_NOTCOLUMN;

   if (Mrows(a) != Mrows(lambda))
      return M_SIZEMISMATCH;

   if (maxit == 0)
      maxit = DEFAULT_MAXIT;
 
   diag     = lambda;
   sub_diag = CreateColumnVector (n);
  
   status = TridiagonalReduction (a, diag, sub_diag, x); 
   if (status)
      return status;

   status = TridiagSymmMatrixEigenModes (diag, sub_diag, diag, x, maxit);
   if (status)
      return status;

   DestroyMatrix (sub_diag);

   return 0;
}

int TridiagSymmMatrixEigenModes (const Matrix diag, const Matrix sub_diag, const Matrix lambda, Matrix x, unsigned int maxit)
{
   unsigned	i;
   int		status;
   unsigned	n;

   if (IsCompact(x))
      return M_COMPACT;

   if (Mrows(diag) != Mrows(sub_diag) || Mrows(diag) != Mrows(lambda))
      return M_SIZEMISMATCH;

   if (!IsSquare(x))
      return M_NOTSQUARE;

   if (!IsColumnVector(lambda))
      return M_NOTCOLUMN;

   if (!IsColumnVector(diag) || !IsColumnVector(sub_diag))
      return M_NOTCOLUMN;

   if (maxit == 0)
      maxit = DEFAULT_MAXIT;

   n = Mrows(x);

   status = SymmetricImplicitQL (diag, sub_diag, x, maxit);
   if (status)
      return status;

   if (lambda != diag) {
      for (i = 1 ; i <= n ; i++)
         sdata(lambda, i, 1) = mdata(diag,i,1);
   }

   return 0;
}

/* UNUSED
static int CholeskyReduction (Matrix a, Matrix b, Matrix ar, Matrix br)
{
   return 1;
}
*/

/* UNUSED
static int
SymmetricMatrixGeneralEigenModes(Matrix a, Matrix b, Matrix lambda,
                                 Matrix x, unsigned maxit)
{
   Matrix	diag;
   Matrix	sub_diag;
   Matrix	a_red;
   Matrix	b_chol;
   unsigned	n;
   int		status;

   n = Mrows(a);

   if (IsCompact(x))
      return M_COMPACT;

   if (!IsSquare(x))
      return M_NOTSQUARE;

   if (Mrows(a) != Mrows(x) || Mcols(a) != Mcols(x))
      return M_SIZEMISMATCH;

   if (Mrows(a) != Mrows(b) || Mcols(a) != Mcols(b))
      return M_SIZEMISMATCH;

   if (!IsColumnVector(lambda))
      return M_NOTCOLUMN;

   if (Mrows(a) != Mrows(lambda))
      return M_SIZEMISMATCH;

   if (maxit == 0)
      maxit = DEFAULT_MAXIT;
 
   diag     = lambda;
   sub_diag = CreateColumnVector (n);
   a_red  = CreateCopyMatrix (a);
   b_chol = CreateCopyMatrix (b);

   status = CholeskyReduction (a, b, a_red, b_chol);
   if (status)
      return status;
  
   status = TridiagonalReduction (a_red, diag, sub_diag, x); 
   if (status)
      return status;

   status = TridiagSymmMatrixEigenModes (diag, sub_diag, diag, x, maxit);
   if (status)
      return status;

   DestroyMatrix (sub_diag);

   return 0;
}
*/

int NormalizeByLength (Matrix b, const Matrix a)
{
   double	div;
   unsigned	i, j;
   unsigned	n;

   if (IsCompact(b))
      return M_COMPACT;

   if (!IsSquare(a))
      return M_NOTSQUARE;

   if (Mrows(a) != Mrows(b) || Mcols(a) != Mcols(b))
      return M_SIZEMISMATCH;

   n = Mrows(a);

   for (j = 1 ; j <= n ; j++) {		/* loop over each mode (column) */

      div = 0.0;
 
      for (i = 1 ; i <= n ; i++) 		/* loop over each row	*/
         div += mdata(a,i,j)*mdata(a,i,j);
      
      div = sqrt (div);

      for (i = 1 ; i <= n ; i++) 
         sdata(b, i, j) = mdata(a,i,j) / div;
   }

   return 0;
} 

int NormalizeByFirst (Matrix b, const Matrix a)
{
   double	div;
   unsigned	i, j;
   unsigned	n;

   if (IsCompact(b))
      return M_COMPACT;

   if (!IsSquare(a))
      return M_NOTSQUARE;

   if (Mrows(a) != Mrows(b) || Mcols(a) != Mcols(b))
      return M_SIZEMISMATCH;

   n = Mrows(a);

   for (j = 1 ; j <= n ; j++) {		/* loop over each mode (column) */
      div = mdata(a,1,j);
 
      if (div != 0)
         for (i = 1 ; i <= n ; i++) 
            sdata(b, i, j) = mdata(a,i,j) / div;
   }

   return 0;
} 

int NormalizeByMaximum (Matrix b, const Matrix a, unsigned int keep_sign)
{
   double	max;
   double	div;
   unsigned	i, j;
   unsigned	n;

   if (IsCompact(b))
      return M_COMPACT;

   if (!IsSquare(a))
      return M_NOTSQUARE;

   if (Mrows(a) != Mrows(b) || Mcols(a) != Mcols(b))
      return M_SIZEMISMATCH;

   n = Mrows(a);

   for (i = 1 ; i <= n ; i++) {

      div = mdata(a,1,i);      
      max = fabs (div);
 
      for (j = 2 ; j <= n ; j++) {
         if (fabs (mdata(a,j,i)) > max) {
            div = mdata(a,j,i);
            max = fabs (div);
         }
      }

      if (keep_sign)
         div = max;

      for (j = 1 ; j <= n ; j++) 
         sdata(b, j, i) = mdata(a,j,i) / div;
   }

   return 0;
} 

int BuildTridiagonalVectors (const Matrix a, Matrix diag, Matrix sub_diag)
{
   unsigned	i;
   unsigned	n;

   if (!IsColumnVector(diag) || !IsColumnVector(sub_diag))
      return M_NOTCOLUMN;

   if (!IsSquare(a))
      return M_NOTSQUARE;

   if (Mrows(a) != Mrows(diag) || Mrows(a) != Mrows(sub_diag))
      return M_SIZEMISMATCH;

   n = Mrows(a);

   for (i = 1 ; i <= n ; i++) {
      sdata(diag, i, 1) = mdata(a,i,i);
     
      if (i < n)
         sdata(sub_diag, i, 1) = mdata(a,i+1,i);
   }

   sdata(sub_diag, n, 1) = 0.0;

   return 0;
}

int TridiagonalReduction (const Matrix a, Matrix diag, Matrix sub_diag, Matrix z)
{
   int		jp1;
   double	h, f, g, hh;
   double	scale;
   int		flag;
   
   if (IsCompact(z))
      return M_COMPACT;

   if (!IsColumnVector(diag) || !IsColumnVector(sub_diag))
      return M_NOTCOLUMN;

   if (!IsSquare(a))
      return M_NOTSQUARE;
   
   if (Mrows(a) != Mrows(z) || Mcols(a) != Mcols(z))
      return M_SIZEMISMATCH;

   if (Mrows(a) != Mrows(diag) || Mrows(a) != Mrows(sub_diag))
      return M_SIZEMISMATCH;
   
   const unsigned n = Mrows(a);

   for (size_t i = 1 ; i <= n ; i++) {
      for (size_t j = i ; j <= n ; j++) 
         sdata(z, j, i) = mdata(a,j,i);

      sdata(diag, i, 1) = mdata(a,n,i);
   }

   for (size_t ii = 2 ; ii <= n ; ii++) {

      size_t i = n + 2 - ii;
      size_t l = i - 1;
      h = 0.0;
      scale = 0.0;

      flag = 0;

      if (l >= 2) {
         for (size_t k = 1 ; k <= l ; k++)
            scale += fabs (mdata(diag,k,1));

         if (scale != 0.0)
            flag = 1;
      }

      if (!flag) {
         sdata(sub_diag, i, 1) = mdata(diag, l, 1);

         for (size_t j = 1 ; j <= l ; j++) {
            sdata(diag, j, 1) = mdata(z,l,j);
            sdata(z, i, j) = 0.0;
            sdata(z, j, i) = 0.0;
         }

         sdata(diag, i, 1) = h;
         continue;
      }

      for (size_t k = 1 ; k <= l ; k++) {
         sdata(diag, k, 1) = mdata(diag,k,1) / scale;
         h += mdata(diag,k,1)*mdata(diag,k,1);
      }

      f = mdata(diag,l,1);
      g = -SIGN(sqrt(h), f);
      sdata(sub_diag, i, 1) = scale*g;
      h -= f * g;
      sdata(diag, l, 1) = f - g;

      for (size_t j = 1 ; j <= l ; j++) 
         sdata(sub_diag, j, 1) = 0.0;

      for (size_t j = 1 ; j <= l ; j++) {
         f = mdata(diag,j,1);
         sdata(z, j, i) = f;
         g = mdata(sub_diag,j,1) + mdata(z,j,j) * f;

         jp1 = j + 1;

         if (l < jp1) {
            sdata(sub_diag, j, 1) = g;
            continue;
         }

         for (size_t k = jp1 ; k <= l ; k++) {
            g += mdata(z,k,j) * mdata(diag,k,1);
            sdata(sub_diag, k, 1) = mdata(sub_diag,k,1) + mdata(z,k,j) * f;
         }

         sdata(sub_diag, j, 1) = g;
      }

      f = 0.0;

      for (size_t j = 1 ; j <= l ; j++) {
         sdata(sub_diag, j, 1) = mdata(sub_diag,j,1) / h;
         f += mdata(sub_diag,j,1) * mdata(diag,j,1);
      }

      hh = f / (h + h);

      for (size_t j = 1 ; j <= l ; j++)
         sdata(sub_diag, j, 1) = mdata(sub_diag,j,1) - hh * mdata(diag,j,1);

      for (size_t j = 1 ; j <= l ; j++) {
         f = mdata(diag,j,1);
         g = mdata(sub_diag,j,1);

         for (size_t k = j ; k <= l ; k++) 
            sdata(z, k, j) = mdata(z,k,j) - 
                             f*mdata(sub_diag,k,1) - g*mdata(diag,k,1);

         sdata(diag, j, 1) = mdata(z,l,j);
         sdata(z, i, j) = 0.0;
      }

      sdata(diag, i, 1) = h;
   }

   for (size_t i = 2 ; i <= n ; i++) {
      size_t l = i - 1;
      sdata(z, n, l) = mdata(z,l,l);
      sdata(z, l, l) = 1.0;
      h = mdata(diag,i,1);         

      if (h == 0.0) {
         for (size_t k = 1 ; k <= l ; k++)
            sdata(z, k, i) = 0.0;

         continue;
      }

      for (size_t k = 1 ; k <= l ; k++)
         sdata(diag, k, 1) = mdata(z,k,i) / h;

      for (size_t j = 1 ; j <= l ; j++) {
         g = 0.0;

         for (size_t k = 1 ; k <= l ; k++)  
            g += mdata(z,k,i) * mdata(z,k,j);

         for (size_t k = 1 ; k <= l ; k++)
            sdata(z, k, j) = mdata(z,k,j) - g*mdata(diag,k,1);
      }

      for (size_t k = 1 ; k <= l ; k++)
         sdata(z, k, i) = 0.0;
   }

   for (size_t i = 1 ; i <= n ; i++) {
      sdata(diag, i, 1) = mdata(z,n,i);
      sdata(z, n, i) = 0.0;
   }

   sdata(z, n, n) = 1.0;

   for (size_t i = 2 ; i <= n ; i++)
      sdata(sub_diag, i-1, 1) = mdata(sub_diag, i, 1);

   sdata(sub_diag, n, 1) = 0.0;

   return 0;
}
