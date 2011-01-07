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
 * File:	spectral.c
 *
 * Description:	Contains code to implement spectral analysis features 
 *		for transient problems in FElt.  Just to confuse everybody
 *		we use a nice mix of FElt Vector types and regular old
 *		pointers to double; for even more fun, we'll try to mix
 *		and match zero- and unit-offset pointer arithmetic,
 *		and throw in the odd reference to the ComplexMatrix type.
 *
 ***************************************************************************/

# include <vector>
# include <stdio.h>
# include <math.h>
# include "fe.h"
# include "allocate.h"
# include "error.h"
# include "problem.h"
# include "cmatrix.h"
# include "cvector1.hpp"

# define BLACKMAN	1
# define HAMMING	2
# define HANNING	3
# define PARZEN		4
# define SQUARE		5
# define TRIANGULAR	6
# define WELCH		7

using std::vector;

static int
RoundToPowerOf2(int n, int *exponent)
{
   int	i;
   int	n1;

   n1 = n;

   i = 0;
   while (n1 >>= 1)
      i ++;

   if (1 << i != n) {
      i++;
      n = 1 << i;
   }

   if (exponent != NULL)
      *exponent = i;

   return n;
} 

/****************************************************************************
 *
 * Function:	ZeroPadData
 *
 * Description:	given an input vector of length n, return a point to 
 *		an array of doubles which had been padded out to the 
 *		length that is the nearest power of two.	
 *
 ***************************************************************************/

static vector<double>
ZeroPadData(Vector x, int *length, int *length2)
{
   int i;
   int n = RoundToPowerOf2 (Mrows(x), &i);

   *length2 = i;
   *length = n;

   vector<double> xt(n);
   
   for (i = 1 ; i <= n ; i++) {
      if (i <= Mrows(x))
         xt[i-1] = mdata(x,i,1);
      else
         xt[i-1] = 0.0;
   }

   return xt;
}

/****************************************************************************
 * 
 * Function:	bit_swap and sswap
 *
 * Description:	a couple of quicky convenience routines, also from the
 *		xmgr code in fourier.c, to make the FFT routine easier
 *
 ****************************************************************************/

static int
bit_swap(int i, int nu)
{
   int ib, i1, i2;

   ib = 0;

   for (i1 = 0; i1 != nu; i1++) {
      i2 = i / 2;
      ib = ib * 2 + i - 2 * i2;
      i = i2;
   }

   return (ib);
}

static void
sswap(double *x1, double *x2)
{
   double 	temp_x;

   temp_x = *x1;
   *x1 = *x2;
   *x2 = temp_x;
}

/****************************************************************************
 * 
 * Function:	FastFourierTransform
 *
 * Description:	computes the forward or inverse fast fourier transform
 *		of a given input.  The length of the input must be a power
 *		of two (use ZeroPadData if necessary).  I basically took
 *		this straight from the xmgr source (fourier.c).  He wasn't
 *		quite sure of the original attribution so I guess I'm 
 *		not either. 
 *
 ****************************************************************************/

/* real part of signal		*/
/* imaginary part of signal	*/
/* length of signal		*/
/* log base 2 of n		*/
/* 1 = forward, -1 = reverse	*/
int
FastFourierTransform(double *Xr, double *Xi, int n, int n2, int direction)
{
   int 		nhalf; 
   int		j, l, i, ib, k, k1, k2;
   double 	tr, ti, arg, n2m1;	/* intermediate values 		*/
   double 	c, s;			/* cosine & sine components 	*/
   double 	fac;

   nhalf = n / 2;
   n2m1 = (double) n2 - 1.0;
   k = 0;

	/*
	 * Calculate the components of the Fourier series of the function
	 */

   for (l = 0; l != n2; l++) {
      do {
         for (i = 0; i != nhalf; i++) {
	    j = k / (int) (pow(2.0, n2m1));
	    ib = bit_swap(j, n2);
	    arg = 2.0 * M_PI * ib / n;
	    c = cos(arg);
	    s = direction * sin(arg);
	    k1 = k;
	    k2 = k1 + nhalf;
	    tr = *(Xr + k2) * c + *(Xi + k2) * s;
	    ti = *(Xi + k2) * c - *(Xr + k2) * s;
	    *(Xr + k2) = *(Xr + k1) - tr;
	    *(Xi + k2) = *(Xi + k1) - ti;
	    *(Xr + k1) = *(Xr + k1) + tr;
	    *(Xi + k1) = *(Xi + k1) + ti;
	    k++;
	 }

	 k += nhalf;

      } while (k < n - 1);

      k = 0;
      n2m1 -= 1.0;
      nhalf /= 2;
   }

   for (k = 0; k != n; k++) {
      ib = bit_swap(k, n2);
      if (ib > k) {
         sswap((Xr + k), (Xr + ib));
         sswap((Xi + k), (Xi + ib));
      }
   }

	/*
	 * If calculating the inverse transform, must divide the data 
         * by the number of data points.
	 */

   if (direction == -1)
      fac = 2.0 / n;
   else
      fac = 0.5;

   for (k = 0; k != n; k++) {
      *(Xr + k) *= fac;
      *(Xi + k) *= fac;
   }

   return 0;
}

static vector<double>
WindowFunction(unsigned win, unsigned n)
{
   unsigned	i;

   vector<double> w(n);

   for (i = 0 ; i < n ; i++) {
      switch (win) {
         case SQUARE:
            w[i] = 1.0;
            break;

         case TRIANGULAR:
            w[i] = 1.0 - fabs((i - 0.5 * (n - 1.0)) / (0.5 * (n - 1.0)));
            break;

         case HANNING:
            w[i] = 0.5 - 0.5 * cos(2.0 * M_PI * i / (n - 1.0));
            break;

         case WELCH:
            w[i] = 1.0 - pow((i - 0.5 * (n - 1.0)) / (0.5 * (n + 1.0)), 2.0);
            break;

         case HAMMING:
            w[i] = 0.54 - 0.46 * cos(2.0 * M_PI * i / (n - 1.0));
            break;

         case BLACKMAN:
            w[i] = 0.42 - 0.5 * cos(2.0 * M_PI * i / (n - 1.0)) + 0.08 * cos(4.0 * M_PI * i /(n - 1.0));
            break;

         case PARZEN: 
            w[i] = 1.0 - fabs((i - 0.5 * (n - 1)) / (0.5 * (n + 1)));
            break;
      }
   }

   return w;
}

int 
Spectrum(Vector x, Vector *P, Vector *F, double delta_t, int nfft)
{
   int		n, nt, n2;
   int		nfft2;
   int		np;
   int		overlap;
   int		windows;
   double	factor;

   vector<double> Xr = ZeroPadData (x, &n, &n2);

	/*
	 * figure out the things we need to window the data
	 */

   nt = nfft;
   nfft2 = 0;
   while (nt >>= 1)
      nfft2++;

   overlap = nfft/2;

   windows = (int) ((n - overlap) / (nfft - overlap));

   vector<double> xr(nfft);
   vector<double> xi(nfft, 0);
   vector<double> p(nfft, 0);

   vector<double> w = WindowFunction(HANNING, nfft);

   for (int i = 0 ; i < windows ; i++) {
      for (int j = 0 ; j < nfft ; j++)
         xr [j] = Xr[i*(nfft - overlap) + j] * w[j];

      FastFourierTransform(&xr[0], &xi[0], nfft, nfft2, 1);
      
      for (int j = 0 ; j < nfft ; j++) 
         p [j] += xr[j]*xr[j] + xi[j]*xi[j];
   }

	/*
	 * compute the normalization factor
	 */

   factor = 0.0;
   for (int i = 0 ; i < nfft ; i++)
      factor += w[i]*w[i];

   factor *= windows;

	/*
	 * create our output vectors and calculate final results
	 */

   if (nfft % 2 == 0)
      np = nfft/2 + 1;
   else
      np = (nfft + 1)/2;

   *P = CreateVector (np);

   if (F != NULL)
      *F = CreateVector (np);

   for (int j = 0 ; j < np ; j++) {
      sdata((*P), j+1, 1) = p[j] / factor;

      if (F != NULL)
         sdata((*F), j+1, 1) = j/delta_t/nfft;
   }

   return 0;
}

int
ComputeOutputSpectraFFT(Matrix dtable, Matrix *Pr, Vector *Fr, int nfft)
{
   Matrix	Pm;
   Vector	P;
   Vector	F;
   Vector	x;
   unsigned	i,j;
   
   x = CreateVector(Mrows(dtable));
   Pm = NullMatrix; /* gcc -Wall */

   for (i = 1 ; i <= Mcols(dtable) ; i++) {
      for (j = 1 ; j <= Mrows(dtable) ; j++)
         sdata(x, j, 1) = mdata(dtable,j,i);
      
      if (i == 1) {
         Spectrum(x, &P, &F, analysis.step, nfft);
         Pm = CreateMatrix(Mrows(P), Mcols(dtable));
         *Fr = F;
      }
      else
         Spectrum(x, &P, NULL, analysis.step, nfft);

      for (j = 1 ; j <= Mrows(P) ; j++)
         sdata(Pm, j, i) = mdata(P, j, i);         

      DestroyMatrix(P);
   }

   *Pr = Pm;

   DestroyMatrix (x);

   return 0; 
}

cvector1<Matrix>
ComputeTransferFunctions(Matrix M, Matrix C, Matrix K, const cvector1<NodeDOF> &forced)
{
   ComplexMatrix	Z;
   ComplexMatrix	Ht;
   double		w;
   unsigned		i,j,k;
   unsigned		input;
   unsigned		n;
   unsigned		size;
   unsigned		nsteps;
  
   n = Mrows(M);
   size = Msize(M);
 
   Z = CreateCompactComplexMatrix (n, n, size, M -> diag);
   Ht = CreateComplexColumnVector(n);

   nsteps = (analysis.stop - analysis.start + analysis.step/2.0) / 
            analysis.step + 1.0;

   const size_t numforced = forced.size();
   cvector1<Matrix> H(numforced);

   for (i = 1 ; i <= numforced ; i++)
      H [i] = CreateFullMatrix(nsteps, analysis.numdofs * analysis.numnodes);

   w = analysis.start;
   for (j = 1 ; j <= nsteps ; j++) {
      for (i = 1 ; i <= size ; i++) {
         rdata(Z, i, 1) = K -> data [i][1] - w*w*M -> data [i][1];
         idata(Z, i, 1) = w*C -> data [i][1];
      }
      
      CroutFactorComplexMatrix (Z);

      for (input = 1 ; input <= numforced ; input++) {

         InvertCroutComplexMatrix (Ht, Z, 
                    GlobalDOF(forced [input].node -> number, 
                              forced [input].dof));

         for (i = 1 ; i <= analysis.numnodes ; i++) {
            for (k = 1 ; k <= analysis.numdofs ; k++) {
               sdata(H [input], j, (i-1)*analysis.numdofs + k) = 
                  modulus(cmdata(Ht, GlobalDOF(analysis.nodes [i] -> number,
                                               analysis.dofs [k]), 1));
            }
         }
      }

      w += analysis.step;
   }

   DestroyComplexMatrix (Z);
   DestroyComplexMatrix (Ht);

   return H; 
}

/* UNUSED
static void
AlignSpectra(Matrix S1, Matrix S2, Matrix freq2)
{
   unsigned	i, j, k;
   double	df;
   double	f;
   double	start, end, inc;


   start = analysis.start;
   end   = analysis.stop;
   inc   = analysis.step;
 
   i = 1;
   for (f = start, k = 1; f <= end + inc/2 ; f += inc, k++) {
      for (j = i ; j < Mrows(S2) ; j++) {

         if (mdata(freq2,j,1) == f) {
            sdata(S1, k, 1) = mdata(S2,j,1);
            i = j + 1;
            break;
         }
         else if (mdata(freq2,j+1,1) == f) {
            sdata(S1, k, 1) = mdata(S2,j+1,1);
            i = j + 1;
            break;
         }
         else if (mdata(freq2,j,1) < f && f < mdata(freq2,j+1,1)) {
            df = mdata(freq2,j+1,1) - f;
            sdata(S1, k, 1) = mdata(S2,j,1) + 
                              (mdata(S2,j+1,1) - mdata(S2,j,1))/
                              (mdata(freq2,j+1,1) - mdata(freq2,j,1))*df; 
            i = j + 1;
            break;
         }
      }
   }
     
   return; 
}
*/

Matrix
ComputeOutputSpectra(const cvector1<Matrix> &H, const cvector1<NodeDOF> &forced)
{
   Matrix	So;
   Matrix	Si;
   unsigned	nsteps;
   unsigned	i, j, k;
   unsigned	idof; 
   Node 	inode;
   Force	f;
   double	w;
   double	start, end, inc;

   start = analysis.start;
   end   = analysis.stop;
   inc   = analysis.step;

   nsteps = (end - start + inc/2.0) / inc + 1.0;

   So = CreateFullMatrix (nsteps, analysis.numnodes * analysis.numdofs);
   Si = CreateColumnVector (nsteps);

   ZeroMatrix (So);

   for (i = 1 ; i <= forced.size() ; i++) {

      inode = forced [i].node;
      idof = forced [i].dof;

      f = inode -> force;

      if (f -> spectrum [idof].expr) {
         for (w = start, j = 1 ; j <= nsteps ; w += inc, j++) 
            sdata(Si, j, 1) = EvalCode (f -> spectrum [idof].expr, w);
      }
      else if (f -> spectrum [idof].value) {
         for (w = start, j = 1 ; j <= nsteps ; w += inc, j++)
            sdata(Si, j, 1) = f -> spectrum [idof].value;
      }
      else {
         error ("warning: no frequency domain forces at node %d, DOF %d",
                inode -> number, idof);
         continue;
      }

      for (k = 1 ; k <= Mcols(So) ; k++) 
         for (j = 1 ; j <= Mrows(H[i]) ; j++) 
            sdata(So, j, k) += (mdata(H[i],j,k)*mdata(H[i],j,k)) * mdata(Si,j,1);
   }

   DestroyMatrix (Si);

   return So;
}
