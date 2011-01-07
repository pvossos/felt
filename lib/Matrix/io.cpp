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

# include <math.h>
# include <stdio.h>
# include <string.h>
# include "matrix.h"

typedef struct {
   long	type;
   long	mrows;
   long ncols;
   long imagf;
   long namlen;
} MATheader;

typedef union {
   double	    r8;
   float	    r4;
   int		    i4;
   short	    i2;
   unsigned short   u2;
   unsigned char    b;
} DataVal;

static int architecture (void)
{
   int	x = 1;

   if (*((char *) &x) == 1)
      return 0;
   else
      return 1;
}

static float SwapFloat (float x)
{
   char		*ptr;
   char		buffer [4];

   ptr = (char *) &x;

   buffer [3] = ptr [0];
   buffer [2] = ptr [1];
   buffer [1] = ptr [2];
   buffer [0] = ptr [3];

   ptr = buffer;
   return *((float *) ptr);
}

static long SwapLong (long int x)
{
   char		*ptr;
   char		buffer [4];

   ptr = (char *) &x;

   buffer [3] = ptr [0];
   buffer [2] = ptr [1];
   buffer [1] = ptr [2];
   buffer [0] = ptr [3];

   ptr = buffer;
   return *((long *) ptr);
}

/* UNUSED
static short SwapShort (short int x)
{
   char		*ptr;
   char		buffer [2];

   ptr = (char *) &x;

   buffer [1] = ptr [0];
   buffer [0] = ptr [1];

   ptr = buffer;
   return *((short *) ptr);
}
*/

 /* UNUSED
static unsigned short SwapUnsignedShort (short unsigned int x)
{
   char		*ptr;
   char		buffer [2];

   ptr = (char *) &x;

   buffer [1] = ptr [0];
   buffer [0] = ptr [1];

   ptr = buffer;
   return *((unsigned short *) ptr);
}
 */

static double SwapDouble (double x)
{
   char		*ptr;
   char		buffer [8];

   ptr = (char *) &x;

   buffer [7] = ptr [0];
   buffer [6] = ptr [1];
   buffer [5] = ptr [2];
   buffer [4] = ptr [3];
   buffer [3] = ptr [4];
   buffer [2] = ptr [5];
   buffer [1] = ptr [6];
   buffer [0] = ptr [7];

   ptr = buffer;
   return *((double *) ptr);
}

static int ReadMAT (FILE *fp, Matrix *a, char **name)
{
   unsigned	count;
   MATheader	h;
   int		loc_arch;
   int		rem_arch; 
   int		m, o, p, t;
   DataVal	x;
   char		buffer [256];

   loc_arch = architecture ( );

   count = 1;

   fread (&h, sizeof(MATheader), 1, fp);
   if (h.type < 0 || h.type > 4502)
      h.type = SwapLong (h.type);

   m = h.type / 1000;
   o = (h.type - m*1000) / 100;
   p = (h.type - m*1000 - o*100) / 10;
   t = h.type - m*1000 - o*100 - p*10; 

   if (m > 1 || o != 0 || t != 0)
      return 0;

   rem_arch = m;

   if (rem_arch != loc_arch) {
      h.mrows = SwapLong (h.mrows);
      h.ncols = SwapLong (h.ncols);
      h.imagf = SwapLong (h.imagf);
      h.namlen = SwapLong (h.namlen);
   }

   if (h.imagf)
      return 0;
 
   fread (buffer, sizeof(char), h.namlen, fp);
   if (name != NULL)
      *name = strdup (buffer);
   
   *a = CreateMatrix (h.mrows, h.ncols); 

   switch (p) {

   case 0:	/* double */
      for (int i = 1 ; i <= h.ncols ; i++) {
         for (int j = 1 ; j <= h.mrows ; j++) {
            fread (&(x.r8), sizeof(double), 1, fp);
            if (loc_arch != rem_arch)
               x.r8 = SwapDouble (x.r8);

            sdata(*a, j, i) = x.r8;
         }
      }
      break;

   case 1:	/* float */
      for (int i = 1 ; i <= h.ncols ; i++) {
         for (int j = 1 ; j <= h.mrows ; j++) {
            fread (&(x.r4), sizeof(float), 1, fp);
            if (loc_arch != rem_arch)
               x.r4 = SwapFloat (x.r4);

            sdata(*a, j, i) = x.r4;
         }
      }
      break;

   case 2:	/* int */
      for (int i = 1 ; i <= h.ncols ; i++) {
         for (int j = 1 ; j <= h.mrows ; j++) {
            fread (&(x.i4), sizeof(int), 1, fp);
            if (loc_arch != rem_arch)
               x.i4 = SwapFloat (x.i4);

            sdata(*a, j, i) = x.i4;
         }
      }
      break;
 
   case 3:	/* short */
      for (int i = 1 ; i <= h.ncols ; i++) {
         for (int j = 1 ; j <= h.mrows ; j++) {
            fread (&(x.i2), sizeof(short), 1, fp);
            if (loc_arch != rem_arch)
               x.i2 = SwapFloat (x.i2);

            sdata(*a, j, i) = x.i2;
         }
      }
      break;
 
   case 4:	/* unsigned short */ 
      for (int i = 1 ; i <= h.ncols ; i++) {
         for (int j = 1 ; j <= h.mrows ; j++) {
            fread (&(x.u2), sizeof(unsigned short), 1, fp);
            if (loc_arch != rem_arch)
               x.u2 = SwapFloat (x.u2);

            sdata(*a, j, i) = x.u2;
         }
      }
      break;

   case 5:	/* unsigned char */
      for (int i = 1 ; i <= h.ncols ; i++) {
         for (int j = 1 ; j <= h.mrows ; j++) {
            fread (&(x.b), sizeof(unsigned char), 1, fp);
            if (loc_arch != rem_arch)
               x.b = SwapFloat (x.b);

            sdata(*a, j, i) = x.b;
         }
      }
      break;

   default:
      break;
   }
   
   return count;
}
 
static void WriteMAT (Matrix a, FILE *fp, const char *name, int arch)
{
   long		mopt;
   double	x;
   unsigned	i, j;
   MATheader	h;

   mopt = arch*1000 + 0*100 + 0*10 + 0*1;
                      /* reserved */
                              /* double precision */
                                     /* numeric full matrix */
   h.type = mopt;
   h.mrows = Mrows(a);
   h.ncols = Mcols(a);
   h.imagf = 0;
   h.namlen = strlen(name) + 1;
       
   fwrite (&h, sizeof(MATheader), 1, fp);
   fwrite (name, sizeof(char), h.namlen, fp);
   
   for (i = 1 ; i <= Mcols(a) ; i++) {
      for (j = 1 ; j <= Mrows(a) ; j++) {
         x = mdata(a,j,i);
         fwrite (&x, sizeof(double), 1, fp);
      }
   }

   return;
}


int MatrixToMatlab (const Matrix a, FILE *fp, const char *name)
{
   int		arch;
  
   arch = architecture ( );
 
   WriteMAT (a, fp, name, arch);

   return 0;
}

int MatricesToMatlab (const Matrix *a, unsigned int n, FILE *fp, const char **name)
{
   int		arch;
   unsigned	i;

   arch = architecture ( );

   for (i = 1 ; i <= n ; i++)
      WriteMAT (a [i], fp, name [i], arch);

   return 0;
}

Matrix MatlabToMatrix (FILE *fp)
{
   Matrix	a;
   int		status;

   status = ReadMAT (fp, &a, NULL);
   if (status == 0)
      return NullMatrix;

   return a;
}
