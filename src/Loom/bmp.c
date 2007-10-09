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

/****************************************************************************
 *
 * File:         bmp.c
 *
 * Description:  
 *
 *****************************************************************************/

# include <stdio.h>
# include "bmp.h"
# include "error.h"

static int architecture (void)
{
   int  x = 1;

   if (*((char *) &x) == 1)
      return 0;
   else
      return 1;
}

static short SwapShort (short int x)
{
   char         *ptr;
   char         buffer [2];

   ptr = (char *) &x;

   buffer [1] = ptr [0];
   buffer [0] = ptr [1];

   ptr = buffer;
   return *((short *) ptr);
}

static long SwapLong (int x)
{
   char         *ptr;
   char         buffer [4];

   ptr = (char *) &x;

   buffer [3] = ptr [0];
   buffer [2] = ptr [1];
   buffer [1] = ptr [2];
   buffer [0] = ptr [3];

   ptr = buffer;
   return *((int *) ptr);
}

void ImageDataToBMP(char *out, unsigned char **image, int rows, int cols,
                    unsigned char *red, unsigned char *green, unsigned char *blue)
{
   FILE		*fp;
   int 		i;
   short	sx;
   long		lx;	
   int		nbytes;
   int		a;

# ifndef DOS
   fp = fopen(out, "w");
# else
   fp = fopen(out, "wb");
# endif

   if (fp == NULL)
      Fatal ("temporary file error -> bitmap");

   a = architecture ( );

   putc('B', fp);
   putc('M', fp);

   nbytes = cols;
   while (nbytes % 4)
      nbytes ++;

   lx = rows*nbytes + 4*256 + 40 + 14;	/* file length */
   fwrite((long *) &lx, sizeof(long), 1, fp); 

   sx = 0;			/* reserved */
   if (a) sx = SwapShort(sx);
   fwrite((short *) &sx, sizeof(short), 1, fp); 
   fwrite((short *) &sx, sizeof(short), 1, fp); 

   lx = 4*256 + 40 + 14;	/* offbits */
   if (a) lx = SwapLong(lx);
   fwrite((long *) &lx, sizeof(long), 1, fp); 

   lx = 40; 			/* infoheader size */
   if (a) lx = SwapLong(lx);
   fwrite((long *) &lx, sizeof(long), 1, fp); 
   lx = cols; 
   if (a) lx = SwapLong(lx);
   fwrite((long *) &lx, sizeof(long), 1, fp); 
   lx = rows; 
   if (a) lx = SwapLong(lx);
   fwrite((long *) &lx, sizeof(long), 1, fp); 

   sx = 1;			/* planes 	*/
   if (a) sx = SwapShort(sx);
   fwrite((short *) &sx, sizeof(short), 1, fp); 
   sx = 8;			/* bits per pixel */
   if (a) sx = SwapShort(sx);
   fwrite((short *) &sx, sizeof(short), 1, fp); 

   lx = 0; 			/* compression */
   if (a) lx = SwapLong(lx);
   fwrite((long *) &lx, sizeof(long), 1, fp); 
   lx = cols * rows;		/* image size */
   if (a) lx = SwapLong(lx);
   fwrite((long *) &lx, sizeof(long), 1, fp); 
   lx = 0;			/* horizontal resolution */
   if (a) lx = SwapLong(lx);
   fwrite((long *) &lx, sizeof(long), 1, fp); 
   lx = 0;			/* vertical resolution */
   if (a) lx = SwapLong(lx);
   fwrite((long *) &lx, sizeof(long), 1, fp); 
   lx = 256;			/* colors used */
   if (a) lx = SwapLong(lx);
   fwrite((long *) &lx, sizeof(long), 1, fp); 
   lx = 256;			/* colors that are important */
   if (a) lx = SwapLong(lx);
   fwrite((long *) &lx, sizeof(long), 1, fp); 

   for (i = 0 ; i < 256 ; i++) {
      putc(blue[i], fp);
      putc(green[i], fp);
      putc(red[i], fp);
      putc(0, fp);
   }

   for (i = rows-1 ; i >= 0 ; i--) {
      fwrite((unsigned char *) image[i], sizeof(unsigned char), cols, fp);
      nbytes = cols;
      while (nbytes % 4) {
         putc(0, fp);
         nbytes ++;
      }
   }

   fclose(fp);

   return;
}
