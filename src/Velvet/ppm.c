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

/*****************************************************************************
 *
 * File:	ppm.c
 *
 * Description:	a really basic set of routines to write a PPM image file
 *		from an ximage structure.  I got the PPM format straight 
 *		from the ppm(5) man page and from looking at examples
 *		in the PBMPLUS source code.
 *		
 *****************************************************************************/

# include <stdio.h>
# include <X11/Xlib.h>
# include <X11/Xutil.h>
# include "colormap.h"
# include "proto.h"
# include "error.h"

#define MAXCOLORS 130
#define RAWBITS	1

static XImage	*img;
static int	*rev_pixel;
static int	black;
static int	white;

static int GetPixel( x, y )
   int x, y;
{
   long		point;
   int 		color;

   point = XGetPixel (img, x, y);

   if (point == white)
      color = 129;
   else if (point == black)
      color = 128;
   else
      color = rev_pixel [point];

   return color;
}

static int 	pixel_count;

static void WritePPMHeader (fp, filename, width, height, rawbits)
   FILE		*fp;
   char		*filename;
   unsigned	width;
   unsigned	height;
   unsigned	rawbits;
{
   fprintf (fp, "%s\n", rawbits ? "P6" : "P3");
   fprintf (fp, "# %s\n", filename);
   fprintf (fp, "%d %d\n", width, height);
   fprintf (fp, "255\n");

   pixel_count = 0;

   return;
}

static void WritePPMPixel (fp, r, g, b)
   FILE		*fp;
   int		r;
   int		g;
   int		b;
{
   fprintf (fp, "%3d %3d %3d ", r, g, b);

   pixel_count ++;
   if (pixel_count == 4) {
      pixel_count = 0;
      fprintf (fp, "\n");
   }

   return;
}

static void WriteRawPPMPixel (fp, r, g, b)
   FILE		*fp;
   int		r;
   int		g;
   int		b;
{
   char		array [3];

   array [0] = r;
   array [1] = g;
   array [2] = b;

   fwrite ((char *) array, sizeof (char), 3, fp);

   return;
}

static void ClosePPM (fp, rawbits)
   FILE		*fp;
   int rawbits;
{
   if (rawbits)
      fprintf (fp, "\n");

   fclose (fp);

   return;
}

void XImageToPPM (filename, ximage, ncells, rev, bl, wh)
   char		*filename;
   XImage	*ximage;
   unsigned	ncells;
   int		*rev;
   int		bl;
   int		wh;
{
   int		color;
   int		height, width;
   unsigned	i,j;
   FILE		*fp;
   int	 	Red [MAXCOLORS], 
		Green [MAXCOLORS], 
		Blue [MAXCOLORS];

   fp = fopen (filename, "w");
   if (fp == NULL) {
      error ("could not open %s for writing", filename);
      return;
   }

   for (i = 0 ; i < ncells ; i++) {
      Red [i] = color_values[i].r / 256;
      Green [i] = color_values[i].g / 256;
      Blue [i] = color_values[i].b / 256;
   }

   Red [128]   = 0;
   Green [128] = 0;
   Blue [128]  = 0;

   Red [129]   = 255;
   Green [129] = 255;
   Blue[129]   = 255;

   img = ximage;
   rev_pixel = rev;
   black = bl;
   white = wh;

   width = img -> width;
   height = img -> height;

   WritePPMHeader (fp, filename, width, height, RAWBITS);

   if (RAWBITS) {
      for (i = 0 ; i < height ; i++) {
         for (j = 0 ; j < width ; j++) {
            color = GetPixel (j, i);
            WriteRawPPMPixel (fp, Red [color], Green [color], Blue [color]);
         }
      }
   }
   else {
      for (i = 0 ; i < height ; i++) {
         for (j = 0 ; j < width ; j++) {
            color = GetPixel (j, i);
            WritePPMPixel (fp, Red [color], Green [color], Blue [color]);
         }
      }
   }

   ClosePPM (fp, RAWBITS);

   return;
}
