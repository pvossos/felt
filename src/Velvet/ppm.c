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
# include <X11/Intrinsic.h>
# include "proto.h"
# include "error.h"

#define RAWBITS	1


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

void WidgetToPPM (filename, w)
   char		*filename;
   Widget	 w;
{
   XImage	*img;
   XColor	*colors;
   int		 ncells;
   int		 color;
   int		 height, width;
   unsigned	 i,j;
   FILE		*fp;

   fp = fopen (filename, "w");
   if (fp == NULL) {
      error ("could not open %s for writing", filename);
      return;
   }

   img = WidgetToXImage(w, &colors, &ncells);

   width = img -> width;
   height = img -> height;

   WritePPMHeader (fp, filename, width, height, RAWBITS);

   if (RAWBITS) {
      for (i = 0 ; i < height ; i++) {
         for (j = 0 ; j < width ; j++) {
            color = XImageCellXY(img, j, i, colors, ncells);
            WriteRawPPMPixel (fp, colors [color].red/256, 
                                  colors [color].green/256, 
                                  colors [color].blue/256);
         }
      }
   }
   else {
      for (i = 0 ; i < height ; i++) {
         for (j = 0 ; j < width ; j++) {
            color = XImageCellXY(img, j, i, colors, ncells);
            WriteRawPPMPixel (fp, colors [color].red/256, 
                                  colors [color].green/256, 
                                  colors [color].blue/256);
         }
      }
   }

   ClosePPM (fp, RAWBITS);

   return;
}
