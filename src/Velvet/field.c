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

/****************************************************************************
 * 
 * File:	field.c
 *
 ***************************************************************************/

# include <stdio.h>
# include <string.h>
# include <ctype.h>
# include <X11/Xlib.h>
# include <X11/Xutil.h>
# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include "Solution.h"
# include "FileDialog.h"
# include "util.h"
# include "fe.h"
# include "Drawing.h"
# include "allocate.h"
# include "error.h"
# include "procedures.h"

# define HORIZ	1
# define VERT	2

extern Widget toplevel;
extern FileDialog	dumpd;

Widget CreateDrawingShell ( );

# include "colormap.h"

# define NUMCELLS		128
# define MaxNodesPerElement 	24

static unsigned		numcells;
static unsigned long	pixel [NUMCELLS];   	
static unsigned		rev_pixel [256];
static Pixmap		pixmap;
static XImage		*ximage;

static Widget		stressShell;
static Widget		stress_dw;
static Pixmap		s_pixmap;
static XImage		*s_ximage = NULL;

static Widget		displShell;
static Widget		displ_dw;
static Pixmap		d_pixmap;
static XImage		*d_ximage = NULL;

static void DumpContourImage (pixmap, img, msg)
   Pixmap	pixmap;
   XImage	*img;
   char		*msg;
{
   String	save_file;
   Display	*display;
   XImage	*tmp;
   String	format;

   FileDialogSetToggles (dumpd, "PPM", "EPS");
   CenterOnScreen (XtNameToWidget (toplevel, "fileDialog"), True);
   WarpToCenter (XtNameToWidget (toplevel, "fileDialog"));
   FileDialogSelect (dumpd, msg, "Save file name:", "", &save_file, &format);

   if (save_file == NULL)
      return;

   display = XtDisplay (toplevel);
   tmp = XGetImage (display, pixmap, 0, 0, img -> width, 
                    img -> height, -1, ZPixmap);

   if (strcmp (format, "PPM") == 0)
      XImageToPPM (save_file, tmp, numcells, rev_pixel, 
                   BlackPixel (display, DefaultScreen (display)),
                   WhitePixel (display, DefaultScreen (display)));
   else if (strcmp (format, "EPS") == 0)
      XImageToEPS (save_file, tmp, numcells, rev_pixel, 
                   BlackPixel (display, DefaultScreen (display)),
                   WhitePixel (display, DefaultScreen (display)));

   XDestroyImage (tmp);

   return;
}

static void D_SaveCallback (w, client_data, call_data)
   Widget	w;
   XtPointer	client_data,
		call_data;
{
   DumpContourImage (d_pixmap, d_ximage, "Save Displacement Plot");
}

static void S_SaveCallback (w, client_data, call_data)
   Widget	w;
   XtPointer	client_data,
		call_data;
{
   DumpContourImage (s_pixmap, s_ximage, "Save Stress Plot");
}

static void InitializeColormap ()
{
   static int		initialized = 0;
   Colormap		cmap;
   unsigned long	plane_masks [8];
   Status		result;
   XColor		colors [NUMCELLS];
   unsigned		cell;
   int			ncells;
   int			screen;
   Display		*display;

   if (initialized)
      return;

   display = XtDisplay (toplevel);
   screen = DefaultScreen (display);
   
   cmap = DefaultColormap (display, screen);

   for (ncells = NUMCELLS ; ncells > 1 ; ncells--) {
      result = XAllocColorCells (display, cmap, True, plane_masks, 0,
                                 pixel, ncells);
      if (result) 
         break;
   }

   for (cell = 0 ; cell < ncells ; cell++) {
      colors [cell].red = color_values [cell].r;
      colors [cell].green = color_values [cell].g;
      colors [cell].blue = color_values [cell].b;
      colors [cell].pixel = pixel [cell];
      colors [cell].flags = DoRed | DoGreen | DoBlue;
      
      rev_pixel [pixel [cell]] = cell;
   }

   if (ncells < NUMCELLS / 2)
      error ("only got %d colors - this may look funny", ncells);

   XStoreColors (display, cmap, colors, ncells);
 
   numcells = ncells;

   initialized = 1;
   return;
}

static void HequalImage (white)
   unsigned long	white;
{
   unsigned		i,j;
   long			point;
   long			value;
   unsigned long	histogram [NUMCELLS];
   float		p [NUMCELLS],
			v [NUMCELLS],
			vstar [NUMCELLS],
			vmin, area, sum;
   
   for (i = 0 ; i < numcells ; i++)
      histogram [i] = 0;

   area = 0;
   for (i = 0 ; i < ximage -> height ; i++) {
      for (j = 0 ; j < ximage -> width ; j++) {

         point = XGetPixel (ximage, j, i);
         if (point != white) {
            area++;
            value = rev_pixel [point];         
            histogram [value]++;
         }
      }
   }

   for (i = 0 ; i < numcells ; i++)
      p [i] = histogram [i] / area;

   sum = 0.0;
   vmin = p[0];
   for (i = 0 ; i < numcells ; i++) {
      sum += p[i];
      v[i] = sum;
      if (v[i] < vmin)
         vmin = v[i];
   }

   for (i = 0 ; i < numcells ; i++)
      vstar[i] = ((((v[i] - vmin)/(1.0 - vmin))*(numcells - 1.0)) + 0.5);

   for (i = 0 ; i < ximage -> height ; i++) {
      for (j = 0 ; j < ximage -> width ; j++) {

         point = XGetPixel (ximage, j, i);
         if (point != white) {
            value = rev_pixel [point];
            value = (char) vstar [value]; 
            point = pixel [value];
            XPutPixel (ximage, j, i, point);
         }
      }
   }
}

static void LoadImage (z,width,height,mask,equalize,wedge_orient)
   float	**z;
   Dimension	width,
		height;
   char		**mask;
   Boolean	equalize;
   unsigned	wedge_orient;
{
   unsigned		x_max,y_max,x_min,y_min;
   float		max,min;
   Display		*display;
   Visual		*visual;
   GC			gc;
   unsigned		i,j;
   float		slope;
   int			point;
   unsigned 		flag;
   unsigned long	value;
   char			*ptr;
   char			buffer1 [20];
   char			buffer2 [20];
   int			max_width;
   int			min_width;
   int			sx1,sy1,sx2,sy2;
   XFontStruct		*font;
   Font			fontid;
   
   display = XtDisplay (toplevel);
   visual = DefaultVisual (display, DefaultScreen (display));
   gc = DefaultGC (display, DefaultScreen (display)); 

   x_max = x_min = y_max = y_min = 0;
   max = min = max_width = min_width = 0;

   flag = 0;
   for (i = 0 ; i < height ; i++) {
      for (j = 0 ; j < width ; j++) {
 
         if (mask[i][j]) {
            if (!flag) {
               x_max = x_min = j;
               y_max = y_min = i;
               max = min = z[i][j];
               flag = 1;
            }
            else {
               if (z[i][j] > max)
                  max = z[i][j];
               else if (z[i][j] < min)
                  min = z[i][j];

               if (i > y_max) 
                  y_max = i;
               else if (i < y_min) 
                  y_min = i;
              
               if (j > x_max)
                  x_max = j;
               else if (j < x_min)
                  x_min = j;
            }
         }
      }
   }

   ximage = XCreateImage (display, visual, 8, ZPixmap, 0, 0, 
                          width, height, 8, 0);

   ximage -> data = Allocate (char, width * height);

   slope = (float) numcells / (max - min);

   for (i = 0 ; i < height ; i++) {
      for (j = 0 ; j < width ; j++) {
     
         if (mask[i][j]) {
            if (z[i][j] != max)
               point = (int) (slope*(z[i][j] - min));
            else
               point = numcells - 1; 

            value = pixel [point];
         }
         else
            value = WhitePixel (display, DefaultScreen (display));

         XPutPixel (ximage, j, height - i - 1, value);
      }
   }

   font = XLoadQueryFont (display, "6x9");
   if (font) {
      sprintf (buffer1,"%11.5g",min);
      ptr = buffer1;
      while (isspace (*ptr)) ptr++;
      min_width = XTextWidth (font, ptr, strlen (ptr));
      strcpy (buffer1, ptr);

      sprintf (buffer2,"%11.5g",max);
      ptr = buffer2;
      while (isspace (*ptr)) ptr++;
      max_width = XTextWidth (font, ptr, strlen (ptr));
      strcpy (buffer2, ptr);

      fontid = XLoadFont (display, "6x9");
      XSetFont (display, gc, fontid);
   }

   if (wedge_orient == HORIZ) {
      for (j = x_min ; j <= x_max ; j++) {

         point = (int) ((float) (j-x_min)/
                        (float) (x_max - x_min)*(numcells - 1.0)); 

         for (i = (int) (0.88*height) ; i < height - 12 ; i++)
            XPutPixel (ximage, j, i, pixel [point]);
      }
      sx1 = x_min;
      sx2 = x_max - max_width;
      sy1 = sy2 = height - 2;
   }
   else {
      for (i = y_min ; i <= y_max ; i++) {

         point = (int) ((float) (i-y_min)/
                        (float) (y_max - y_min)*(numcells - 1.0)); 

         for (j = (int) (0.9*width) ; j < (int) (0.98*width) ; j++) 
            XPutPixel (ximage, j, height - i - 1, pixel [point]);
      }
      sy1 = y_max + 10;
      sy2 = y_min - 2;
      sx1 = width - min_width - 1;
      sx2 = width - max_width - 1; 
   }

   if (equalize)
      HequalImage (WhitePixel (display, DefaultScreen (display)));

   XPutImage (display, pixmap, gc, ximage, 0, 0, 0, 0, width, height);

   if (font) {
      XDrawImageString (display, pixmap, gc, sx1,sy1,buffer1,strlen(buffer1));
      XDrawImageString (display, pixmap, gc, sx2,sy2,buffer2,strlen(buffer2));
   }
}

static void PlotContourField (shell, dw, node, numnodes, element, numelts, nd,
                              equalize, plot_elt)
   Widget	shell;
   Widget	dw;
   Node		*node;
   unsigned	numnodes;
   Element	*element;
   unsigned	numelts,
		nd;
   Boolean	equalize;
   Boolean	plot_elt;
{
   unsigned	i,j;
   unsigned	n;
   float	x_min, y_min,
		x_max, y_max;
   float	maxX,maxY,	
		minX,minY;
   float	Xscale, Yscale;
   float	*xd,*yd,*zd;
   float	*xi,*yi,**zi;
   Dimension	width, height;
   unsigned	result;
   Region	current, global;
   XPoint	rgpnt [MaxNodesPerElement];
   Display	*display;
   GC		gc;
   char		**mask;
   unsigned	wedge_orient;

   InitializeColormap ();

   if (ximage != NULL) {
      XDestroyImage (ximage);
      DW_RemoveAll (dw);
      XFreePixmap (XtDisplay (toplevel), pixmap);
   }

	/*
	 * some preliminaries to set-up a properly 
 	 * proportioned window
	 */

   x_min = x_max = element[1] -> node[1] -> x;
   y_min = y_max = element[1] -> node[1] -> y;
   for (i = 1 ; i <= numelts ; i++) {
      for (j = 1 ; j <= element[i] -> definition -> shapenodes ; j++) {
 
         if (element[i] -> node[j] -> x > x_max)
            x_max = element[i] -> node[j] -> x;
         else if (element[i] -> node[j] -> x < x_min)
            x_min = element[i] -> node[j] -> x;

         if (element[i] -> node[j] -> y > y_max)
            y_max = element[i] -> node[j] -> y;
         else if (element[i] -> node[j] -> y < y_min)
            y_min = element[i] -> node[j] -> y;
      }
   }

   minX = x_min - 0.05*(x_max - x_min);
   maxX = x_max + 0.05*(x_max - x_min);
   minY = y_min - 0.05*(y_max - y_min);
   maxY = y_max + 0.05*(y_max - y_min);

   if (x_max - x_min < y_max - y_min) {
      maxX += 0.25*(x_max - x_min);
      wedge_orient = VERT;
   }
   else {
      minY -= 0.25*(y_max - y_min); 
      wedge_orient = HORIZ;
   }
   
   InitializeDrawingShell (shell, dw, minX, maxX, minY, maxY,
                           &Xscale, &Yscale, &width, &height);

   SetWaitCursor (dw);

   pixmap = DW_CreatePixmap (dw, width, height);

	/*
	 * set-up to do the bivariate interpolation so
	 * we can fill in the entire stress field
	 */

   zi = Allocate (float *, height); 
   mask = Allocate (char *, height);
   for (i = 0 ; i < height ; i++) {
      zi [i] = Allocate (float, width); 
      mask [i] = Allocate (char, width);
   }

   xi = Allocate (float, width);
   UnitOffset (xi);
   yi = Allocate (float, height);
   UnitOffset (yi);

   if (numnodes != 0 && node != NULL)
      nd = numnodes;

   xd = Allocate (float, nd);
   yd = Allocate (float, nd);
   zd = Allocate (float, nd);

   if (xd == NULL || yd == NULL || zd == NULL) 
      Fatal ("allocation error plotting stress field");   

   UnitOffset (xd);
   UnitOffset (yd);
   UnitOffset (zd);

   global = XCreateRegion ();

   n = 0;
   for (i = 1 ; i <= numelts ; i++) {
      for (j = 1; j <= element[i] -> definition -> shapenodes ; j++) {
         rgpnt [j - 1].x = (int) ((element[i] -> node[j] -> x - minX)*Xscale);
         rgpnt [j - 1].y = (int) ((element[i] -> node[j] -> y - minY)*Yscale);
      }
   
      current=XPolygonRegion (rgpnt, element[i] -> definition -> shapenodes, 
                              EvenOddRule);
      XUnionRegion (global, current, global);
      XDestroyRegion (current);

      if (numnodes == 0) {
         for (j = 1; j <= element[i] -> ninteg ; j++) {
            xd[n+1] = (element[i] -> stress[j] -> x - minX)*Xscale;
            yd[n+1] = (element[i] -> stress[j] -> y - minY)*Yscale;
            zd[n+1] = element[i] -> stress[j] -> values[solution->s_component];
            n++;
         }
      }
   }

   if (numnodes > 0 && node != NULL) {
     for (i = 1 ; i <= numnodes ; i++) {
        xd[i] = (node[i] -> x - minX)*Xscale;
        yd[i] = (node[i] -> y - minY)*Yscale;
        zd[i] = node[i] -> dx [solution -> d_component];
     }
   }

   for (i = 0 ; i < height ; i++) {
      for (j = 0 ; j < width ; j++) {
 
         if (XPointInRegion(global,j,i))
            mask[i][j] = 1;
         else
            mask[i][j] = 0; 
      }
   }

   XDestroyRegion (global);

   for (i = 1 ; i <= height; i++) 
      yi [i] = (float) i - 1.0;

   for (j = 1 ; j <= width ; j++) 
      xi [j] = (float) j - 1.0;

   result = BivariateInterp (nd, xd, yd, zd, width, height,xi,yi,zi,mask);

   if (result) 
      error ("could not interpolate to form stress field"); 
   else {	/* interpolation ok - draw everything */

      LoadImage (zi, width, height, mask, equalize, wedge_orient);
  
      DW_SetAutoRedraw (dw, False);

      DW_DrawPixmap (dw, minX, minY, pixmap);

      display = XtDisplay (toplevel);
      gc = DefaultGC (display, DefaultScreen (display));

      if (plot_elt) {
         DW_SetForeground (dw, "black");

         for (i = 1 ; i <= numelts ; i++) {
            for (j = 0 ; j < element[i] -> definition -> shapenodes ; j++) {
               rgpnt [j].x = (int) ((element[i] -> node[j+1] -> x - minX)*Xscale);
               rgpnt [j].y = height - 
                             (int) ((element[i] -> node[j+1] -> y - minY)*Yscale);
            }

            rgpnt [element[i] -> definition -> shapenodes].x = 
              (int) ((element[i] -> node[1] -> x - minX)*Xscale);
            rgpnt [element[i] -> definition -> shapenodes].y = 
              height - (int) ((element[i] -> node[1] -> y - minY)*Yscale);

            XDrawLines (display, pixmap, gc, rgpnt, 
                        element[i] -> definition -> shapenodes + 1, CoordModeOrigin);
         }
      }

      DW_SetAutoRedraw (dw, True);
   } 

	/*
	 * cleanup
	 */

   ZeroOffset (xd); Deallocate (xd);
   ZeroOffset (yd); Deallocate (yd);
   ZeroOffset (zd); Deallocate (zd);

   ZeroOffset (xi); Deallocate (xi);
   ZeroOffset (yi); Deallocate (yi);

   for (i = 0 ; i < height ; i++) {
      Deallocate (zi [i]);
      Deallocate (mask [i]);
   }

   Deallocate (mask);
   Deallocate (zi);

   SetNormalCursor (dw);
}  

void PlotStressField (element, numelts, nd)
   Element	*element;
   unsigned	numelts;
   unsigned	nd;
{
   static int		flag = 1;

   if (flag) {
      stressShell = CreateDrawingShell ("stressShell", "Stress Plot", 
                                        S_SaveCallback, &stress_dw);
      flag = 0;
   }
  
   pixmap = s_pixmap;
   ximage = s_ximage;
 
   PlotContourField (stressShell, stress_dw, NULL, 0, element, numelts, nd,
                     solution -> s_equalize, solution -> s_plot_elt);
   s_ximage = ximage;
   s_pixmap = pixmap;
}

void PlotDisplacementField (node, numnodes, element, numelts)
   Node		*node;
   unsigned	numnodes;
   Element	*element;
   unsigned	numelts;
{
   static int	flag = 1;

   if (flag) {
      displShell = CreateDrawingShell ("displShell", "Displacement Plot", 
                                       D_SaveCallback, &displ_dw);
      flag = 0;
   }

   pixmap = d_pixmap;
   ximage = d_ximage;

   PlotContourField (displShell, displ_dw, node, numnodes, element, numelts, 0,
                     solution -> d_equalize, solution -> d_plot_elt);

   d_ximage = ximage;
   d_pixmap = pixmap;
}
