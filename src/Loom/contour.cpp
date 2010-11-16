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
 * File:	contour.c
 *
 ***************************************************************************/

# include <stdio.h>
# include <string.h>
# include <ctype.h>
# include <X11/Xutil.h>
# include "contour.h"
# include "fe.h"
# include "allocate.h"
# include "error.h"
# include "bivar.h"
# include "colormap.h"
# include "bmp.h"
# include "cvector1.hpp"

# define HORIZ	1
# define VERT	2

# define WIDTH	400
# define HEIGHT 400

# define NUMCELLS		240
# define MaxNodesPerElement 	24

static unsigned char	  red [256];
static unsigned char	  green [256];
static unsigned char	  blue [256];
static unsigned char	  white, black;

# define Sign(x) ((x) >= 0 ? 1 : -1)

static void BresenhamLine (int xs, int ys, int xe, int ye, unsigned char **image, int nrows, int ncols)
{
   unsigned	i;
   int		x,y,
		dx,dy,
		e;
   int		signx, signy,
		temp;
   unsigned	change;

   if (xe == xs && ye == ys)
      return;

   signx = Sign (xe - xs);
   signy = Sign (ye - ys);

   dx = signx*(xe - xs);
   dy = signy*(ye - ys);

   if (dy <= dx)
      change = 0;
   else {
      change = 1;
      temp = dx;
      dx = dy;
      dy = temp;
   }

   e = 2*dy - dx;
   x = xs;
   y = ys;
 
   for (i = 0 ; i <= dx ; i++) {
      if (y > 0 && y < nrows && x > 0 && x < ncols)
         image [y][x] = black;

      while (e >= 0) {
         if (!change)
            y += signy;
         else
            x += signx;
         
         e -= 2*dx;
      }

      if (!change)
         x += signx;
      else
         y += signy;

      e += 2*dy;
   }

   return;
}

static void InitializeColormapTable (void)
{
   int	i;

   for (i = 0 ; i < NUMCELLS ; i++) {
      red [i] = color_values [i].r;
      green [i] = color_values [i].g;
      blue [i] = color_values [i].b;
   }

   white = NUMCELLS;
   black = white + 1;

   red [white] = 255;
   blue [white] = 255;
   green [white] = 255;
   red [black] = 0;
   blue [black] = 0;
   green [black] = 0;
     
   for (i = black + 1 ; i < 256 ; i++) {
      red [i] = 0;
      blue [i] = 0;
      green [i] = 0;
   } 
      
   return;
}

static void HequalImageData (unsigned char **image, int nrows, int ncols)
{
   unsigned		i,j;
   unsigned char			point;
   int	   	        histogram [NUMCELLS];
   float		p [NUMCELLS],
			v [NUMCELLS],
			vstar [NUMCELLS],
			vmin, area, sum;
   
   for (i = 0 ; i < NUMCELLS ; i++)
      histogram [i] = 0;

   area = 0;
   for (i = 0 ; i < nrows ; i++) {
      for (j = 0 ; j < ncols ; j++) {

         point = image [i][j];

         if (point != white) {
            area++;
            histogram [point]++;
         }
      }
   }

   for (i = 0 ; i < NUMCELLS ; i++)
      p [i] = histogram [i] / area;

   sum = 0.0;
   vmin = p[0];
   for (i = 0 ; i < NUMCELLS ; i++) {
      sum += p[i];
      v[i] = sum;
      if (v[i] < vmin)
         vmin = v[i];
   }

   for (i = 0 ; i < NUMCELLS ; i++)
      vstar[i] = ((((v[i] - vmin)/(1.0 - vmin))*(NUMCELLS - 1.0)) + 0.5);

   for (i = 0 ; i < nrows ; i++) {
      for (j = 0 ; j < ncols ; j++) {

         point = image [i][j];
         if (point != white) {
            point = (unsigned char) vstar [point]; 
            image [i][j] = point;
         }
      }
   }
}

static unsigned char **LoadImage (float **z, int width, int height, unsigned char **mask, int equalize, unsigned int wedge_orient)
{
   unsigned		x_max,y_max,x_min,y_min;
   float		max,min;
   unsigned		i,j;
   float		slope;
   int			point;
   unsigned 		flag;
   int			max_width;
   int			min_width;
   int			sx1,sy1,sx2,sy2;
   unsigned char			**image;
   
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

   image = Allocate (unsigned char *, height);
   for (i = 0 ; i < height ; i++)
      image [i] = Allocate (unsigned char, width);

   slope = (float) NUMCELLS / (max - min);

   for (i = 0 ; i < height ; i++) {
      for (j = 0 ; j < width ; j++) {
     
         if (mask[i][j]) {
            if (z[i][j] != max)
               point = (int) (slope*(z[i][j] - min));
            else
               point = NUMCELLS - 1; 
         }
         else
            point = white;

         image [height - i - 1][j] = point;
      }
   }

   if (wedge_orient == HORIZ) {
      for (j = x_min ; j <= x_max ; j++) {

         point = (int) ((float) (j-x_min)/
                        (float) (x_max - x_min)*(NUMCELLS - 1.0)); 

         for (i = (int) (0.88*height) ; i < height - 12 ; i++)
            image [i][j] = point;
      }
      sx1 = x_min;
      sx2 = x_max - max_width;
      sy1 = sy2 = height - 2;
   }
   else {
      for (i = y_min ; i <= y_max ; i++) {

         point = (int) ((float) (i-y_min)/
                        (float) (y_max - y_min)*(NUMCELLS - 1.0)); 

         for (j = (int) (0.9*width) ; j < (int) (0.98*width) ; j++) 
            image [height - i - 1][j] = point;
      }
      sy1 = y_max + 10;
      sy2 = y_min - 2;
      sx1 = width - min_width - 1;
      sx2 = width - max_width - 1; 
   }

   if (equalize)
      HequalImageData (image, height, width);

   return image;
}

static void PlotContourField (char *filename, Node *node, unsigned int numnodes, Element *element, unsigned int numelts, unsigned int nd, int component, int equalize, int plot_elt, int width, int height)
{
   unsigned	i,j;
   unsigned	n;
   float	x_min, y_min,
		x_max, y_max;
   float	maxX,maxY,	
		minX,minY;
   float	Xscale, Yscale;
   float	**zi;
   unsigned	result;
   unsigned char		**mask;
   unsigned	wedge_orient;
   Region       current, global;
   XPoint       rgpnt [MaxNodesPerElement];
   unsigned char		**image;

   InitializeColormapTable ();

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
  
    
   if (width && height) {
      Xscale = (float) width / (maxX - minX);
      Yscale = (float) height / (maxY - minY);
   }
   else { 
      if (maxX - minX > maxY - minY) {
         Xscale = (float) WIDTH / (maxX - minX);
         width  = WIDTH;
         Yscale = Xscale;
         height = Yscale*(maxY - minY);
      }
      else {
         Yscale = (float) HEIGHT / (maxY - minY);
         height = HEIGHT;
         Xscale = Yscale;
         width = Xscale*(maxX - minX);
      }
   }
      
	/*
	 * set-up to do the bivariate interpolation so
	 * we can fill in the entire stress field
	 */

   zi = Allocate (float *, height); 
   mask = Allocate (unsigned char *, height);
   for (i = 0 ; i < height ; i++) {
      zi [i] = Allocate (float, width); 
      mask [i] = Allocate (unsigned char, width);
   }

   cvector1f xi(width);
   cvector1f yi(height);

   if (numnodes != 0 && node != NULL)
      nd = numnodes;

   cvector1f xd(nd);
   cvector1f yd(nd);
   cvector1f zd(nd);

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
            zd[n+1] = element[i] -> stress[j] -> values[component];
            n++;
         }
      }
   }

   if (numnodes > 0 && node != NULL) {
     for (i = 1 ; i <= numnodes ; i++) {
        xd[i] = (node[i] -> x - minX)*Xscale;
        yd[i] = (node[i] -> y - minY)*Yscale;
        zd[i] = node[i] -> dx [component];
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

   result = BivariateInterp (nd, xd.c_ptr1(), yd.c_ptr1(), zd.c_ptr1(), width, height,xi.c_ptr1(),yi.c_ptr1(),zi,mask);

   if (result) {
      error ("could not interpolate to form stress field"); 
      image = NULL;
   }
   else {	/* interpolation ok - draw everything */

      image = LoadImage (zi, width, height, mask, equalize, wedge_orient);
  
      if (plot_elt) {
         for (i = 1 ; i <= numelts ; i++) {
            for (j = 1 ; j < element[i] -> definition -> shapenodes ; j++) 
               BresenhamLine((int) ((element[i] -> node[j] -> x - minX)*Xscale),
                             height - (int) ((element[i] -> node[j] -> y - minY)*Yscale),
                             (int) ((element[i] -> node[j+1] -> x - minX)*Xscale),
                             height - (int) ((element[i] -> node[j+1] -> y - minY)*Yscale),
                             image, height, width);
            BresenhamLine((int) ((element[i] -> node[j] -> x - minX)*Xscale),
                          height - (int) ((element[i] -> node[j] -> y - minY)*Yscale),
                          (int) ((element[i] -> node[1] -> x - minX)*Xscale),
                          height - (int) ((element[i] -> node[1] -> y - minY)*Yscale),
                          image, height, width);
         }
      }
   } 

	/*
	 * cleanup
	 */

   for (i = 0 ; i < height ; i++) {
      Deallocate (zi [i]);
      Deallocate (mask [i]);
   }

   Deallocate (mask);
   Deallocate (zi);

   if (image)
      ImageDataToBMP(filename, image, height, width, red, green, blue);

   return;
}  

void PlotStressField (char *out, Element *element, unsigned numelts, int comp,
                      int equalize, int plot_elt, int width, int height)
{
   int		nd;
   int		i, j;
   int		flag;

   nd = 0;
   flag = 1;
   for (i = 1; i <= numelts ; i++) {
      if (element [i] -> definition -> shape != Planar) {
          error ("cannot plot stresses for non-planar elements");
          return;
      }
      if (element [i] -> stress == NULL) {
         error ("could not get stresses for all elements");
        return;
      }
      if (comp > element [i] -> definition -> numstresses) {
         error ("invalid stress component for element %d",i);
         return;
      }

      if (flag) {
         for (j = 1 ; j <= element [i] -> ninteg ; j++) {
            if (element [i] -> stress [j] -> values [comp] != 0)
               flag = 0;
         }
      }

      nd += element [i] -> ninteg;
   }

   if (flag) {
      error ("all stresses are zero for specified component");
      return;
   }

   PlotContourField (out, NULL, 0, element, numelts, nd, comp, 
                     equalize, plot_elt, width, height);
}

void PlotDisplacementField (char *out, Node *node, unsigned numnodes,
                            Element *element, unsigned numelts, int comp, 
                            int equalize, int plot_elt, int width, int height)
{
   int		i;
   int		flag;

   for (i = 1; i <= numelts ; i++) {
      if (element [i] -> definition -> shape != Planar) {
          error ("cannot plot stresses for non-planar elements");
          return;
      }
   }

   flag = 1;
   for (i = 1 ; i <= numnodes ; i++) {
      if (node [i] -> dx [comp] != 0) {
         flag = 0;
         break;
      }
   }

   if (flag) {
      error ("all displacements are zero for specified component");
      return;
   }

   PlotContourField (out, node, numnodes, element, numelts, 0, comp, 
                     equalize, plot_elt, width, height);
}
