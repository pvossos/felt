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
 * File:	postscript.c						*
 *									*
 * Description:								*
 *									*
 ************************************************************************/

# include <stdio.h>
# include <string.h>
# include <X11/Xatom.h>
# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include "error.h"
# include "Drawing.h"
# include "Canvas.h"
# include "Tree.h"
# include "pslib.h"

static double		Xscale, Yscale;
static double		Xmin, Ymin;
static double		conv;
static Widget		w;	

extern Canvas		canvas;

static char *color_names [ ] = {
"white", "black", "red", "green", "blue", "yellow", "brown", 
"gray", "violet", "cyan", "magenta", "orange"};

static char *Color_names [ ] = {
"White", "Black", "Red", "Green", "Blue", "Yellow", "Brown", 
"Gray", "Violet", "Cyan", "Magenta", "Orange"};

static int xconv (x)
   double	x;
{
   return xconvps ((x - Xmin)/Xscale);
}

static int yconv (y)
   double	y;
{
   return yconvps ((y - Ymin)/Yscale);
}

static void SetupFont (fname, fstruct)
   String	fname;
   XFontStruct	*fstruct;
{
   static char    *size_names [ ] = {"--8","--10","--11","--12","--14","--17",
                                     "--18","--20","--24","--25","--34"};
   static int     sizes [ ] = {8,10,11,12,14,17,18,20,24,25,34};
   int		  i;
   int		  number;
   double	  size;
   unsigned long  weight;
   unsigned long  pt_size;
   unsigned long  italic;
   static char	  *prev_name = NULL;

   if (fname == prev_name)		/* save some expensive checks */
      return;
 
 
 /* make sure the font we use for labels is small! */

   if (strcmp (fname, canvas -> label_font) == 0) {
      size = 6;
      number = 4;
   }

 /* check to see if this is actually an adobe that we can match exactly */

   else if (strstr (fname, "adobe") != NULL) {
      if (strstr (fname, "helvetica") != NULL) {	/* helvetica    */
         if (strstr (fname, "-o-") != NULL) {		   /* oblique   */
            if (strstr (fname, "bold") != NULL) 	      /* bold   */
               number = 7;
            else					      /* medium */
               number = 6;
         }
         else {						   /* roman     */
            if (strstr (fname, "bold") != NULL) 	      /* bold   */
               number = 5;
            else					      /* medium */
               number = 4;
         } 
      }
      else if (strstr (fname, "times") != NULL) {	/* times        */
         if (strstr (fname, "-i-") != NULL) {		   /* italic    */
            if (strstr (fname, "bold") != NULL) 	      /* bold   */
               number = 3;
            else					      /* medium */
               number = 2;
         }
         else {						   /* roman     */
            if (strstr (fname, "bold") != NULL) 	      /* bold   */
               number = 1;
            else					      /* medium */
               number = 0;
         } 
      }
      else if (strstr (fname, "courier") != NULL) {
         if (strstr (fname, "-o-") != NULL) {		   /* oblique   */
            if (strstr (fname, "bold") != NULL) 	      /* bold   */
               number = 11;
            else					      /* medium */
               number = 10;
         }
         else {						   /* roman     */
            if (strstr (fname, "bold") != NULL) 	      /* bold   */
               number = 9;
            else					      /* medium */
               number = 8;
         } 
      }
      else if (strstr (fname, "symbol") != NULL) 	/* symbol       */
         number = 12;
      else 						/* default	*/
         number = 4;

      size = 11.0;
      for (i = 0 ; i < XtNumber (sizes) ; i++) {
         if (strstr (fname, size_names [i]) != NULL) {
            size = (double) sizes [i];
            break;
         }
      } 
   }

 /* otherwise make a basically random guess using a helvetica font */  

   else {				
      if (!XGetFontProperty (fstruct, XA_POINT_SIZE, &pt_size)) 
         pt_size = 78;
   
      if (!XGetFontProperty (fstruct, XA_WEIGHT, &weight))
         weight = 0;
 
      if (!XGetFontProperty (fstruct, XA_ITALIC_ANGLE, &italic))
         italic = 5760;

      if (abs(italic) != 5760 && weight > 300)
         number = 7;
      else if (abs(italic) != 5760)
         number = 6;
      else if (weight > 500) 
         number = 5;
      else
         number = 4;

      size = pt_size / 7.1;
   }

   prev_name = fname;

   pssetfont (number);
   pssetfontsize (size);
}

static void SetupLineStyle (style)
   int			style;
{
   int			ps_style;

   switch (style) {

   case DW_LineSolid:
      ps_style = 1;
      break;
 
   case DW_LineDashed:
      ps_style = 4;
      break;
  
   case DW_LineDotted:
      ps_style = 2;
      break;

   case DW_LineDotDashed:
      ps_style = 5;
      break;

   case DW_LineLongDashed:
      ps_style = 3;
      break;

   default:
      ps_style = 1;
   }

   pssetlinestyle (ps_style);
}   

static void SetupColor (color)
   String	color;
{
   int		i;
   int		color_number;
   static char	*prev_color = NULL;

   if (color == prev_color)		/* save all the strcmp's */
      return; 

   color_number = 1;
   for (i = 0 ; i < XtNumber (color_names) ; i++) {
      if (!strcmp(color, color_names [i]) || !strcmp(color, Color_names [i])) {
         color_number = i;
         break;
      }
   }
 
   prev_color = color;

   pssetcolor (color_number);
}

static int TranslateFigure (f)
   Figure		f;
{
   FigureAttributes	attr;
   unsigned		j;

   DW_GetAttributes (w, f, &attr);

   SetupColor (attr.color);

   switch (attr.type) {
   
   case LineFigure:
      SetupLineStyle (attr.line_style);
      drawps (xconv(attr.points [0].x), yconv(attr.points [0].y), 0);  
      drawps (xconv(attr.points [1].x), yconv(attr.points [1].y), 1);  
      break;

   case RectangleFigure:
      SetupLineStyle (attr.line_style);
      drawps (xconv(attr.x), yconv(attr.y), 0);
      drawps (xconv(attr.x + attr.width), yconv(attr.y), 1);
      drawps (xconv(attr.x + attr.width), yconv(attr.y + attr.height), 1);
      drawps (xconv(attr.x), yconv(attr.y + attr.height), 1);
      drawps (xconv(attr.x), yconv(attr.y), 1);
      break;
   
   case TextFigure:
      SetupFont (attr.font, attr.font_struct);
      dispstrps (xconv(attr.x), yconv(attr.y), 0, attr.text, 0, 1);
      break;

   case ArcFigure:
      SetupLineStyle (attr.line_style);
      if (attr.filled)
         if (attr.scaled)
            psfillellipse (xconv(attr.x), yconv(attr.y), 
                           abs(attr.width/2*conv), abs(attr.height/2*conv),
                           (int) attr.arc_start, 
			   (int) (attr.arc_length + attr.arc_start));
         else
            psfillellipse (xconv(attr.x), yconv(attr.y), 
                           abs(attr.width/2*3), abs(attr.height/2*3),
                           (int) attr.arc_start, 
			   (int) (attr.arc_length + attr.arc_start));
      else
         if (attr.scaled)
            psdrawellipse (xconv(attr.x), yconv(attr.y), 
                           abs(attr.width/2*conv), abs(attr.height/2*conv),
                           (int) attr.arc_start, 
			   (int) (attr.arc_length + attr.arc_start));
         else
            psdrawellipse (xconv(attr.x), yconv(attr.y), 
                           abs(attr.width/2*3), abs(attr.height/2*3),
                           (int) attr.arc_start, 
			   (int) (attr.arc_length + attr.arc_start));
      break;

   case PolygonFigure:
      SetupLineStyle (attr.line_style);
      drawps (xconv(attr.points [0].x), yconv(attr.points [0].y), 0); 
      for (j = 1 ; j < attr.npoints ; j++) {
         drawps (xconv(attr.points [j].x), yconv(attr.points [j].y), 1);
      }
      break;

   default:
      break;
   }

   return 0;
}
      
int WritePostscriptFigures (f, n, widget, xmin, xmax, ymin, ymax, ps_filename)
   Figure	*f;
   unsigned	n;
   Widget	widget;
   double	xmin;
   double	xmax;
   double	ymin;
   double	ymax;
   char		*ps_filename;
{
   int		i;
   int		width, height;
   FILE		*fp;

   Ymin = ymin;
   Xmin = xmin;

   w = widget;

   fp = fopen (ps_filename, "w");
   if (fp == NULL) {
      error ("could not open %s for writing", ps_filename);
      return 1;
   }

   if (xmax - xmin > ymax - ymin) 
      psinitgraphics (1, fp);	/* landscape */
   else
      psinitgraphics (3, fp);	/* portrait  */

   psgetextents (&width, &height);

   if ((xmax - xmin) / width > (ymax - ymin) / height) {
      Xscale = (xmax - xmin)/width;
      Yscale = Xscale;
   }
   else {
      Yscale = (ymax - ymin)/height;
      Xscale = Yscale;
   }
   conv = 1.0/Xscale;

   pssetlinewidth (1);
   pssetlinestyle (1);
   pssetcolor (1);

	/*
	 * walk through the array backwards such that the top things get
	 * drawn last ... (in case someone is using color and actually cares)
	 */

   for (i = n-1 ; i >= 0 ; i--)
      TranslateFigure (f [i]);

   psleavegraphics (fp);

   return 0;
}
