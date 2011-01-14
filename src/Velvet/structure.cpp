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
 * File:	structure.c
 *
 * Description:	Contains routines to visualize structural problems in velvet
 *		Both 2d and 3d line drawing are supported ... displaced
 * 		shapes are drawn over the original shape.
 *
 ****************************************************************************/

# include <stdio.h>
# include <math.h>
# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <X11/Shell.h>
# include "Solution.h"
# include "fe.h"
# include "Drawing.h"
# include "draw3d.h"
# include "error.h"
# include "procedures.h"
# include "cvector1.hpp"

extern Widget toplevel;

Widget CreateDrawingShell (String, String, XtCallbackProc, Widget *);

# define MaxNodesPerElement 	12

static Widget	structShell;
static Widget	struct_dw;
static int     	first_time = 1;

static void SaveCallback (Widget w, XtPointer client_data, XtPointer call_data)
{
   DumpDrawingArea (struct_dw, "Save Structure Plot", True);
}                       

void VisualizeStructure (Element *element, unsigned int numelts)
{
   Point	points [MaxNodesPerElement];
   unsigned	n;
   unsigned	i,j;
   float	x_max, x_min,
		y_max, y_min;
   float	maxX, minX, 
                maxY, minY, 
                Xscale, Yscale;
   float	x,y;
   float	x1,x2,y1,y2;
   Dimension	width, height;

   if (first_time) {
      structShell = CreateDrawingShell ("structShell", "Structure Plot", 
                                        SaveCallback, &struct_dw);

      first_time = 0;
   }

   x_max = x_min = element [1] -> node[1] -> x +
             (element[1] -> node[1] -> dx[1]*solution -> magnify); 
   y_max = y_min = element [1] -> node[1] -> y + 
             (element[1] -> node[1] -> dx[2]*solution -> magnify); 

   for (i = 1 ; i <= numelts ; i++) {
      for (j = 1 ; j <= element[i] -> definition -> shapenodes ; j++) {

         x = element[i] -> node[j] -> x + 
            (element[i] -> node[j] -> dx[1] *solution -> magnify);
         y = element[i] -> node[j] -> y + 
            (element[i] -> node[j] -> dx[2] *solution -> magnify);

         if (x > x_max)
            x_max = x;
         else if (x < x_min)
            x_min = x;

         if (y > y_max) 
            y_max = y;
         else if (y < y_min) 
            y_min = y;

         x = element[i] -> node[j] -> x;
         y = element[i] -> node[j] -> y;

         if (x > x_max)
            x_max = x;
         else if (x < x_min)
            x_min = x;

         if (y > y_max) 
            y_max = y;
         else if (y < y_min) 
            y_min = y;
      }
   }

   if (x_max != x_min) {
      minX = x_min - 0.05*(x_max - x_min); 
      maxX = x_max + 0.05*(x_max - x_min);
   }
   else {
      minX = x_min - 0.05*(y_max - y_min);
      maxX = x_max + 0.05*(y_max - y_min);
   }
 
   if (y_max != y_min) {
      minY = y_min - 0.05*(y_max - y_min);
      maxY = y_max + 0.05*(y_max - y_min);
   }
   else {
      minY = y_min - 0.05*(x_max - x_min);
      maxY = y_max + 0.05*(x_max - x_min);
   }
   
   DW_RemoveAll (struct_dw);

   InitializeDrawingShell (structShell, struct_dw, minX, maxX, minY, maxY,
                           &Xscale, &Yscale, &width, &height);

   DW_SetAutoRedraw (struct_dw, False);

	/*
 	 * plot the underlying original shape if requested
	 */

   if (solution -> plot_orig) {
      DW_SetLineStyle (struct_dw, DW_LineDashed);
      DW_SetForeground (struct_dw, "black");

      for (i = 1 ; i <= numelts ; i++) {
         n = element[i] -> definition -> shapenodes;
         if (n > 2) {
            for (j = 1 ; j <= n ; j++) {
               points [j-1].x = element [i] -> node[j] -> x;
               points [j-1].y = element [i] -> node[j] -> y;
            }
            points [n].x = element[i] -> node[1] -> x;
            points [n].y = element[i] -> node[1] -> y;

            DW_DrawPolygon (struct_dw, True, points, n+1);
         } else 
            DW_DrawLine (struct_dw, element[i] -> node[1] -> x,
                         element[i] -> node[1] -> y, element[i] -> node[2] -> x,
                         element[i] -> node[2] -> y); 
      }
   }

   DW_SetLineStyle (struct_dw, DW_LineSolid);
   for (i = 1 ; i <= numelts ; i++) {
      n = element[i] -> definition -> shapenodes;
      if (n > 2) {
         for (j = 1 ; j <= n ; j++) {
            points [j-1].x = element [i] -> node[j] -> x +
                             (element[i] -> node[j] -> dx[1] * solution -> magnify);
            points [j-1].y = element [i] -> node[j] -> y +
                             (element[i] -> node[j] -> dx[2] * solution -> magnify);
         }
         points [n].x = element[i] -> node[1] -> x +
                        (element[i] -> node[1] -> dx[1] * solution -> magnify);
         points [n].y = element[i] -> node[1] -> y +
                        (element[i] -> node[1] -> dx[2] * solution -> magnify);

         DW_DrawPolygon (struct_dw, True, points, n+1);
      } else {
         x1 = element[i] -> node[1] -> x +
              (element[i] -> node[1] -> dx[1] * solution -> magnify); 
         x2 = element[i] -> node[2] -> x +
              (element[i] -> node[2] -> dx[1] * solution -> magnify); 
         y1 = element[i] -> node[1] -> y +
              (element[i] -> node[1] -> dx[2] * solution -> magnify); 
         y2 = element[i] -> node[2] -> y +
              (element[i] -> node[2] -> dx[2] * solution -> magnify); 

         DW_DrawLine (struct_dw, x1, y1, x2, y2);
      }
   }
   DW_SetAutoRedraw (struct_dw, True);
}

struct s_pair {
    float	xd[MaxNodesPerElement], yd[MaxNodesPerElement];
    float    	x[MaxNodesPerElement], y[MaxNodesPerElement]; 
};

void VisualizeStructure3D (Element *element, unsigned int numelts)
{
   Point	points [MaxNodesPerElement];
   Point	points2 [MaxNodesPerElement];
   unsigned	i,j;
   float	maxX, minX, 
                maxY, minY, 
                maxZ, minZ, 
		Xscale, Yscale;
   float	xdiff, ydiff;
   float	x,y,z,
		sx,sy;
   Dimension	width, height;

   if (first_time) {
      structShell = CreateDrawingShell ("structShell", "Structure Plot", 
                                        SaveCallback, &struct_dw);
      first_time = 0;
   }

   cvector1<s_pair> s_element(numelts);

   maxX = minX = element[1] -> node[1] -> x +
                 (element[1] -> node[1] -> dx[1] * solution -> magnify);
   maxY = minY = element[1] -> node[1] -> y +
                 (element[1] -> node[1] -> dx[2] * solution -> magnify);
   maxZ = minZ = element[1] -> node[1] -> z +
                 (element[1] -> node[1] -> dx[3] * solution -> magnify);

   for (i = 1 ; i <= numelts ; i++) {
      for (j = 1 ; j <= element[i] -> definition -> shapenodes; j++) {

         x = element [i] -> node[j] -> x +
             (element [i] -> node[j] -> dx[1] * solution -> magnify);
         y = element [i] -> node[j] -> y +
             (element [i] -> node[j] -> dx[2] * solution -> magnify);
         z = element [i] -> node[j] -> z +
             (element [i] -> node[j] -> dx[3] * solution -> magnify);

         if (x > maxX) maxX = x;
         else if (x < minX) minX = x;

         if (y > maxY) maxY = y;
         else if (y < minY) minY = y;

         if (z > maxZ) maxZ = z;
         else if (z < minZ) minZ = z;

         x = element [i] -> node[j] -> x;
         y = element [i] -> node[j] -> y;
         z = element [i] -> node[j] -> z;

         if (x > maxX) maxX = x;
         else if (x < minX) minX = x;

         if (y > maxY) maxY = y;
         else if (y < minY) minY = y;

         if (z > maxZ) maxZ = z;
         else if (z < minZ) minZ = z;
      }
   }

   Setup3D (minX,maxX,minY,maxY,minZ,maxZ);

   xdiff = maxX - minX;
   ydiff = maxY - minY;

   for (i = 1 ; i <= numelts ; i++) {
      for (j = 1 ; j <= element [i] -> definition -> shapenodes ; j++) {

         x = element [i] -> node[j] -> x +
             (element [i] -> node[j] -> dx[1] * solution -> magnify);
         y = element [i] -> node[j] -> y +
             (element [i] -> node[j] -> dx[2] * solution -> magnify);
         z = element [i] -> node[j] -> z +
             (element [i] -> node[j] -> dx[3] * solution -> magnify);
         
         Convert3Dto2D (x, y, z, xdiff, ydiff, &sx, &sy);

         if (i == 1 && j == 1) {
            maxX = minX = sx;
            maxY = minY = sy;
         }
         else {
            if (sx > maxX) maxX = sx;
            else if (sx < minX) minX = sx;

            if (sy > maxY) maxY = sy;
            else if (sy < minY) minY = sy;
         } 
           
         s_element[i].xd[j] = sx;
         s_element[i].yd[j] = sy;

         x = element [i] -> node[j] -> x;
         y = element [i] -> node[j] -> y;
         z = element [i] -> node[j] -> z;
         
         Convert3Dto2D (x, y, z, xdiff, ydiff, &sx, &sy);

         if (i == 1 && j == 1) {
            maxX = minX = sx;
            maxY = minY = sy;
         }
         else {
            if (sx > maxX) maxX = sx;
            else if (sx < minX) minX = sx;

            if (sy > maxY) maxY = sy;
            else if (sy < minY) minY = sy;
         } 
           
         s_element[i].x[j] = sx;
         s_element[i].y[j] = sy;
      }
   } 

   if (maxX != minX) {
      maxX += 0.05*(maxX - minX);
      minX -= 0.05*(maxX - minX);
   }
   else {
      maxX += 0.05*(maxY - minY);
      minX -= 0.05*(maxY - minY);
   }

   if (maxY != minY) {
      maxY += 0.05*(maxY - minY);
      minY -= 0.05*(maxY - minY);
   }
   else {
      maxY += 0.05*(maxX - minX);
      minY -= 0.05*(maxX - minX);
   }


   InitializeDrawingShell (structShell, struct_dw, minX, maxX, minY, maxY,
                           &Xscale, &Yscale, &width, &height);

   DW_RemoveAll (struct_dw);
    
   if (solution -> plot_orig) {
      DW_SetLineStyle (struct_dw, DW_LineDashed);
      DW_SetForeground (struct_dw, "black");

      for (i = 1 ; i <= numelts ; i++) {
         switch (element [i] -> definition -> shape) {

         case Solid:
            if (element [i] -> definition -> shapenodes == 8) {
               for (j = 1 ; j <= 4 ; j++) {
                  points [j-1].x = s_element[i].x[j];
                  points [j-1].y = s_element[i].y[j];
                  points2 [j-1].x = s_element[i].x[j+4];
                  points2 [j-1].y = s_element[i].y[j+4];
                  DW_DrawLine (struct_dw, s_element[i].x[j], s_element[i].y[j],
                               s_element[i].x[j+4], s_element[i].y[j+4]); 
               }
               points [4].x = s_element[i].x[1];
               points [4].y = s_element[i].y[1];
               points2 [4].x = s_element[i].x[5];
               points2 [4].y = s_element[i].y[5];
   
               DW_DrawPolygon (struct_dw, True, points, 5);
               DW_DrawPolygon (struct_dw, True, points2, 5);
            }
            break;
     
         case Planar:
            for (j = 1 ; j <= element[i] -> definition -> shapenodes ; j++) {
               points [j-1].x = s_element[i].x[j];
               points [j-1].y = s_element[i].y[j];
            }
            points [j].x = s_element[i].x[1];
            points [j].y = s_element[i].y[1];
   
            DW_DrawPolygon (struct_dw, True, points, j+1);
            break;

         case Linear:
            DW_DrawLine (struct_dw, s_element[i].x[1], s_element[i].y[1],
               s_element[i].x[2], s_element[i].y[2]); 
            break;
         }
      }
   }

   DW_SetLineStyle (struct_dw, DW_LineSolid);
   for (i = 1 ; i <= numelts ; i++) {
      switch (element [i] -> definition -> shape) {

      case Solid:
         if (element [i] -> definition -> shapenodes == 8) {
            for (j = 1 ; j <= 4 ; j++) {
               points [j-1].x = s_element[i].xd[j];
               points [j-1].y = s_element[i].yd[j];
               points2 [j-1].x = s_element[i].xd[j+4];
               points2 [j-1].y = s_element[i].yd[j+4];
               DW_DrawLine (struct_dw, s_element[i].xd[j], s_element[i].yd[j],
                            s_element[i].xd[j+4], s_element[i].yd[j+4]); 
            }
            points [4].x = s_element[i].xd[1];
            points [4].y = s_element[i].yd[1];
            points2 [4].x = s_element[i].xd[5];
            points2 [4].y = s_element[i].yd[5];

            DW_DrawPolygon (struct_dw, True, points, 5);
            DW_DrawPolygon (struct_dw, True, points2, 5);
         }
         break;
  
      case Planar:
         for (j = 1 ; j <= element[i] -> definition -> shapenodes ; j++) {
            points [j-1].x = s_element[i].xd[j];
            points [j-1].y = s_element[i].yd[j];
         }
         points [j].x = s_element[i].xd[1];
         points [j].y = s_element[i].yd[1];

         DW_DrawPolygon (struct_dw, True, points, j+1);
         break;

      case Linear:
         DW_DrawLine (struct_dw, s_element[i].xd[1], s_element[i].yd[1],
            s_element[i].xd[2], s_element[i].yd[2]); 
         break;
      }
   }
}
