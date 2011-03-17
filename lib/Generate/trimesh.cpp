/*
    This file is part of the FElt finite element analysis package.
    Copyright (C) 1993 Jason I. Gobat and Darren C. Atkinson

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
# include "allocate.h"
# include "error.h"
# include "fe.h"
# include "meshgen.hpp"
# include "triangle.h"

typedef double dbl_pair [2];
typedef int    triple_int [3];

static double PolygonArea (double (*vcl)[2], int n)
{
   int       i;
   double    A;

   A = 0.0;
   for (i = 0 ; i < n - 1 ; i++)
      A += (vcl [i][0]*vcl [i+1][1] - vcl [i+1][0]*vcl [i][1]);

   A += (vcl [n-1][0]*vcl [0][1] - vcl [0][0]*vcl [n-1][1]);
 
   return 0.5*A;
}

unsigned
GenerateTriMesh(TriMesh trimesh, cvector1<Element> &element, cvector1<Node> &node, 
                unsigned int bnode, unsigned int belement)
{
   unsigned	i,j;
   int		n, m;
   int		ne, nn;
   struct triangulateio   in, out;
   int		npoints;
   double       x, y;
   int		start;
   double	A, Atot;
   double	Acrit;
   char		opts [32];

   if (trimesh -> numcurves <= 0) {
      error ("must have at least a boundary curve to generate a TriMesh");
      return 1;
   }

   if (trimesh -> definition -> numnodes != 3) {
      error ("TriMesh generation requires three node elements");
      return 1;
   }


   npoints = 0;

   Atot = 0.0;

   for (i = 0 ; i < trimesh -> numcurves ; i++) {
      if (trimesh -> curves [i] -> numvc < 3) {
         error ("each curve must have at least 3 points");
         return 1;
      }

      npoints += trimesh -> curves [i] -> numvc;

      A = PolygonArea(trimesh -> curves [i] -> vcl, 
                      trimesh -> curves [i] -> numvc);

      if (i == 0 && A < 0) {
         error ("main boundary points must be in CCW order");
         return 1;
      }
      else if (i > 0 && A > 0) {
         error ("hole boundary points must be in CW order");
         return 1;
      }

      Atot += A;
   }

   Acrit = trimesh -> alpha * Atot / trimesh -> target;

   if (npoints <= 0) {
      error ("nothing to generate");
      return 1;
   }

   in.numberofpoints = npoints;
   in.numberofpointattributes = 0;
   in.pointlist = (double *) malloc(in.numberofpoints * 2 * sizeof(double));
   in.pointattributelist = NULL;
   in.pointmarkerlist = (int *) malloc(sizeof(int) * in.numberofpoints);
   for (i = 0 ; i < in.numberofpoints ; i++)
       in.pointmarkerlist [i] = 1;

   in.numberofholes = trimesh -> numcurves - 1;
   if (in.numberofholes)
      in.holelist = (double *) malloc(2 * in.numberofholes * sizeof(double));
   else
      in.holelist = NULL;

   in.numberofregions = 0;
   in.numberofsegments = npoints;
   in.segmentlist = (int *) malloc(2 * in.numberofsegments * sizeof(int));
   in.segmentmarkerlist = (int *) malloc(sizeof(int) * in.numberofsegments);
   for (i = 0 ; i < in.numberofsegments ; i++)
       in.segmentmarkerlist [i] = 1;

   n = 0;
   m = 0;
   for (i = 0 ; i < trimesh -> numcurves ; i++) {
      start = m;
      for (j = 0 ; j < trimesh -> curves [i] -> numvc ; j++) {
         in.pointlist [n]     = trimesh -> curves [i] -> vcl [j][0];
         in.pointlist [n + 1] = trimesh -> curves [i] -> vcl [j][1];
        
         in.segmentlist [n]     = m; 
         in.segmentlist [n + 1] = m + 1;

         n += 2;
         m += 1;
      }

      in.segmentlist [n - 1] = start;
   }

   for (i = 0 ; i < in.numberofholes ; i++) {
      x = 0.0;
      y = 0.0;

      for (j = 0 ; j < trimesh -> curves [i+1] -> numvc ; j++) {
         x += trimesh -> curves [i+1] -> vcl [j][0];
         y += trimesh -> curves [i+1] -> vcl [j][1];
      }

      x /= trimesh -> curves [i+1] -> numvc;
      y /= trimesh -> curves [i+1] -> numvc;

      in.holelist [2*i]     = x;
      in.holelist [2*i + 1] = y;
   }

   out.pointlist                  = (double *) NULL;            
   out.pointattributelist         = (double *) NULL;
   out.pointmarkerlist            = (int *) NULL; 
   out.trianglelist               = (int *) NULL;          
   out.numberoftriangleattributes = 0;
   out.triangleattributelist      = (double *) NULL;
   out.segmentmarkerlist          = (int *) NULL;
   out.segmentlist                = (int *) NULL;

   sprintf (opts, "Qpqza%f", Acrit);

   triangulate(opts, &in, &out, NULL);

   ne = out.numberoftriangles;
   nn = out.numberofpoints;
   if (ne <= 0 || nn <= 0) {
      error ("nothing to generate");
      return 1;
   }

	/*
	 * allocate some memory to hold everything that we will generate
	 */

   node.resize(nn);
   for (i = 1 ; i <= nn ; i++)
       node[i] = new node_t;

   element.resize(ne);
   for (i = 1 ; i <= ne ; i++)
       element [i] = new element_t(0, trimesh -> definition);

	/*	
	 * generate all the nodes
	 */

   for (i = 1 ; i <= nn ; i++) {
 
      node [i] -> number = i + bnode;
      node [i] -> x = out.pointlist [2*i - 2];
      node [i] -> y = out.pointlist [2*i - 1];
      node [i] -> z = 0;

   }

	/*
	 * attach all the elements to the nodes
	 */

   for (i = 1 ; i <= ne ; i++) {

      element [i] -> number = i + belement;
      element [i] -> node [1] = node [out.trianglelist [3*(i - 1) + 0] + 1];
      element [i] -> node [2] = node [out.trianglelist [3*(i - 1) + 1] + 1];
      element [i] -> node [3] = node [out.trianglelist [3*(i - 1) + 2] + 1];

   } 

   Deallocate(in.pointlist);
   Deallocate(in.pointmarkerlist);
   Deallocate(in.holelist);
   Deallocate(in.segmentlist);
   Deallocate(in.segmentmarkerlist);
   Deallocate(out.pointlist);
   Deallocate(out.pointattributelist);
   Deallocate(out.pointmarkerlist);
   Deallocate(out.trianglelist);
   Deallocate(out.triangleattributelist);
   Deallocate(out.segmentmarkerlist);
   Deallocate(out.segmentlist);

   return 0;
}
