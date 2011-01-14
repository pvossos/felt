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

/******************************************************************************
 *
 * File:	dxf.c
 *
 * Description:	contains code to read and write DXF files
 *
 *****************************************************************************/

# include <math.h>
# include <stdio.h>
# include <string.h>
# include "problem.h"
# include "objects.h"
# include "meshgen.hpp"
# include "patchwork.h"
# include "error.h"
# include "dxf.h"
# include "definition.h"

int ReadDXFFile (char *name)
{
   int		status;
   unsigned	i;
   Node		nodes [20];
   int		flag;
   DXFLine	line;
   DXFPolyline	poly;
   FILE		*input;
   Constraint	constraint;
   Material	material;
   Definition	truss, cst, quad;

   if (strcmp (name, "-") == 0)
      input = stdin;
   else {
      input = fopen (name, "r"); 
      if (input == NULL) {
         error ("could not open %s for reading.", name);
         return 1;
      }
   }

   InitializeProblem ( );

   constraint = CreateConstraint ("default");
   material = CreateMaterial ("default");

   truss = LookupDefinition ("truss");
   cst   = LookupDefinition ("CSTPlaneStress");
   quad  = LookupDefinition ("quad_PlaneStress");

   if (ReadDXFHeader (input)) {
      error ("dxf: error reading DXF header.");
      return 1;
   }

   flag = 1;
   while (flag) {
      status = ReadDXFEntity (input);

      if (status == DXF_ERROR)
         return 1;

      if (status == DXF_ENDSEC) {
         flag = 0;
         if (ReadDXFTrailer (input))
            return 1;
      }
      else if (status == DXF_UNKNOWN) {
         error ("dxf: unknown entity, cannot convert to standard element.");
         return 1;
      }
      else if (status == DXF_LINE) {
         if (ReadDXFLine (&line, NULL, input))
            return 1;

         nodes [0] = AddNode (line.xa, line.ya, line.za, constraint, NULL);
         nodes [1] = AddNode (line.xb, line.yb, line.zb, constraint, NULL);

         AddElement (truss, nodes, material, NULL, 0);
      }
      else if (status == DXF_POLYLINE) {
         if (ReadDXFPolyline (&poly, NULL, input))
            return 1;

         if (poly.n > 4 || poly.n < 2) {
            error ("dxf: can't handle polylines with %d vertices.", poly.n);
               return 1;      
         }

         for (i = 0 ; i < poly.n ; i++) 
            nodes [i] = AddNode (poly.x [i], poly.y [i], poly.z [i], 
                                 constraint, NULL);

         if (poly.n == 2)
            AddElement (truss, nodes, material, NULL, 0);
         else if (poly.n == 3)
            AddElement (cst, nodes, material, NULL, 0);
         else if (poly.n == 4)
            AddElement (quad, nodes, material, NULL, 0); 
      }
   }       

   cvector1<Node> pn(problem.nodes, problem.num_nodes);
   cvector1<Element> pe(problem.elements, problem.num_elements);
   pn = CoalesceNodes(pn, pe);
   problem.nodes = pn.release1();
   pe.release1();

   if (input != stdin)
      fclose (input);

   return 0;
}

int WriteDXFFile (char *name)
{
   unsigned	i, j;
   FILE		*fp;
   Element	e;
   DXFLine	line;
   DXFPolyline	poly;

   if (strcmp (name, "-") == 0)
      fp = stdout;
   else {
      fp = fopen (name, "w");
      if (fp == NULL) {
         error ("dxf: could not open %s for writing.", name);
         return 1;
      }
   }

   WriteDXFHeader (fp);

   for (j = 1 ; j <= problem.num_elements ; j++) {
      e = problem.elements [j];

      switch (e -> definition -> shape) {

      case Linear:
         line.xa = e -> node [1] -> x;
         line.ya = e -> node [1] -> y;
         line.za = e -> node [1] -> z;
       
         line.xb = e -> node [2] -> x;
         line.yb = e -> node [2] -> y;
         line.zb = e -> node [2] -> z;

         WriteDXFLine (&line, "0", fp);
         break;
      
      case Planar:
         for (i = 1 ; i <= e -> definition -> numnodes ; i++) {
            poly.x [i-1] = e -> node [i] -> x;    
            poly.y [i-1] = e -> node [i] -> y;    
            poly.z [i-1] = e -> node [i] -> z;    
         }

         poly.n = e -> definition -> numnodes;

         WriteDXFPolyline (&poly, "0", fp);
         break;

      default:
         error ("dxf: can only convert linear and planar elements.");
         return 1;
      }
   }

   WriteDXFTrailer (fp);

   if (fp != stdout)
      fclose (fp);

   return 0;
}
