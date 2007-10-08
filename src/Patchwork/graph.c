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
 * File:	graph.c
 * 
 * Description:	contains code to read and write basic ASCII data files that
 *		are formatted for input into graphing packages like gnuplot
 *
 ****************************************************************************/

# include <stdio.h>
# include <string.h>
# include "problem.h"
# include "objects.h"
# include "mesh.h"
# include "patchwork.h"
# include "definition.h"
# include "error.h"

# undef atof
extern double atof (const char *);
extern double strtod (const char *, char **);

extern int InitializeProblem (void);

int ReadGraphFile (char *filename)
{
   FILE			*fp;
   unsigned		i;
   Node			node [20];
   double		x, y, z;
   Material		material;
   Constraint		constraint;
   char			buffer [80];
   char			*ptr1, *ptr2;
   Definition		truss, cst, htk;
    
   if (strcmp (filename, "-") == 0)
      fp = stdin;
   else {   
      fp = fopen (filename, "r");
      if (fp == NULL) {
         error ("graph: could not open %s for reading.", filename);
         return 1;
      }
   }

   InitializeProblem ( );

   constraint = CreateConstraint ("default");
   material = CreateMaterial ("default");

   truss = LookupDefinition ("truss");
   cst   = LookupDefinition ("CSTPlaneStress");
   htk   = LookupDefinition ("htk");

   i = 0;
   while (fgets (buffer, 80, fp) != NULL) {
      if (strcmp (buffer, "\n") != 0) {
         x = strtod (buffer, &ptr1);
         ptr1 = strchr (buffer, ' ');
         y = strtod (ptr1+1, &ptr2);
         ptr2 = strchr (ptr1+1, ' ');
         z = atof (ptr2+1);
         node [i] = AddNode (x, y, z, constraint, NULL);
         i++;
      }
      else { 
         if (i == 2)
            AddElement (truss, node, material, NULL, 0);
         else if (i == 3)
            AddElement (cst, node, material, NULL, 0);
         else if (i == 4)
            AddElement (htk, node, material, NULL, 0);
         else {
            error ("graph: can only handle 2, 3, and 4 node elements.\n");
            return 1;
         }

         i = 0;
      }
   }

   problem.nodes = CoalesceNodes (problem.nodes, problem.elements,
                                  &(problem.num_nodes), problem.num_elements);

   if (fp != stdin)
      fclose (fp);

   return 0;
}

int WriteGraphFile (char *filename)
{
   FILE		*output;
   unsigned	i,j;
   Element	e;

   if (strcmp (filename, "-") == 0)
      output = stdout;
   else {   
      output = fopen (filename, "w");
      if (output == NULL) {
         error ("graph: could not open %s for writing.", filename);
         return 1;
      }
   }

   for (i = 1 ; i <= problem.num_elements ; i++) {

      e = problem.elements [i];

      for (j = 1 ; j <= e -> definition -> shapenodes ; j++) {
         if (e -> node[j] == NULL) 
            break;

         fprintf (output,"%g %g %g\n", e -> node [j] -> x,
                  e -> node [j] -> y, e -> node [j] -> z); 
      }

      if (e -> definition -> shapenodes > 2)
         fprintf (output,"%g %g %g\n", e -> node [1] -> x,
                  e -> node [1] -> y, e -> node [1] -> z); 

      fprintf (output,"\n");
   }

   if (output != stdout)
      fclose (output);

   return 0;
}
