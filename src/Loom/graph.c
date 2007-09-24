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

/************************************************************************
 * File:	graph.c							*
 *									*
 * Description:	This file contains the function definitions for 	*
 *		writing line graph data files				*
 ************************************************************************/

# include <stdio.h>
# include <string.h>
# include "fe.h"
# include "error.h"
# include "problem.h"
# include "allocate.h"

void WriteLineGraph (d, alt_title, xlabel, ylabel, output)
   Matrix	d;
   char		*alt_title;
   char		*xlabel;
   char		*ylabel;
   char		*output;
{
   static char *symbols [ ] = {"", "Tx", "Ty", "Tz", "Rx", "Ry", "Rz"};
   unsigned	i,j;
   unsigned	numcurves;
   unsigned	numpoints;
   FILE		*fp;

   fp = fopen(output, "w");
   if (fp == NULL)
      Fatal("temporary file error -> line graph");

   numcurves = analysis.numdofs * analysis.numnodes;
   numpoints = Mrows(d);

   if (problem.title == NULL || strcmp(problem.title, "") == 0)
      fprintf(fp, "%s\n", alt_title);
   else
      fprintf(fp, "%s\n", problem.title);

   fprintf(fp, "%s\n", xlabel);
   fprintf(fp, "%s\n", ylabel);
   fprintf(fp, "%d\n", numcurves);
   fprintf(fp, "%d\n", numpoints);
   for (i = 1 ; i <= analysis.numnodes ; i++) 
      for (j = 1 ; j <= analysis.numdofs ; j++) 
         fprintf (fp, "%s (%d)\n", symbols [(int) analysis.dofs [j]], analysis.nodes [i] -> number);

   for (j = 1 ; j <= numpoints ; j++) {
      fprintf (fp, "%g ", (j - 1)*analysis.step + analysis.start);

      for (i = 1 ; i <= numcurves ; i++) 
         fprintf (fp, "%g ", mdata(d, j, i));

      fprintf (fp, "\n");
   }

   fclose (fp);

   return;
}

void WriteLineGraphTransferFunctions (H, forced, numforced, output)
   Matrix	*H;
   unsigned	*forced;
   unsigned	numforced;
   char		*output;
{
   static char *symbols [ ] = {"", "Tx", "Ty", "Tz", "Rx", "Ry", "Rz"};
   unsigned	i,j,l;
   unsigned	nn;
   unsigned	numcurves;
   unsigned	numpoints;
   FILE		*fp;
   unsigned	inode, idof;

   fp = fopen(output, "w");
   if (fp == NULL)
      Fatal("temporary file error -> line graph transfer function");

   nn = analysis.numdofs * analysis.numnodes;
   numcurves = nn * numforced;
   numpoints = Mrows(H [1]);

   if (problem.title == NULL || strcmp(problem.title, "") == 0)
      fprintf(fp, "Spectral Transfer Functions\n");
   else
      fprintf(fp, "%s\n", problem.title);

   fprintf(fp, "frequency\n");
   fprintf(fp, "H\n");
   fprintf(fp, "%d\n", numcurves);
   fprintf(fp, "%d\n", numpoints);
   for (i = 1 ; i <= numforced ; i++) {
      LocalDOF (forced [i], &inode, &idof);
      for (j = 1 ; j <= nn ; j++) 
         fprintf (fp, "%s(%d)->%s(%d)\n",
                  symbols[idof], inode, 
                  symbols[(int) analysis.dofs[(j-1) % analysis.numdofs + 1]],
                  analysis.nodes [(j-1) % analysis.numnodes + 1] -> number);
   }

   for (j = 1 ; j <= numpoints ; j++) {
      fprintf (fp, "%g ", (j - 1)*analysis.step + analysis.start);

      for (i = 1 ; i <= numforced ; i++) 
         for (l = 1 ; l <= nn ; l++) 
            fprintf (fp, "%g ", mdata(H [i], j, l));

      fprintf (fp, "\n");
   }

   fclose (fp);

   return;
}
