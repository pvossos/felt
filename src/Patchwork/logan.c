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
 * File:	logan.c
 *
 * Description:	contains code to write and ?read? output files used 
 *		formatted for the software that comes with Logan's book
 *		
 *****************************************************************************/

# include <stdio.h>
# include <string.h>
# include "fe.h"
# include "error.h"
# include "problem.h"
# include "patchwork.h"

Material *SetupMaterialArray    ( );
int      CountSurfaceTractions  ( );
int	 GetKode                ( );

int
WriteLoganFile(char *name)
{
    FILE	*fp;			/* output file pointer		*/
    unsigned	 i,			/* loop variable		*/
		 j;			/* loop variable		*/
    unsigned	 node1,
	         node2;
    float	 mag1,
		 mag2;
    float	 ulx,vly;		/* prescribed load or displ.	*/
    int		 code;			/* plane stress or plane strain	*/
    int		 nummat;		/* number of material props.    */
    int		 numst;			/* number of surface tractions  */
    Material	 *material;		/* array of all materials used	*/
    char	 *element_name;

    if (strcmp (name, "-") == 0)
       fp = stdout;
    else {
       fp = fopen (name, "w"); 
       if (fp == NULL) {
          error ("could not open specified output file");
          return 1;
       }
    }
    element_name = problem.elements [1] -> definition -> name;

    if (strcmp (element_name, "truss") == 0) {
       fprintf (fp, "%s\n", problem.title);
       fprintf (fp, "%d, %d\n", problem.num_elements, problem.num_nodes);

       for (i = 1 ; i <= problem.num_nodes ; i++) {
          fprintf (fp, "%d,%d,%d,%d,%g,%g,%g,", problem.nodes [i] -> number, 
                   problem.nodes [i] -> constraint -> constraint [1],
                   problem.nodes [i] -> constraint -> constraint [2], 
                   problem.nodes [i] -> constraint -> constraint [3],
                   problem.nodes [i] -> x, 
                   problem.nodes [i] -> y, 
                   problem.nodes [i] -> z); 

          if (problem.nodes [i] -> force != NULL)
             fprintf (fp, "%g,%g,%g\n", problem.nodes [i] -> force -> force [1].value, 
                                        problem.nodes [i] -> force -> force [2].value, 
                                        problem.nodes [i] -> force -> force [3].value);
          else
             fprintf (fp, "0.0,0.0,0.0\n");
       }
       for (i = 1 ; i <= problem.num_elements ; i++)
          fprintf (fp, "%d,%d,%d,%g,%g\n", problem.elements [i] -> number,
             problem.elements [i] -> node[1] -> number, 
             problem.elements [i] -> node[2] -> number,
             problem.elements [i] -> material -> E, 
             problem.elements [i] -> material -> A);

    }

    else if (strcmp (element_name, "beam") == 0) {
       fprintf (fp, "1\n");
       fprintf (fp, "%s\n",problem.title);
       fprintf (fp, "%d,%d\n", problem.num_elements,problem.num_nodes);
       for (i = 1 ; i <= problem.num_nodes ; i++) {
          fprintf (fp, "%d,%d,%d,%d,%g,%g,%g,", problem.nodes [i] -> number, 
             problem.nodes [i] -> constraint -> constraint [1],
             problem.nodes [i] -> constraint -> constraint [2], 
             problem.nodes [i] -> constraint -> constraint [3],
             problem.nodes [i] -> x, 
             problem.nodes [i] -> y, 
             problem.nodes [i] -> z);
          if (problem.nodes [i] -> force != NULL)
             fprintf (fp, "%g,%g,%g\n", 
                      problem.nodes [i] -> force -> force [1].value, 
                      problem.nodes [i] -> force -> force [2].value, 
                      problem.nodes [i] -> force -> force [6].value);
          else
             fprintf (fp, "0.0,0.0,0.0\n");
       }
       for (i = 1 ; i <= problem.num_elements ; i++)
          fprintf (fp, "%d,%d,%d,%g,%g,%g,%g,%g\n", 
             problem.elements [i] -> number,
             problem.elements [i] -> node[1] -> number, 
             problem.elements [i] -> node[2] -> number,
             problem.elements [i] -> material -> E, 
             problem.elements [i] -> material -> G,
             problem.elements [i] -> material -> A, 
             problem.elements [i] -> material -> Ix,
             problem.elements [i] -> material -> J);

    }
    else if (strncmp (element_name, "CST", 3) == 0) {
       material = SetupMaterialArray (problem.elements, 
                                      problem.num_elements, &nummat);
       numst = CountSurfaceTractions (problem.elements, problem.num_elements);
       fprintf (fp, "%s\n", problem.title);

       if (strcmp (element_name, "CSTPlaneStress") == 0)
          code = 2;
       else 
          code = 1;

       fprintf (fp, "%d,%d,%d,%d,%d,0\n", problem.num_nodes, 
                                          problem.num_elements, 
                                          nummat, numst, code);
       fprintf (fp, "0,0\n");
       for (i = 0 ; i < nummat ; i++) 
          fprintf (fp, "%g,%g,%g,%g\n", material [i] -> E, material [i] -> nu, 
             material [i] -> rho, material [i] -> t);
       for (i = 1 ; i <= problem.num_nodes ; i++) {
          code = GetKode (problem.nodes [i], &ulx, &vly);
          fprintf (fp, "%d,%d,%g,%g,%g,%g\n", problem.nodes [i] -> number, code, 
                   problem.nodes [i] -> x, problem.nodes [i] -> y, ulx, vly); 
       }
       for (i = 1 ; i <= problem.num_elements ; i++) {
          for (j = 0 ; j < nummat ; j++) {
             if (problem.elements [i] -> material == material [j]) {
                code = j+1;
                break;
             }
          }
          fprintf (fp, "%d,%d,%d,%d,%d,%d\n", problem.elements [i] -> number, 
             problem.elements [i] -> node[1] -> number,
             problem.elements [i] -> node[2] -> number, 
             problem.elements [i] -> node[3] -> number,
             problem.elements [i] -> node[3] -> number, code);
       }
       if (numst > 0) {
          for (i = 1 ; i <= problem.num_elements ; i++) {
             if (problem.elements [i] -> numdistributed != 0) {
                node1 = problem.elements[i] -> distributed [1] -> value[1].node;
                node2 = problem.elements[i] -> distributed [1] -> value[2].node;
                mag1 = problem.elements[i] -> distributed [1] -> value[1].magnitude;
                mag2 = problem.elements[i] -> distributed [1] -> value[2].magnitude;

                if (problem.elements [i] -> distributed [1] -> direction == 1)
                   fprintf (fp, "%d,%d,%g,%g,0.0,0.0\n",
                      problem.elements[i] -> node[node1] -> number,
                      problem.elements[i] -> node[node2] -> number, mag1, mag2);
                else if (problem.elements [i] -> distributed [1] -> direction == 2)
                   fprintf (fp, "%d,%d,0.0,0.0,%g,%g\n",
                      problem.elements[i] -> node[node1] -> number,
                      problem.elements[i] -> node[node2] -> number, mag1, mag2);
                else {
                   error ("could not resolve surface traction for elt %d",i);
                   return 1;
                }
             }
          }
       }
    }
    else {
       error ("logan: can only convert truss, beam and CST elements");
       return 0; 
    }

    if (fp != stdout)
       fclose (fp);

    return 0;
}

int GetKode (node, ulx, vly)
   Node		node;
   float	*ulx,
		*vly;
{
   unsigned	code;

   if (node -> constraint -> constraint [1] && 
       node -> constraint -> constraint [2]) {
      code = 3;
      *ulx = 0.0;
      *vly = 0.0; 
   } else if (node -> constraint -> constraint [1]) {
      code = 1;
      *ulx = 0.0;
      if (node -> force != NULL)
         *vly = node -> force -> force [2].value;
      else
         *vly=0.0;
   } else if (node -> constraint -> constraint [2]) {
      code = 2;
      *vly = 0;
      if (node -> force != NULL)
         *ulx = node -> force -> force [1].value; 
      else
         *ulx = 0.0;
   } else {
      code = 0;
      if (node -> force != NULL) {
         *ulx = node -> force -> force [1].value;
         *vly = node -> force -> force [2].value;
      } else {
         *ulx = 0.0;
         *vly = 0.0;
      }
   }

   return code;
} 

Material *SetupMaterialArray (element, numelts, nummat)
   Element	*element;
   unsigned	numelts;
   int		*nummat;
{
   unsigned	i,j,
		count;
   static Material	material [50];
   unsigned	flag;

   count = 0;

   for (i = 1 ; i <= numelts ; i++) {
      flag = 0;
      for (j = 0 ; j < count ; j++) {
         if (element [i] -> material == material [j]) {
            flag = 1;
            break;
          }
       } 
      
       if (!flag)
          material [count++] = element [i] -> material;

       if (count >= 50)
          Fatal ("too many materials defined for conversion");
    }

    *nummat = count;

    return material;
}

int CountSurfaceTractions (element, numelts)
   Element	*element;
   unsigned	numelts;
{
   unsigned	i, count;

   count = 0;

   for (i = 1 ; i <= numelts ; i++) {
      if (element [i] -> numdistributed != 0) 
         count++;
   }

   return count;
}
