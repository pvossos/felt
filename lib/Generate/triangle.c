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

/****************************************************************************
 *
 * File:	triangle.c
 *
 ***************************************************************************/

# include <math.h>
# include <stdio.h>
# include "allocate.h"
# include "error.h"
# include "fe.h"
# include "objects.h"
# include "mesh.h"

typedef double dbl_pair [2];
typedef int    triple_int [3];

void geompk_ ( );

/****************************************************************************
 *
 * Function:	GenerateTriMesh
 *
 * Description:	a procedure to interface to Geompack and generate a mesh
 *		of triangular elements. 
 *
 ****************************************************************************/

unsigned GenerateTriMesh (trimesh,element,node,numelts,numnodes,bnode,belement)
   TriMesh	trimesh;
   Element	**element;
   Node		**node;
   unsigned	*numelts;
   unsigned	*numnodes;
   unsigned	bnode;
   unsigned	belement;
{
   unsigned	i,j;
   unsigned	count;
   int		ne, nn;
   int		nvertices,
		*nvbc;
   double	(*vcl) [2];
   int		(*eln) [3];
   int		status;

   nvertices = 0;
   
   if (trimesh -> numcurves <= 0) {
      error ("must have at least a boundary curve to generate a TriMesh");
      return 1;
   }

   if (trimesh -> definition -> numnodes != 3) {
      error ("TriMesh generation requires three node elements");
      return 1;
   }

   vcl = Allocate (dbl_pair, 4*trimesh -> max);
   eln = Allocate (triple_int, 2*trimesh -> max);

   if (vcl == NULL || eln == NULL)
      Fatal ("allocation error creating TriMesh");

   nvbc = Allocate (int, trimesh -> numcurves);

   if (nvbc == NULL)
      Fatal ("allocation error creating TriMesh");

   count = 0;
   
   for (i = 0 ; i < trimesh -> numcurves ; i++) {
      nvertices += trimesh -> curves [i] -> numvc;
      nvbc [i] = trimesh -> curves [i] -> numvc;
      
      for (j = 0 ; j < trimesh -> curves[i] -> numvc ; j++) {
         vcl [count][0] = trimesh -> curves[i] -> vcl[j][0]; 
         vcl [count][1] = trimesh -> curves[i] -> vcl[j][1]; 
         count++;
      }
   }
   
   if (nvertices <= 0) {
      error ("nothing to generate");
      Deallocate (vcl);
      Deallocate (eln);

      return 1;
   }


   geompk_ (&(trimesh -> tolin), &(trimesh -> angspc), 
            &(trimesh -> angtol), &(trimesh -> kappa), 
            &(trimesh -> dmin), &(trimesh -> min),
            &(trimesh -> max), &nvertices, &(trimesh -> numcurves),
            nvbc, vcl, eln, &ne, &status);

   if (status) {
      error ("Geompack error code #%d in TriMesh generation",status);
      return 1;
   }

   if (ne <= 0) {
      error ("nothing to generate");
      return 1;
   }

	/*
	 * allocate some memory to hold everything that we will generate
	 */

   nn = nvertices;

   if (nn <= 0) {
      error ("nothing to generate");
      return 1;
   }

   if (!(*node = Allocate(Node, nn)))
      Fatal ("allocation error in TriMesh generation");

   UnitOffset (*node);

   for (i = 1 ; i <= nn ; i++) {
      if (!((*node) [i] = CreateNode (0)))
         Fatal ("allocation error in TriMesh generation");
   }

   if (!(*element = Allocate(Element, ne)))
      Fatal ("allocation error in TriMesh generation");

   UnitOffset (*element);

   for (i = 1 ; i <= ne ; i++) {
      if (!((*element) [i] = CreateElement (0, trimesh -> definition)))
         Fatal ("allocation error in TriMesh generation");
   }

	/*	
	 * generate all the nodes
	 */

   for (i = 1 ; i <= nn ; i++) {
 
      (*node) [i] -> number = i + bnode;
      (*node) [i] -> x = vcl [i-1][0];
      (*node) [i] -> y = vcl [i-1][1];
      (*node) [i] -> z = 0;

   }

	/*
	 * attach all the elements to the nodes
	 */

   for (i = 1 ; i <= ne ; i++) {

      (*element) [i] -> number = i + belement;
      (*element) [i] -> node [1] = (*node) [eln[i-1][0]];
      (*element) [i] -> node [2] = (*node) [eln[i-1][1]];
      (*element) [i] -> node [3] = (*node) [eln[i-1][2]];

   } 

   *numnodes = nn;
   *numelts  = ne;

   Deallocate (vcl);
   Deallocate (eln);

   return 0;
}
