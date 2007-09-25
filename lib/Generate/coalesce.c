/*
    This file is part of the FElt finite element analysis package.
    Copyright (C) 1993, 1994 Jason I. Gobat and Darren C. Atkinson

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
 * File:	coalesce.c
 *
 ***************************************************************************/

# include <math.h>
# include "allocate.h"
# include "error.h"
# include "fe.h"
# include "objects.h"
# include "mesh.h"

# define TOLERANCE	0.001
# define DIST(a,b)	(sqrt(((a) -> x - (b) -> x)*((a) -> x - (b) -> x) + \
                              ((a) -> y - (b) -> y)*((a) -> y - (b) -> y) + \
                              ((a) -> z - (b) -> z)*((a) -> z - (b) -> z)))

/****************************************************************************
 *
 * Function:	
 *
 * Description:	
 *
 ****************************************************************************/

static int
CheckConnections(Node n1, Node n2, Element *element, unsigned numelts)
{
   unsigned	i, j, k;
   int		status;

   status = 0;
   for (i = 1 ; i <= numelts ; i++) {
      for (j = 1 ; j <= element[i] -> definition -> numnodes ; j++) {

         if (element[i] -> node[j] == n1) {
            for (k = 1 ; k <= element[i] -> definition -> numnodes ; k++) {
               if (element[i] -> node[k] == n2) {
                  status = 1;
                  break;
               }
            }
         } 
      }
      if (status)
         break;
   }
   
   return status;
}

static void
Reconnect(Node new, Node old, Element *element, unsigned numelts)
{
   unsigned	i,j;

   for (i = 1 ; i <= numelts ; i++) {
      for (j = 1 ; j <= element[i] -> definition -> numnodes ; j++) {

         if (element[i] -> node[j] == old) {
            element[i] -> node[j] = new;
            break;
         }
      }
   }

   return;
}
      
static Node*
MergeNodes(Node *node, Element *element,
           unsigned numnodes, unsigned numelts,
           unsigned *merges, unsigned merge_count)
{
   Node		*new_nodes;
   unsigned	i;
   unsigned	count;

   new_nodes = Allocate (Node, (numnodes - merge_count));
   UnitOffset (new_nodes);

   count = 0;
   for (i = 1 ; i <= numnodes ; i++) {
      if (merges [i]) {
         Reconnect (node [merges[i]], node[i], element, numelts);
         DestroyNode (node [i]);
      }
      else {
         count++;
         new_nodes [count] = node [i];
         node [i] -> number = count;
      }
   }
         
   ZeroOffset (node);
   Deallocate (node);

   return new_nodes;
}

Node*
CoalesceNodes(Node *node, Element *element, unsigned *nn, unsigned numelts)
{
   double	maxX, maxY, maxZ;
   double	minX, minY, minZ;
   double	tol;
   unsigned	i,j;
   unsigned	merge_count;
   unsigned	numnodes;
   unsigned	*merges;
   Node		*new_nodes;
   int		status;

   numnodes = *nn;

   if (numnodes == 0 || numelts == 0) {
      error ("nothing to coalesce");
      return NULL;
   }

   maxX = minX = node[1] -> x;
   maxY = minY = node[1] -> y;
   maxZ = minZ = node[1] -> z;

   for (i = 1 ; i <= numnodes ; i++) {
      if (node[i] -> x > maxX)
         maxX = node[i] -> x;
      else if (node[i] -> x < minX)
         minX = node[i] -> x;
 
      if (node[i] -> y > maxY)
         maxY = node[i] -> y;
      else if (node[i] -> y < minY)
         minY = node[i] -> y;

      if (node[i] -> z > maxZ)
         maxZ = node[i] -> z;
      else if (node[i] -> z < minZ)
         minZ = node[i] -> z;
   }

   tol = TOLERANCE * sqrt ((maxX - minX)*(maxX - minX) + 
                           (maxY - minY)*(maxY - minY) +
                           (maxZ - minZ)*(maxZ - minZ));

   merges = Allocate (unsigned, numnodes);
   UnitOffset (merges);

   for (i = 1 ; i <= numnodes ; i++)
      merges [i] = 0;

   merge_count = 0;

   for (i = 1 ; i <= numnodes ; i++) {
      for (j = i+1 ; j <= numnodes ; j++) {
      
         if (!merges [j]) {
            if (DIST(node[i], node[j]) < tol) {
               status = CheckConnections (node[i], node[j], element, numelts);
               if (!status) {
                  merge_count++;
                  merges [j] = i;
               }
            }
         }
      }
   } 

   if (merge_count) {
      new_nodes = MergeNodes(node,element,numnodes,numelts,merges,merge_count);
      numnodes -= merge_count;
   }
   else
      new_nodes = node;

   ZeroOffset (merges);
   Deallocate (merges);

   *nn = numnodes;
   return new_nodes;
}
