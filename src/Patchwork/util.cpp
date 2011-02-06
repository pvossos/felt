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
 * File:	util.c
 *
 * Description:	contains utility routines for format conversion
 *
 *****************************************************************************/

# include <math.h>
# include <stdio.h>
# include <string.h>
# include <stdlib.h>
# include "fe.h"
# include "problem.h"
# include "objects.h"
# include "Tree.h"
# include "patchwork.h"

int
InitializeProblem(void)
{
   ReadFeltFile (NULL);

   problem.nodes.clear();
   problem.elements.clear();

   return 0;
}

Node
AddNode(double x, double y, double z, Constraint constraint, Force force)
{
   Node		node;

   node = CreateNode (problem.nodes.size()+1);

   node -> x = x;
   node -> y = y;
   node -> z = z;

   node -> force = force;
   node -> constraint = constraint;

   problem.nodes.push_back(node);

   if (force) 
      TreeInsert (problem.force_tree, (Item) force);

   if (constraint)
      TreeInsert (problem.constraint_tree, (Item) constraint);

   return node;
}

Element
AddElement(Definition defn, Node *nodes, Material material, 
           Distributed *distributed, unsigned numdistributed)
{
   unsigned	i;
   Element	element;

   element = CreateElement (problem.elements.size()+1, defn);
   for (i = 1 ; i <= defn -> numnodes ; i++)
      element -> node [i] = nodes [i-1]; 

   element -> material = material;

   for (i = 1 ; i <= numdistributed ; i++) {
      element -> distributed [i] = distributed [i-1];
      TreeInsert (problem.distributed_tree, distributed [i-1]);
   }

   problem.elements.push_back(element);

   if (material) 
      TreeInsert (problem.material_tree, (Item) material);

   return element;
}
