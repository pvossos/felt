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
 * File:	scale.c
 * 
 * Description:	contains routines to scale the numeric quantities in a FElt
 *		file - generally for the purpose of unit conversion.
 *
 ****************************************************************************/

# include "problem.h"
# include "error.h"

static double	length;
static double	force;
static double	mass;
static double	stress;
static double	density;
static double	length4;
static double	length2;

static int ScaleMaterial (item)
   Item	item;
{
   Material m = (Material) item;

   m -> E *= stress;
   m -> G *= stress;

   m -> t *= length;
   m -> A *= length2;

   m -> Ix *= length4;
   m -> Iy *= length4;
   m -> Iz *= length4;
   m -> J *= length4;

   m -> rho *= density;

   return 0;
}

static int ScaleDistributed (item)
   Item	item;
{
   Distributed	d = (Distributed) item;

   if (d -> nvalues > 2) 
      error ("cannot convert distributed load %s (more than 2 values).", d -> name); 
   else {
      d -> value [1].magnitude *= mass;
      d -> value [2].magnitude *= mass;
   }

   return 0;
}

static int ScaleForce (item)
   Item	item;
{
   Force 	f = (Force) item;
   unsigned	i;

   for (i = 1 ; i <= 6 ; i++) {
      if (f -> force [i].value)
         f -> force [i].value *= force;
 
      if (f -> force [i].expr != NULL)
         error ("expression not converted in force %s.", f -> name);
   }

   return 0;
}

static int ScaleConstraint (item)
   Item	item;
{
   Constraint	c = (Constraint) item;
   unsigned	i;

   for (i = 1 ; i <= 6 ; i++)  {

      if (c -> constraint [i]) { 
         c -> dx [i].value *= length; 
         if (i <= 4) {
            if (c -> ax [i])
               c -> ax [i] *= length; 

            if (c -> vx [i])
               c -> vx [i] *= length; 
         }
      } 

   }

   return 0;
}

static int ScaleNode (item)
   Item	item;
{
   Node	n = (Node) item;

   n -> x *= length;
   n -> y *= length;
   n -> z *= length;

   return 0;
}

# define ScaleConfig(x) \
	if (appearance.x != UnspecifiedValue) appearance.x *= length

static void ScaleAppearance ( )
{
   FigInfo	*f;
   unsigned	i, j;

   ScaleConfig(x_min);
   ScaleConfig(x_max);
   ScaleConfig(y_min);
   ScaleConfig(y_min);

   ScaleConfig(snap_size);
   ScaleConfig(grid_size);
   
   ScaleConfig(scale);

   for (i = 0 ; i < appearance.num_figures ; i++) {
      f = &(appearance.figures [i]);

      switch (f -> type) {

      case RECTANGLE:
      case ARC:
         f -> x *= length;
         f -> y *= length;
         f -> width *= length;
         f -> height *= length;
         break;

      case POLYLINE:
         for (j = 0 ; j < f -> num_points ; j++) {
            f -> points [j].x *= length;
            f -> points [j].y *= length;
         }
         break;

      case TEXT:
         f -> x *= length;
         f -> y *= length;
         break;

      }
   } 
}

void ScaleFeltFile (l, f)
   double	l;
   double	f;
{
   length = l;
   force = f;

	/*
	 * some derived unit conversions
	 */

   length2 = l*l;
   length4 = length2*length2;
   stress = force / length2;
   mass = force / length;
   density = mass / (length2*length);

   TreeSetIterator (problem.node_tree, ScaleNode);
   TreeIterate (problem.node_tree);

   TreeSetIterator (problem.material_tree, ScaleMaterial);
   TreeIterate (problem.material_tree);

   TreeSetIterator (problem.distributed_tree, ScaleDistributed);
   TreeIterate (problem.distributed_tree);

   TreeSetIterator (problem.force_tree, ScaleForce);
   TreeIterate (problem.force_tree);

   TreeSetIterator (problem.constraint_tree, ScaleConstraint);
   TreeIterate (problem.constraint_tree);

   ScaleAppearance ( );

   return;
}
