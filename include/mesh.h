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
 * File:	mesh.h							*
 *									*
 * Description:	This file contains the the public function and type	*
 *		declarations for the element generation routines.	*
 ************************************************************************/

# ifndef _MESH_H
# define _MESH_H
# include "fe.h"


typedef enum {
    LinearRule, 
    CosRule, SinRule, 
    LogRule, RevLogRule,
    ParabolicRule, RevParabolicRule, 
} Rule;

typedef struct _curve {
   unsigned	numvc;
   double	(*vcl) [2];
} *Curve;

typedef struct _trimesh {
   Definition	definition;	/* element definition			    */
   int		target;		/* target number to generate		    */
   double       alpha;          /* elt area constraint is alpha*Atot/target */
   unsigned	numcurves;	/* number of curves (first is boundary)	    */
   Curve	*curves; 	/* array of curve structures		    */
} *TriMesh;

typedef struct _line {
   Definition	definition;	/* element definition			*/
   double	xs,ys,zs;	/* starting coordinate			*/
   double	xe,ye,ze;	/* ending coordinate			*/
   unsigned	number;		/* number of lines			*/
   Rule		rule;		/* scale rule				*/
} *Line;

typedef struct _grid {
   Definition	definition;	/* element definition			*/
   double	xs,ys,zs;	/* starting coordinate			*/
   double	xe,ye,ze;	/* ending coordinate			*/
   unsigned	xnumber;	/* number of segments along x-axis	*/
   unsigned	ynumber;	/* number of segments along y-axis	*/
   unsigned	znumber;	/* number of segments along z-axis	*/
   Rule		xrule;		/* scale rule for x-axis		*/
   Rule		yrule;		/* scale rule for y-axis		*/
   Rule		zrule;		/* scale rule for z-axis		*/
} *Grid;

Node* CoalesceNodes(Node *node, Element *element, unsigned *nn, unsigned numelts);

/*!
A simple procedure to generate a 1-d line of line elements with all
the elements along a single line.
*/
unsigned 
GenerateLine (Line line, Element **element, Node **node,
              unsigned int *numelts, unsigned int *numnodes,
              unsigned int bnode, unsigned int belement);

/*!
  A simple procedure to generate a 3-d grid of line elements with all
  the elements running parallel to one of the axes.
*/
unsigned
GenerateGrid(Grid grid, Element **element, Node **node,
             unsigned int *numelts, unsigned int *numnodes, 
             unsigned int bnode, unsigned int belement);

/*!
  A simple procedure to generate a 2-d grid of quadrilateral (four
  shape nodes) elements with all the elements running parallel to one
  of the axes.
*/
unsigned
GenerateQuadGrid(Grid grid, Element **element, Node **node,
                 unsigned int *numelts, unsigned int *numnodes, 
                 unsigned int bnode, unsigned int belement);

/*!
  A simple procedure to generate a 3-d grid of solid brick (eight
  shape nodes) elements with all the elements running parallel to one
  of the axes.
 */
unsigned
GenerateBrickGrid(Grid grid, Element **element, Node **node, 
                  unsigned int *numelts, unsigned int *numnodes, 
                  unsigned int bnode, unsigned int belement);

/*!
 A procedure to interface to Triangle and generate a mesh of
 triangular elements. 
 */
unsigned
GenerateTriMesh(TriMesh trimesh, Element **element, Node **node, 
                unsigned int *numelts, unsigned int *numnodes,
                unsigned int bnode, unsigned int belement);

# endif /* _MESH_H */
