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
 * File:	objects.h						*
 *									*
 * Description:	This file contains the function declarations for the	*
 *		operations on the various objects.			*
 ************************************************************************/

# ifndef _OBJECTS_H
# define _OBJECTS_H
# include "fe.h"
# include "Tree.h"

/*!
  CreateDistributed creates and initializes a new distributed
  structure.  The name is assigned (not copied) and the array of
  values allocated.
*/
Distributed CreateDistributed(char *name, unsigned int nvalues);

/*!
  DestroyDistributed deallocates a distributed structure.  The array
  of values, name, and auxillary structure are deallocated.
*/
void DestroyDistributed(Distributed distributed);

/*!
  CreateForce creates and initializes a force structure.  The name of
  the force is assigned (not copied) and the force components are
  initialized to zero.
*/
Force CreateForce(char *name);

/*!
  DestroyForce deallocates a force structure.  The name and auxillary
  pointer are also deallocated.
*/
void DestroyForce(Force force);

/*!
Assigns a force given as a piece of code to a force structure.
*/
void AssignForce(Force force, DOF dof, Code expr, char *text);

/*!
  Assigns a spectrum given as a piece of code to a force	
  structure.						
*/
void AssignSpectrum(Force force, DOF dof, Code expr, char *text);

/*!
  CreateConstraint creates and initializes a new constraint structure.
  The name is assigned (not copied) and the fields are initialized to
  zero.
*/
Constraint CreateConstraint(char *name);

/*!
  DestroyConstraint deallocates a constraint structure.  The name
  and auxillary structure are also deallocated.
*/
void DestroyConstraint(Constraint constraint);

/*!
  Assigns a boundary condition given as a piece of code to the
  boundary value part of a constraint structure (Tx=, Ty=, Tz=, Rx=,
  Ry=, Rz=).
 */
void AssignConstraint(Constraint constraint, DOF dof, Code expr, char *text, int symbol);

/*!
  CreateMaterial creates and initializes a new material structure.
  The name is assigned (not copied) and the fields are initialized to
  zero.
*/
Material CreateMaterial(char *name);

/*!
  DestroyMaterial deallocates a material structure.  The name and
  auxillary structure are also deallocated.
*/
void DestroyMaterial(Material material);

/*!
  CreateNode creates and initializes a node structure.  The node
  number is assigned and all pointer fields are set to NULL.
*/
Node CreateNode(unsigned int number);

/*!
  DestroyNode deallocates a node structure.  The auxillary pointer and
  equivalent force vector are deallocated.
*/
void DestroyNode(Node node);

/*!
  CreateElement creates and initializes a new element structure.  The
  element number and definition are assigned, the array of node
  pointers is allocated and if definition is not NULL, and all other
  pointer fields are initialized to NULL.
*/
Element CreateElement(unsigned int number, Definition defn);

/*!
  DestroyElement deallocates an element structure.  The	
  array of pointers to nodes, the array of stresses, and	
  the structure pointed to by the auxillary pointer are	
  deallocated.						
*/
void DestroyElement(Element element);

/*!
  CreateLoadCase creates and initializes a new load case structure.
  The name is assigned (not copied) and the array of values allocated.
*/
LoadCase CreateLoadCase(char *name);

/*!
DestroyLoadCase deallocates a load case structure.
*/
void DestroyLoadCase(LoadCase loadcase);

# endif /* _OBJECTS_H */
