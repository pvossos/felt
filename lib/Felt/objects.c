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
 * File:	objects.c						*
 *									*
 * Description:	This file contains the function definitions for		*
 *		operations on the various objects.			*
 ************************************************************************/

# include <stdio.h>
# include <string.h>
# include "fe.h"
# include "error.h"
# include "objects.h"
# include "allocate.h"

# ifdef NEED_STRDUP
extern char *strdup ( );
# endif

/************************************************************************
 * Function:	 CreateNode						*
 *									*
 * Description:	 CreateNode creates and initializes a node structure.	*
 *		 The node number is assigned and all pointer fields are	*
 *		 set to NULL.						*
 ************************************************************************/

Node CreateNode (number)
    unsigned number;
{
    Node     node;
    unsigned i;


    if (!(node = AllocNew (struct node)))
	Fatal ("unable to allocate memory for new node");

    node -> number     = number;
    node -> m          = 0.0;
    node -> force      = NULL;
    node -> eq_force   = NULL;
    node -> constraint = NULL;
    node -> aux        = NULL;
    node -> stress     = NULL;
    node -> numelts    = 0;

    for (i = 1; i <= 6; i ++)
	node -> dx [i] = 0;

    return node;
}


/************************************************************************
 * Function:	 DestroyNode						*
 *									*
 * Description:	 DestroyNode deallocates a node structure.  The		*
 *		 auxillary pointer and equivalent force vector are	*
 *		 deallocated.						*
 ************************************************************************/

void DestroyNode (node)
    Node node;
{
    if (node) {
	Deallocate (node -> aux);
	Deallocate (node -> eq_force);
	Deallocate (node);
    }
}


/************************************************************************
 * Function:	 CreateElement						*
 *									*
 * Description:	 CreateElement creates and initializes a new element	*
 *		 structure.  The element number and definition are	*
 *		 assigned, the array of node pointers is allocated and	*
 *		 if definition is not NULL, and all other pointer	*
 *		 fields are initialized to NULL.			*
 ************************************************************************/

Element CreateElement (number, defn)
    unsigned   number;
    Definition defn;
{
    unsigned i;
    Element  element;


    if (!(element = AllocNew (struct element)))
	Fatal ("unable to allocate memory for new element");

    element -> node = NULL;

    if (defn && !(element -> node = Allocate (Node, defn -> numnodes)))
	Fatal ("unable to allocate memory for nodes of new element");

    element -> K  	  = NullMatrix;
    element -> M	  = NullMatrix;
    element -> f	  = NullMatrix;
    element -> aux 	  = NULL;
    element -> number 	  = number;
    element -> definition = defn;

    if (element -> node) {
	UnitOffset (element -> node);
	for (i = 1; i <= defn -> numnodes; i ++)
	    element -> node [i] = NULL;
    }

    element -> material = NULL;
    element -> numdistributed = 0;

    for (i = 0; i <= 3; i ++)
	element -> distributed [i] = NULL;

    element -> stress = NULL;
    element -> ninteg = 0;

    return element;
}


/************************************************************************
 * Function:	 DestroyElement						*
 *									*
 * Description:	 DestroyElement deallocates an element structure.  The	*
 *		 array of pointers to nodes, the array of stresses, and	*
 *		 the structure pointed to by the auxillary pointer are	*
 *		 deallocated.						*
 ************************************************************************/

void DestroyElement (element)
    Element element;
{
    unsigned i;


    if (element) {
        for (i = 1; i <= element -> ninteg; i ++) {
	    ZeroOffset (element -> stress [i] -> values);	
	    Deallocate (element -> stress [i] -> values);		
	    Deallocate (element -> stress [i]);
	}
	ZeroOffset (element -> stress);
	Deallocate (element -> stress);
	ZeroOffset (element -> node);
	Deallocate (element -> node);
	Deallocate (element -> aux);
	Deallocate (element);
    }
}


/************************************************************************
 * Function:	 CreateForce						*
 *									*
 * Description:	 CreateForce creates and initializes a force structure.	*
 *		 The name of the force is assigned (not copied) and the	*
 *		 force components are initialized to zero.		*
 ************************************************************************/

Force CreateForce (name)
    char *name;
{
    int   i;
    Force force;


    if (!(force = AllocNew (struct force)))
	Fatal ("unable to allocate memory for new force");

    force -> aux = NULL;
    force -> color = NULL;
    force -> name = name;
    for (i = 0; i < 7; i ++) {
	force -> force [i].value = 0;
	force -> force [i].expr  = NULL;
	force -> force [i].text  = NULL;
	force -> spectrum [i].value = 0;
	force -> spectrum [i].expr  = NULL;
	force -> spectrum [i].text  = NULL;
    }

    return force;
}


/************************************************************************
 * Function:	 DestroyForce						*
 *									*
 * Description:	 DestroyForce deallocates a force structure.  The name	*
 *		 and auxillary pointer are also deallocated.		*
 ************************************************************************/

void DestroyForce (force)
    Force force;
{
    int i;


    if (force) {
	for (i = 1; i <= 6; i ++) {
	    FreeCode (force -> force [i].expr);
	    Deallocate (force -> force [i].text);
	}
	Deallocate (force -> name);
	Deallocate (force -> color);
	Deallocate (force -> aux);
	Deallocate (force);
    }
}


/************************************************************************
 * Function:	 CreateMaterial						*
 *									*
 * Description:	 CreateMaterial creates and initializes a new material	*
 *		 structure.  The name is assigned (not copied) and the	*
 *		 fields are initialized to zero.			*
 ************************************************************************/

Material CreateMaterial (name)
    char *name;
{
    Material material;


    if (!(material = AllocNew (struct material)))
	Fatal ("unable to allocate memory for new material");

    material -> aux   = NULL;
    material -> color = NULL;
    material -> name  = name;
    material -> E     = 0;
    material -> Ix    = 0;
    material -> Iy    = 0;
    material -> Iz    = 0;
    material -> A     = 0;
    material -> J     = 0;
    material -> G     = 0;
    material -> t     = 0;
    material -> rho   = 0;
    material -> nu    = 0;
    material -> kappa = 0;
    material -> Rk    = 0;
    material -> Rm    = 0;
    material -> Kx    = 0;
    material -> Ky    = 0;
    material -> Kz    = 0;
    material -> c     = 0;

    return material;
}


/************************************************************************
 * Function:	 DestroyMaterial					*
 *									*
 * Description:	 DestroyMaterial deallocates a material structure.  The	*
 *		 name and auxillary structure are also deallocated.	*
 ************************************************************************/

void DestroyMaterial (material)
    Material material;
{
    if (material) {
	Deallocate (material -> name);
	Deallocate (material -> color);
	Deallocate (material -> aux);
	Deallocate (material);
    }
}


/************************************************************************
 * Function:	 CreateConstraint					*
 *									*
 * Description:	 CreateConstraint creates and initializes a new		*
 *		 constraint structure.  The name is assigned (not	*
 *		 copied) and the fields are initialized to zero.	*
 ************************************************************************/

Constraint CreateConstraint (name)
    char *name;
{
    int        i;
    Constraint constraint;


    if (!(constraint = AllocNew (struct constraint)))
	Fatal ("unable to allocate memory for new constraint");

    constraint -> aux = NULL;
    constraint -> color = NULL;
    constraint -> name = name;
    for (i = 0; i < 7; i ++) {
	constraint -> constraint [i] = 0;
	constraint -> dx [i].expr  = NULL;
	constraint -> dx [i].text  = NULL;
	constraint -> dx [i].value = 0;
	constraint -> ix [i] = 0;
    }

    for (i = 0; i < 4; i ++) {
	constraint -> vx [i] = 0;
	constraint -> ax [i] = UnspecifiedValue;
    }

    return constraint;
}


/************************************************************************
 * Function:	 DestroyConstraint					*
 *									*
 * Description:	 DestroyConstraint deallocates a constraint structure.	*
 *		 The name and auxillary structure are also deallocated.	*
 ************************************************************************/

void DestroyConstraint (constraint)
    Constraint constraint;
{
    int i;


    if (constraint) {
	for (i = 1; i <= 6; i ++) {
	    FreeCode (constraint -> dx [i].expr);
	    Deallocate (constraint -> dx [i].text);
	}
	Deallocate (constraint -> name);
	Deallocate (constraint -> color);
	Deallocate (constraint -> aux);
	Deallocate (constraint);
    }
}


/************************************************************************
 * Function:	 CreateDistributed					*
 *									*
 * Description:	 CreateDistributed creates and initializes a new	*
 *		 distributed structure.  The name is assigned (not	*
 *		 copied) and the array of values allocated.		*
 ************************************************************************/

Distributed CreateDistributed (name, nvalues)
    char    *name;
    unsigned nvalues;
{
    Distributed distributed;


    if (!(distributed = AllocNew (struct distributed)))
	Fatal ("unable to allocate memory for new distributed");

    distributed -> aux = NULL;
    distributed -> color = NULL;
    distributed -> value = NULL;

    if (nvalues && !(distributed -> value = Allocate (Pair, nvalues)))
	Fatal ("unable to allocate memory values for new distributed");

    distributed -> name = name;
    distributed -> nvalues = nvalues;
    UnitOffset (distributed -> value);

    return distributed;
}


/************************************************************************
 * Function:	 DestroyDistributed					*
 *									*
 * Description:	 DestroyDistributed deallocates a distributed		*
 *		 structure.  The array of values, name, and auxillary	*
 *		 structure are deallocated.				*
 ************************************************************************/

void DestroyDistributed (distributed)
    Distributed distributed;
{
    if (distributed) {
	ZeroOffset (distributed -> value);
	Deallocate (distributed -> value);
	Deallocate (distributed -> name);
	Deallocate (distributed -> color);
	Deallocate (distributed -> aux);
	Deallocate (distributed);
    }
}


/************************************************************************
 * Function:	 CreateLoadCase						*
 *									*
 * Description:	 CreateLoadCase creates and initializes a new		*
 *		 load case structure.  The name is assigned (not	*
 *		 copied) and the array of values allocated.		*
 ************************************************************************/

LoadCase CreateLoadCase (name)
    char    *name;
{
    LoadCase	loadcase;


    if (!(loadcase = AllocNew (struct loadcase)))
	Fatal ("unable to allocate memory for new load case");

    loadcase -> numforces = 0;
    loadcase -> numloads  = 0;
    loadcase -> nodes     = NULL;
    loadcase -> elements  = NULL;
    loadcase -> forces    = NULL;
    loadcase -> loads     = NULL;

    loadcase -> name = name;

    return loadcase;
}


/************************************************************************
 * Function:	 DestroyLoadCase					*
 *									*
 * Description:	 DestroyLoadCase deallocates a load case 		*
 *		 structure.                                           	* 
 ************************************************************************/

void DestroyLoadCase (loadcase)
    LoadCase	loadcase;
{
    if (loadcase) {
        if (loadcase -> forces) {
	   ZeroOffset (loadcase -> forces);
           Deallocate (loadcase -> forces);
        }
        if (loadcase -> loads) {
	   ZeroOffset (loadcase -> loads); 
           Deallocate (loadcase -> loads);
        }
        if (loadcase -> nodes) {
	   ZeroOffset (loadcase -> nodes); 
           Deallocate (loadcase -> nodes);
        }
        if (loadcase -> elements) {
	   ZeroOffset (loadcase -> elements);
           Deallocate (loadcase -> elements);
        }

	Deallocate (loadcase);
    }
}
/************************************************************************
 * Function:	AssignForce						*
 *									*
 * Description:	Assigns a force given as a piece of code to a force	*
 *		structure.						*
 ************************************************************************/

void AssignForce (force, dof, expr, text)
    Force force;
    DOF   dof;
    Code  expr;
    char *text;
{
    Deallocate (force -> force [dof].text);
    FreeCode (force -> force [dof].expr);

    force -> force [dof].value = EvalCode (expr, 0.0);
    force -> force [dof].expr  = IsConstant (expr) ? NULL : CopyCode (expr);
    force -> force [dof].text  = text ? strdup (text) : NULL;
}

/************************************************************************
 * Function:	AssignSpectrum						*
 *									*
 * Description:	Assigns a spectrum given as a piece of code to a force	*
 *		structure.						*
 ************************************************************************/

void AssignSpectrum (force, dof, expr, text)
    Force force;
    DOF   dof;
    Code  expr;
    char *text;
{
    Deallocate (force -> spectrum [dof].text);
    FreeCode (force -> spectrum [dof].expr);

    force -> spectrum [dof].value = EvalCode (expr, 0.0);
    force -> spectrum [dof].expr  = IsConstant (expr) ? NULL : CopyCode (expr);
    force -> spectrum [dof].text  = text ? strdup (text) : NULL;
}

/************************************************************************
 * Function:	AssignConstraint					*
 *									*
 * Description:	Assigns a boundary condition given as a piece of code   *
 *		to the boundary value part of a constraint structure	*
 *              (Tx=, Ty=, Tz=, Rx=, Ry=, Rz=)				*
 ************************************************************************/

void AssignConstraint (constraint, dof, expr, text, symbol)
    Constraint	constraint;
    DOF   	dof;
    Code  	expr;
    char       *text;
    int  	symbol;
{
    Deallocate (constraint -> dx [dof].text);
    FreeCode (constraint -> dx [dof].expr);

    constraint -> constraint [dof] = symbol;
    constraint -> dx [dof].value   = EvalCode (expr, 0.0);
    constraint -> dx [dof].expr    = IsConstant (expr) ? NULL : CopyCode (expr);
    constraint -> dx [dof].text    = text ? strdup (text) : NULL;
}
