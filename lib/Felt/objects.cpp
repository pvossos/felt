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

Node
CreateNode(unsigned int number)
{
    Node node = new struct node;
    node -> number     = number;
    node -> m          = 0.0;
    node -> force      = NULL;
    node -> eq_force.clear();
    node -> constraint = NULL;
    node -> aux        = NULL;
    node -> stress.clear();
    node -> numelts    = 0;

    for (unsigned i = 1; i <= 6; i ++)
        node -> dx [i] = 0;

    return node;
}

void
DestroyNode(Node node)
{
    if (node) {
        Deallocate (node -> aux);
        node->eq_force.clear();
        node->stress.clear();
        delete node;
    }
}

Element
CreateElement(unsigned int number, Definition defn)
{
    Element element = new struct element;

    element->node.clear();
    if (defn)
        element->node.resize(defn->numnodes, NULL);
    
    element -> K  	  = NullMatrix;
    element -> M	  = NullMatrix;
    element -> f	  = NullMatrix;
    element -> aux 	  = NULL;
    element -> number 	  = number;
    element -> definition = defn;

    element -> material = NULL;
    element -> numdistributed = 0;

    for (unsigned i = 0; i <= 3; i ++)
        element -> distributed [i] = NULL;

    element -> stress = NULL;
    element -> ninteg = 0;

    return element;
}

void
DestroyElement(Element element)
{
    if (element) {
        for (unsigned i = 1; i <= element -> ninteg; i ++) {
            element->stress[i]->values.clear();
            delete element->stress[i];
        }
        element->stress.clear();
        Deallocate (element -> aux);
        delete element;
    }
}

Force
CreateForce(const char *name)
{
    Force force = new struct force;

    force -> aux = NULL;
    force -> color = "";
    force -> name = name;
    for (unsigned i = 0; i < 7; i ++) {
        force -> force [i].value = 0;
        force -> force [i].expr  = NULL;
        force -> force [i].text  = NULL;
        force -> spectrum [i].value = 0;
        force -> spectrum [i].expr  = NULL;
        force -> spectrum [i].text  = NULL;
    }

    return force;
}

void
DestroyForce(Force force)
{
    if (force) {
        for (unsigned i = 1; i <= 6; i ++) {
            FreeCode (force -> force [i].expr);
            Deallocate (force -> force [i].text);
        }
        //Deallocate (force -> name);
        //Deallocate (force -> color);
        Deallocate (force -> aux);
        delete force;
    }
}

Material
CreateMaterial(const char *name)
{
    Material material = new struct material;

    material -> aux   = NULL;
    material -> color = "";
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

void
DestroyMaterial(Material material)
{
    if (material) {
        //Deallocate (material -> name);
        //Deallocate (material -> color);
        Deallocate (material -> aux);
        delete material;
    }
}

Constraint
CreateConstraint(const char *name)
{
    Constraint constraint = new struct constraint;

    constraint -> aux = NULL;
    constraint -> color = "";
    constraint -> name = name;
    for (unsigned i = 0; i < 7; i ++) {
        constraint -> constraint [i] = 0;
        constraint -> dx [i].expr  = NULL;
        constraint -> dx [i].text  = NULL;
        constraint -> dx [i].value = 0;
        constraint -> ix [i] = 0;
    }

    for (unsigned i = 0; i < 4; i ++) {
        constraint -> vx [i] = 0;
        constraint -> ax [i] = UnspecifiedValue;
    }

    return constraint;
}

void
DestroyConstraint(Constraint constraint)
{
    if (constraint) {
	for (unsigned i = 1; i <= 6; i ++) {
	    FreeCode (constraint -> dx [i].expr);
	    Deallocate (constraint -> dx [i].text);
	}
    Deallocate (constraint -> aux);
    delete constraint;
    }
}

Distributed
CreateDistributed(const char *name, unsigned int nvalues)
{
    Distributed distributed = new struct distributed;

    distributed -> aux = NULL;
    distributed -> color = "";
    distributed -> value = NULL;

    distributed -> name = name;
    distributed -> value.resize(nvalues);

    return distributed;
}

void
DestroyDistributed(Distributed distributed)
{
    if (distributed) {
        distributed->value.clear();
	Deallocate (distributed -> aux);
    delete distributed;
    }
}

LoadCase
CreateLoadCase(const char *name)
{
    LoadCase loadcase = new struct loadcase;
    loadcase -> name = name;
    return loadcase;
}

void
DestroyLoadCase(LoadCase loadcase)
{
    if (loadcase) {
        delete loadcase;
    }
}

void
AssignForce(Force force, DOF dof, Code expr, char *text)
{
    Deallocate (force -> force [dof].text);
    FreeCode (force -> force [dof].expr);

    force -> force [dof].value = EvalCode (expr, 0.0);
    force -> force [dof].expr  = IsConstant (expr) ? NULL : CopyCode (expr);
    force -> force [dof].text  = text ? strdup (text) : NULL;
}

void
AssignSpectrum(Force force, DOF dof, Code expr, char *text)
{
    Deallocate (force -> spectrum [dof].text);
    FreeCode (force -> spectrum [dof].expr);

    force -> spectrum [dof].value = EvalCode (expr, 0.0);
    force -> spectrum [dof].expr  = IsConstant (expr) ? NULL : CopyCode (expr);
    force -> spectrum [dof].text  = text ? strdup (text) : NULL;
}

void
AssignConstraint(Constraint constraint, DOF dof, Code expr, char *text, int symbol)
{
    Deallocate (constraint -> dx [dof].text);
    FreeCode (constraint -> dx [dof].expr);

    constraint -> constraint [dof] = symbol;
    constraint -> dx [dof].value   = EvalCode (expr, 0.0);
    constraint -> dx [dof].expr    = IsConstant (expr) ? NULL : CopyCode (expr);
    constraint -> dx [dof].text    = text ? strdup (text) : NULL;
}
