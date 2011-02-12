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

node_t::node_t(unsigned number)
{
    this -> number     = number;
    this -> m          = 0.0;
    this -> force      = NULL;
    this -> eq_force.clear();
    this -> constraint = NULL;
    this -> aux        = NULL;
    this -> stress.clear();
    this -> numelts    = 0;

    for (unsigned i = 1; i <= 6; i ++)
        this -> dx [i] = 0;
}

node_t::~node_t()
{
    Deallocate (this -> aux);
    this->eq_force.clear();
    this->stress.clear();
}

element_t::element_t(unsigned number, Definition defn)
{
    this->node.clear();
    if (defn)
        this->node.resize(defn->numnodes, NULL);
    
    this -> K  	  = NullMatrix;
    this -> M	  = NullMatrix;
    this -> f	  = NullMatrix;
    this -> aux 	  = NULL;
    this -> number 	  = number;
    this -> definition = defn;

    this -> material = NULL;
    this -> numdistributed = 0;

    for (unsigned i = 0; i <= 3; i ++)
        this -> distributed [i] = NULL;

    this -> stress = NULL;
    this -> ninteg = 0;
}

element_t::~element_t()
{
    for (unsigned i = 1; i <= this -> ninteg; i ++) {
        this->stress[i]->values.clear();
        delete this->stress[i];
    }
    this->stress.clear();
    Deallocate (this -> aux);
}

force_t::force_t(const char *name)
{
    this -> aux = NULL;
    this -> color = "";
    if (name)
        this -> name = name;
    for (unsigned i = 0; i < 7; i ++) {
        this -> force [i].value = 0;
        this -> force [i].expr  = NULL;
        this -> force [i].text  = NULL;
        this -> spectrum [i].value = 0;
        this -> spectrum [i].expr  = NULL;
        this -> spectrum [i].text  = NULL;
    }
}

force_t::~force_t()
{
    for (unsigned i = 1; i <= 6; i ++) {
        FreeCode (this -> force [i].expr);
        Deallocate (this -> force [i].text);
    }
    //Deallocate (force -> name);
    //Deallocate (force -> color);
    Deallocate (this -> aux);
}

material_t::material_t(const char *name)
{
    this -> aux   = NULL;
    this -> color = "";
    if (name)
        this -> name  = name;
    this -> E     = 0;
    this -> Ix    = 0;
    this -> Iy    = 0;
    this -> Iz    = 0;
    this -> A     = 0;
    this -> J     = 0;
    this -> G     = 0;
    this -> t     = 0;
    this -> rho   = 0;
    this -> nu    = 0;
    this -> kappa = 0;
    this -> Rk    = 0;
    this -> Rm    = 0;
    this -> Kx    = 0;
    this -> Ky    = 0;
    this -> Kz    = 0;
    this -> c     = 0;
}

material_t::~material_t()
{
    //Deallocate (material -> name);
    //Deallocate (material -> color);
    Deallocate (this -> aux);
}

constraint_t::constraint_t(const char *name)
{
    this -> aux = NULL;
    this -> color = "";
    if (name)
        this -> name = name;
    for (unsigned i = 0; i < 7; i ++) {
        this -> constraint [i] = 0;
        this -> dx [i].expr  = NULL;
        this -> dx [i].text  = NULL;
        this -> dx [i].value = 0;
        this -> ix [i] = 0;
    }

    for (unsigned i = 0; i < 4; i ++) {
        this -> vx [i] = 0;
        this -> ax [i] = UnspecifiedValue;
    }
}

constraint_t::~constraint_t()
{
	for (unsigned i = 1; i <= 6; i ++) {
	    FreeCode (this -> dx [i].expr);
	    Deallocate (this -> dx [i].text);
	}
    Deallocate (this -> aux);
}

distributed_t::distributed_t(const char *name, unsigned int nvalues)
{
    this -> aux = NULL;
    this -> color = "";
    this -> value = NULL;

    if (name)
        this -> name = name;
    this -> value.resize(nvalues);
}

distributed_t::~distributed_t()
{
    this->value.clear();
    Deallocate (this -> aux);
}

loadcase_t::loadcase_t(const char *name)
{
    if (name)
        this -> name = name;
}

loadcase_t::~loadcase_t()
{
    // NO-OP
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
