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
 * File:	definition.c						*
 *									*
 * Description:	This file contains the public and private function and	*
 *		variable definitions for adding, removing, and looking	*
 *		up element definitions.					*
 ************************************************************************/

# include <string.h>
# include "problem.h"
# include "definition.h"


/************************************************************************
 * Function:	DefinitionCompare					*
 *									*
 * Description:	Compares two definition structures represented as	*
 *		items.							*
 ************************************************************************/

static int DefinitionCompare (item1, item2)
    Item item1;
    Item item2;
{
    return strcmp (((Definition) item1) -> name, ((Definition) item2) -> name);
}


/************************************************************************
 * Function:	AddDefinition						*
 *									*
 * Description:	Add a definition to the set of definitions.		*
 ************************************************************************/

int AddDefinition (definition)
    Definition definition;
{
    if (!problem.definition_tree)
	problem.definition_tree = TreeCreate (DefinitionCompare);
    
    return TreeInsert (problem.definition_tree,definition) != (Item) definition;
}


/************************************************************************
 * Function:	RemoveDefinition					*
 *									*
 * Description:	Remove a definition for the set of definitions.		*
 ************************************************************************/

int RemoveDefinition (definition)
    Definition definition;
{
    if (!problem.definition_tree)
	problem.definition_tree = TreeCreate (DefinitionCompare);

    return TreeDelete (problem.definition_tree,definition) != (Item) definition;
}


/************************************************************************
 * Function:	LookupDefinition					*
 *									*
 * Description:	Look up a definition by name in the set of definitions.	*
 ************************************************************************/

Definition LookupDefinition (name)
    char *name;
{
    struct definition definition;


    if (!problem.definition_tree)
	problem.definition_tree = TreeCreate (DefinitionCompare);

    definition.name = name;
    return (Definition) TreeSearch (problem.definition_tree, &definition);
}
