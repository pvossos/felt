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
 * File:	function.c						*
 *									*
 * Description:	This file contains the function definitions for		*
 *		function blocks.					*
 ************************************************************************/

# include "function.h"
# include "allocate.h"


/************************************************************************
 * Function:	CreateFunction						*
 *									*
 * Description:	Creates, initializes, and returns a new function block.	*
 *		The function name is *not* copied.			*
 ************************************************************************/

Function CreateFunction (name)
    char *name;
{
    Function func;


    func = New (struct function);
    func -> name = name;
    func -> cs = new_cs ( );
    func -> num_args = 0;
    func -> num_locals = 0;
    func -> ref_count = 1;
    func -> arg_types = NULL;
    func -> local_names = NULL;

    return func;
}


/************************************************************************
 * Function:	CopyFunction						*
 *									*
 * Description:	Creates a copy of a function.				*
 ************************************************************************/

Function CopyFunction (func)
    Function func;
{
    func -> ref_count ++;
    return func;
}


/************************************************************************
 * Function:	DestroyFunction						*
 *									*
 * Description:	Destroys a function block.  The function name is *not*	*
 *		deallocated.						*
 ************************************************************************/

void DestroyFunction (func)
    Function func;
{
    int i;


    if (! -- func -> ref_count) {
	for (i = 0; i < func -> num_locals; i ++)
	    Deallocate (func -> local_names [i]);
	Deallocate (func -> local_names);
	free_cs (func -> cs);
	Deallocate (func -> arg_types);
	Delete (func);
    }
}
