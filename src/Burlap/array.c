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
 * File:	array.c							*
 *									*
 * Description:	This file contains the function definitions for array	*
 *		blocks.							*
 ************************************************************************/

# include "allocate.h"
# include "descriptor.h"


/************************************************************************
 * Function:	CreateArray						*
 *									*
 * Description:	Creates, initializes, and returns a new array block.	*
 *		The size of an element is determined from the type.  By	*
 *		default the array is not temporary.			*
 ************************************************************************/

Array CreateArray (void *ptr, int type, int length, int handler)
{
    Array array;


    array = New (struct array);

    array -> ptr	= ptr;
    array -> temp	= F_False;
    array -> type	= type;
    array -> length	= length;
    array -> handler	= handler;
    array -> ref_count	= 1;


    switch (type) {
    case T_String:
	array -> elt_size = sizeof (char *);
	break;


    case T_Double:
	array -> elt_size = sizeof (double);
	break;


    case T_Int:
	array -> elt_size = sizeof (int);
	break;


    case T_Byte:
	array -> elt_size = sizeof (char);
	break;


    case T_Matrix:
	array -> elt_size = sizeof (Matrix);
	break;


    case T_MatrixPtr:
    case T_Constraint:
    case T_Definition:
    case T_Element:
    case T_Force:
    case T_Load:
    case T_Material:
    case T_Node:
	array -> elt_size = sizeof (void *);
	break;


    case T_Pair:
	array -> elt_size = sizeof (struct {unsigned u; double m;});
	break;


    default:
	array -> elt_size = 0;
	break;
    }


    return array;
}


/************************************************************************
 * Function:	CopyArray						*
 *									*
 * Description:	Creates a copy of an array.				*
 ************************************************************************/

Array CopyArray (Array array)
{
    array -> ref_count ++;
    return array;
}


/************************************************************************
 * Function:	DestroyArray						*
 *									*
 * Description:	Destroys an array block.  The array itself is not	*
 *		deallocated unless it is temporary.			*
 ************************************************************************/

void DestroyArray (Array array)
{
    if (! -- array -> ref_count) {
	if (array -> temp)
	    Deallocate (array -> ptr);

	Delete (array);
    }
}
