/*
    This file is part of the FElt finite element analysis package.
    Copyright (C) 1993-1997 Jason I. Gobat and Darren C. Atkinson

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
 * File:	desctab.h						*
 *									*
 * Description:	This file contains the table of type names of each	*
 *		descriptor type.  Note that this table must be kept in	*
 *		correspondence with the types listed in descriptor.h!	*
 ************************************************************************/

# ifndef _DESCTAB_H
# define _DESCTAB_H

char *type_names [ ] = {
    "null",			/* T_Null	*/
    "variable",			/* T_Variable	*/
    "function",			/* T_Function	*/
    "function",			/* T_Intrinsic	*/
    "string",			/* T_String	*/
    "scalar",			/* T_Double	*/
    "scalar",			/* T_Int	*/
    "scalar",			/* T_Byte	*/
    "array",			/* T_Array	*/
    "row",			/* T_Row	*/
    "matrix",			/* T_Matrix	*/
    "matrix_ptr",		/* T_MatrixPtr	*/
    "analysis parameters",	/* T_Analysis	*/
    "constraint",		/* T_Constraint	*/
    "definition",		/* T_Definition	*/
    "element",			/* T_Element	*/
    "force",			/* T_Force	*/
    "load",			/* T_Load	*/
    "material",			/* T_Material	*/
    "node",			/* T_Node	*/
    "pair",			/* T_Pair	*/
    "problem definition",	/* T_Problem	*/
    "stress",			/* T_Stress	*/
    "external C function",	/* T_External	*/
};

# endif /* _DESCTAB_H */
