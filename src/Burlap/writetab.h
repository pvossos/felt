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
 * File:	writetab.h						*
 *									*
 * Description:	This file contains the table of write functions of each	*
 *		descriptor type.  Note that this table must be kept in	*
 *		correspondence with the types listed in descriptor.h!	*
 ************************************************************************/

# ifndef _WRITETAB_H
# define _WRITETAB_H
# include "descriptor.h"		/* descriptor type definition */

static void write_null	     (descriptor *);
static void write_variable   (descriptor *);
static void write_function   (descriptor *);
static void write_intrinsic  (descriptor *);
static void write_string     (descriptor *);
static void write_double     (descriptor *);
static void write_int	     (descriptor *);
static void write_byte	     (descriptor *);
static void write_array	     (descriptor *);
static void write_row	     (descriptor *);
static void write_matrix     (descriptor *);
static void write_matrix_ptr (descriptor *);
static void write_analysis   (descriptor *);
static void write_constraint (descriptor *);
static void write_definition (descriptor *);
static void write_element    (descriptor *);
static void write_force	     (descriptor *);
static void write_load	     (descriptor *);
static void write_material   (descriptor *);
static void write_node	     (descriptor *);
static void write_pair	     (descriptor *);
static void write_problem    (descriptor *);
static void write_stress     (descriptor *);
static void write_external   (descriptor *);

static void (*(writetab [ ])) (descriptor *) = {
    write_null,		/* T_Null	*/
    write_variable,	/* T_Variable	*/
    write_function,	/* T_Function	*/
    write_intrinsic,	/* T_Intrinsic	*/
    write_string,	/* T_String	*/
    write_double,	/* T_Double	*/
    write_int,		/* T_Int	*/
    write_byte,		/* T_Byte	*/
    write_array,	/* T_Array	*/
    write_row,		/* T_Row	*/
    write_matrix,	/* T_Matrix	*/
    write_matrix_ptr,	/* T_MatrixPtr	*/
    write_analysis,	/* T_Analysis	*/
    write_constraint,	/* T_Constraint	*/
    write_definition,	/* T_Definition	*/
    write_element,	/* T_Element	*/
    write_force,	/* T_Force	*/
    write_load,		/* T_Load	*/
    write_material,	/* T_Material	*/
    write_node,		/* T_Node	*/
    write_pair,		/* T_Pair	*/
    write_problem,	/* T_Problem	*/
    write_stress,	/* T_Stress	*/
    write_external,	/* T_External	*/
};

# endif /* _WRITETAB_H */
