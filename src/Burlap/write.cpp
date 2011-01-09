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
 * File:	write.c							*
 *									*
 * Description:	This file contains the function definitions for the	*
 *		write and writes intrinsic functions.			*
 ************************************************************************/

# include <ctype.h>
# include "felt.h"
# include "write.h"
# include "coerce.h"
# include "execute.h"
# include "functab.h"
# include "writetab.h"


/************************************************************************
 * Function:	write_null						*
 *									*
 * Description:	Writes a null descriptor.				*
 ************************************************************************/

static void write_null (descriptor *d)
{
    printf ("null");
}


/************************************************************************
 * Function:	write_variable						*
 *									*
 * Description:	Writes a variable descriptor.				*
 ************************************************************************/

static void write_variable (descriptor *d)
{
    d = deref (d);
    writetab [D_Type (d)] (d);
}


/************************************************************************
 * Function:	write_function						*
 *									*
 * Description:	Writes a function descriptor.				*
 ************************************************************************/

static void write_function (descriptor *d)
{
    int      i;
    Function func;


    func = D_Function (d);
    printf ("%s(", func -> name);

    for (i = 0; i < func -> num_args; i ++)
	printf ("%s%c", i ? "," : "", 'a' + i);

    printf (")");
}


/************************************************************************
 * Function:	write_intrinsic						*
 *									*
 * Description:	Writes an intrinsic function descriptor.		*
 ************************************************************************/

static void write_intrinsic (descriptor *d)
{
    int i;
    int nargs;
    int index;


    index = D_Intrinsic (d);
    nargs = functab [index].num_args;

    printf ("%s(", functab [index].name);

    if (nargs < 0)
	printf ("...");
    else
	for (i = 0; i < nargs; i ++)
	    printf ("%s%c", i ? "," : "", 'a' + i);

    printf (")");
}


/************************************************************************
 * Function:	write_string						*
 *									*
 * Description:	Writes a string descriptor.				*
 ************************************************************************/

static void write_string (descriptor *d)
{
    printf ("%s", *D_String (d) ? *D_String (d) : "(null)");
}


/************************************************************************
 * Function:	write_double						*
 *									*
 * Description:	Writes a double descriptor.				*
 ************************************************************************/

static void write_double (descriptor *d)
{
    printf ("%g", *D_Double (d));
}


/************************************************************************
 * Function:	write_int						*
 *									*
 * Description:	Writes an integer descriptor.				*
 ************************************************************************/

static void write_int (descriptor *d)
{
    printf ("%d", *D_Int (d));
}


/************************************************************************
 * Function:	write_byte						*
 *									*
 * Description:	Writes a byte descriptor.				*
 ************************************************************************/

static void write_byte (descriptor *d)
{
    printf (iscntrl (*D_Byte (d)) ? "%d" : "%c", *D_Byte (d));
}


/************************************************************************
 * Function:	write_array						*
 *									*
 * Description:	Writes an array descriptor.				*
 ************************************************************************/

static void write_array (descriptor *d)
{
    printf ("array of %s", type_names [D_Array (d) -> type]);
}


/************************************************************************
 * Function:	write_row						*
 *									*
 * Description:	Writes a row descriptor.				*
 ************************************************************************/

static void write_row (descriptor *d)
{
    printf ("row");
}


/************************************************************************
 * Function:	write_matrix						*
 *									*
 * Description:	Writes a matrix descriptor.				*
 ************************************************************************/

static void write_matrix (descriptor *d)
{
    PrintMatrix (D_Matrix (d), stdout);
}


/************************************************************************
 * Function:	write_matrix_ptr					*
 *									*
 * Description:	Writes a matrix_ptr descriptor.				*
 ************************************************************************/

static void write_matrix_ptr (descriptor *d)
{
    printf ("matrixptr");
}


/************************************************************************
 * Function:	write_analysis						*
 *									*
 * Description:	Writes an analysis descriptor.				*
 ************************************************************************/

static void write_analysis (descriptor *d)
{
    printf ("analysis parameters");
}


/************************************************************************
 * Function:	write_constraint					*
 *									*
 * Description:	Writes a constraint descriptor.				*
 ************************************************************************/

static void write_constraint (descriptor *d)
{
    Constraint c;


    c = *D_Constraint (d);
    printf ("constraint (%s)", c ? c -> name : "null");
}


/************************************************************************
 * Function:	write_definition					*
 *									*
 * Description:	Writes a definition descriptor.				*
 ************************************************************************/

static void write_definition (descriptor *d)
{
    Definition def;


    def = *D_Definition (d);
    printf ("definition (%s)", def ? def -> name : "null");
}


/************************************************************************
 * Function:	write_element						*
 *									*
 * Description:	Writes an element descriptor.				*
 ************************************************************************/

static void write_element (descriptor *d)
{
    Element e;


    e = *D_Element (d);
    printf (e ? "element (%u)" : "element (null)", e ? e -> number : 0);
}


/************************************************************************
 * Function:	write_force						*
 *									*
 * Description:	Writes a force descriptor.				*
 ************************************************************************/

static void write_force (descriptor *d)
{
    Force f;


    f = *D_Force (d);
    printf ("force (%s)", f ? f -> name : "null");
}


/************************************************************************
 * Function:	write_load						*
 *									*
 * Description:	Writes a load descriptor.				*
 ************************************************************************/

static void write_load (descriptor *d)
{
    Distributed l;


    l = *D_Load (d);
    printf ("load (%s)", l ? l -> name : "null");
}


/************************************************************************
 * Function:	write_material						*
 *									*
 * Description:	Writes a material descriptor.				*
 ************************************************************************/

static void write_material (descriptor *d)
{
    Material m;


    m = *D_Material (d);
    printf ("material (%s)", m ? m -> name : "null");
}


/************************************************************************
 * Function:	write_node						*
 *									*
 * Description:	Writes a node descriptor.				*
 ************************************************************************/

static void write_node (descriptor *d)
{
    Node n;


    n = *D_Node (d);
    printf (n ? "node (%u)" : "node (null)", n ? n -> number : 0);
}


/************************************************************************
 * Function:	write_pair						*
 *									*
 * Description:	Writes a pair descriptor.				*
 ************************************************************************/

static void write_pair (descriptor *d)
{
    Pair *p;


    p = D_Pair (d);
    printf ("pair (%u,%g)", p -> node, p -> magnitude);
}


/************************************************************************
 * Function:	write_problem						*
 *									*
 * Description:	Writes a problem descriptor.				*
 ************************************************************************/

static void write_problem (descriptor *d)
{
    printf ("problem definition");
}


/************************************************************************
 * Function:	write_stress						*
 *									*
 * Description:	Writes a stress descriptor.				*
 ************************************************************************/

static void write_stress (descriptor *d)
{
    printf ("stress");
}


/************************************************************************
 * Function:	write_external						*
 *									*
 * Description:	Writes an external descriptor.				*
 ************************************************************************/

static void write_external (descriptor *d)
{
    printf ("external C function (%p)", *D_External (d));
}


/************************************************************************
 * Function:	write_func						*
 *									*
 * Description:	Writes a list of descriptors followed by a new line.	*
 *		The result of the function is always zero.		*
 ************************************************************************/

int write_func (int n)
{
    int		i;
    descriptor *d;


    d = NULL;

    for (i = n - 1; i >= 0; i --) {
	d = ntop (i);
	d = deref (d);
	d = CollapseArray (d);

	writetab [D_Type (d)] (d);
	RecycleData (d);
    }

    if (d && D_Type (d) != T_Matrix)
	printf ("\n");

    for (i = 0; i < n; i ++)
	pop ( );

    d = push ( );
    D_Type    (d) = T_Double;
    D_Temp    (d) = F_False;
    D_Trapped (d) = F_False;
    D_Double  (d) = dbllit (0);
    return 0;
}


/************************************************************************
 * Function:	writes_func						*
 *									*
 * Description:	Writes a list of descriptors.  The result of the	*
 *		function is always zero.				*
 ************************************************************************/

int writes_func (int n)
{
    int		i;
    descriptor *d;


    d = NULL;

    for (i = n - 1; i >= 0; i --) {
	d = ntop (i);
	d = deref (d);
	d = CollapseArray (d);

	writetab [D_Type (d)] (d);
	RecycleData (d);
    }

    for (i = 0; i < n; i ++)
	pop ( );

    d = push ( );
    D_Type    (d) = T_Double;
    D_Temp    (d) = F_False;
    D_Trapped (d) = F_False;
    D_Double  (d) = dbllit (0);
    return 0;
}
