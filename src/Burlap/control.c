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
 * File:	control.c						*
 *									*
 * Description:	This file contains the function definitions for the	*
 *		control flow related virtual machine instructions.	*
 ************************************************************************/

# include "debug.h"
# include "coerce.h"
# include "execute.h"
# include "control.h"


/************************************************************************
 * Function:	fail_op							*
 *									*
 * Description:	Pops and tests the descriptor on the top of the stack.	*
 *		If the descriptor is zero then a null descriptor is	*
 *		pushed on the stack and control is transferred to the	*
 *		specified address.					*
 ************************************************************************/

int fail_op ( )
{
    Address	offset;
    descriptor *d;


    d = pop ( );
    d = CoerceData (deref (d), T_Double);
    offset = fetch (pc ++).addr;


    if (D_Type (d) != T_Double) {
	TypeError ("in conditional context", d, NULL, NULL, F_False);
	RecycleData (d);
	return 1;
    }

    if (*D_Double (d) == 0) {
	RecycleData (d);
	pc += offset;
	d = push ( );
	D_Type    (d) = T_Null;
	D_Temp    (d) = F_False;
	D_Trapped (d) = F_False;
	D_Pointer (d) = NULL;
    } else
	RecycleData (d);

    return 0;
}


/************************************************************************
 * Function:	gen_op							*
 *									*
 * Description:	Generates the next result in a sequence by examining	*
 *		the three descriptors on the top of the stack.  The	*
 *		following generation sequences are possible, where a	*
 *		vector is a matrix with a single row or column:		*
 *									*
 *		scalar -> scalar  (identity operation)			*
 *		vector -> scalar  (next scalar in vector)		*
 *		matrix -> vector  (next column vector in matrix)	*
 *		array  -> unknown (next element in array)		*
 *									*
 *		An attempt is first made to coerce the sequence to a	*
 *		double value.  If all values have been generated then	*
 *		the generator is popped from the stack and a null	*
 *		descriptor is pushed on the stack as the result.	*
 ************************************************************************/

int gen_op ( )
{
    Matrix	a;
    Matrix	b;
    void       *ptr;
    descriptor *d;
    descriptor *v;
    descriptor *var;
    descriptor *index;
    descriptor *vector;
    descriptor	temp;
    double	value;
    Address	increment;
    Array	arr;
    int		fail;
    unsigned	offset;
    unsigned	i;
    unsigned	c;
    unsigned	r;


    index = ntop (0);
    vector = ntop (1);
    var = ntop (2);

    offset = fetch (pc ++).ival;


    if (D_Type (index) == T_Double) {
	if (!assignable (var)) {
	    TypeError ("cannot assign to", NULL, var, NULL, F_False);
	    return 1;
	}

	d = &temp;
	D_Type    (d) = T_Null;
	D_Temp    (d) = F_False;
	D_Trapped (d) = F_False;
	D_Pointer (d) = NULL;

	v = CoerceData (vector, T_Double);
	AssignData (d, &v);
	RecycleData (v);
	D_Temp (d) = F_False;

	d_printf ("d = %s %p\n", D_TypeName (d), D_Pointer (d));

	switch (D_Type (d)) {
	case T_Double:
	case T_Matrix:
	case T_Array:
	case T_Null:
	    break;


	default:
	    TypeError ("cannot index", NULL, d, NULL, F_False);
	    return 1;
	}

	*vector = *d;

	D_Type (index) = T_Row;
	D_Row (index) = 0;
    }

    d_printf ("vector = %s %p\n", D_TypeName (vector), D_Pointer (vector));
    var = deref (var);
    fail = F_False;


    switch (D_Type (vector)) {
    case T_Double:
	if (D_Row (index) ++ == 0)
	    AssignData (var, &vector);
	else
	    fail = F_True;
	break;


    case T_Matrix:
	a = D_Matrix (vector);
	d = &temp;
	D_Temp	  (d) = F_False;
	D_Trapped (d) = F_False;

	if (Mrows (a) == 1) {
	    if (++ D_Row (index) <= Mcols (a)) {
		D_Type   (d) = T_Double;
		D_Double (d) = &value;
		value = mdata (a, 1, D_Row (index));
		AssignData (var, &d);
	    } else
		fail = F_True;

	} else if (Mcols (a) == 1) {
	    if (++ D_Row (index) <= Mrows (a)) {
		D_Type   (d) = T_Double;
		D_Double (d) = &value;
		value = mdata (a, D_Row (index), 1);
		AssignData (var, &d);
	    } else
		fail = F_True;

	} else {
	    if (++ D_Row (index) <= Mcols (a)) {
		d_printf ("indexing matrix\n");
		r = Mrows (a);
		c = D_Row (index);

		FreeData (var);
		CreateData (var, NULL, NULL, T_Matrix, r, 1);
		D_Temp (var) = F_False;
		b = D_Matrix (var);

		for (i = 1; i <= r; i ++)
		    sdata (b, i, 1) = mdata (a, i, c);
	    } else
		fail = F_True;
	}
	break;


    case T_Array:
	arr = D_Array (vector);
	d = &temp;

	if (++ D_Row (index) <= arr -> length) {
	    increment = D_Row (index) * arr -> elt_size;
	    ptr = (void *) ((char *) arr -> ptr + increment);

	    D_Type    (d) = arr -> type;
	    D_Temp    (d) = F_False;
	    D_Trapped (d) = F_False;
	    D_Pointer (d) = ptr;
	    AssignData (var, &d);
	} else
	    fail = F_True;
	break;


    case T_Null:
	fail = F_True;
	break;
    }


    /* After assignment the variable is certainly not temporary.  Its trapped
       status remains as before: if it was trapped then AssignData() called
       the trap handler which didn't change the status.  If it wasn't then
       AssignData() left the status alone. */

    D_Temp (var) = F_False;

    if (fail == F_True) {
	pop ( );
	FreeData (pop ( ));		/* free the privately owned vector */
	pop ( );

	d = push ( );
	D_Type	  (d) = T_Null;
	D_Temp	  (d) = F_False;
	D_Trapped (d) = F_False;
	D_Pointer (d) = NULL;

	pc += offset;
	d_printf ("failing\n");
    }

    return 0;
}


/************************************************************************
 * Function:	halt_op							*
 *									*
 * Description:	Halts execution.					*
 ************************************************************************/

int halt_op ( )
{
    return -1;
}


/************************************************************************
 * Function:	jmp_op							*
 *									*
 * Description:	Transfers control to the specified address.		*
 ************************************************************************/

int jmp_op ( )
{
    Address offset;


    offset = fetch (pc ++).addr;
    pc += offset;
    return 0;
}


/************************************************************************
 * Function:	jnz_op							*
 *									*
 * Descriptor:	Pops and tests the descriptor on the top of the stack.	*
 *		If the descriptor is nonzero then control is		*
 *		transferred to the specified address.			*
 ************************************************************************/

int jnz_op ( )
{
    Address	offset;
    descriptor *d;


    d = pop ( );
    d = CoerceData (deref (d), T_Double);
    offset = fetch (pc ++).addr;


    if (D_Type (d) != T_Double) {
	TypeError ("in conditional context", d, NULL, NULL, F_False);
	RecycleData (d);
	return 1;
    }

    if (*D_Double (d) != 0)
	pc += offset;

    RecycleData (d);
    return 0;
}


/************************************************************************
 * Function:	jz_op							*
 *									*
 * Description:	Pops and tests the descriptor on the top of the stack.	*
 *		If the descriptor is zero then control is transferred	*
 *		to the specified address.				*
 ************************************************************************/

int jz_op ( )
{
    Address	offset;
    descriptor *d;


    d = pop ( );
    d = CoerceData (deref (d), T_Double);
    offset = fetch (pc ++).addr;


    if (D_Type (d) != T_Double) {
	TypeError ("in conditional context", d, NULL, NULL, F_False);
	RecycleData (d);
	return 1;
    }

    if (*D_Double (d) == 0)
	pc += offset;

    RecycleData (d);
    return 0;
}
