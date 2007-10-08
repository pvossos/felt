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
 * File:	globals.c						*
 *									*
 * Description:	This file contains the function and variable		*
 *		definitions for the global symbols.			*
 ************************************************************************/

# include <string.h>
# include <stdlib.h>
# include "trap.h"
# include "execute.h"
# include "functab.h"
# include "globals.h"
# include "allocate.h"
# include "consttab.h"

/* Symbol tables for global symbols. */

st var_st;
st str_st;
st dbl_st;
st field_st;
st import_st;


/* Current sizes of global symbol arrays. */

unsigned var_array_size;
unsigned str_array_size;
unsigned dbl_array_size;


/* Global symbol arrays. */

descriptor *var_array;
char	  **str_array;
double	   *dbl_array;


/************************************************************************
 * Function:	constant_handler					*
 *									*
 * Description:	Trapped variable handler for enumeration constants.	*
 ************************************************************************/

static int constant_handler (descriptor *dest, descriptor **src)
{
    /* This data is never recycled. */

    if (!src)
	return 0;


    /* This data is read-only. */

    TypeError ("cannot assign to constant", NULL, NULL, NULL, F_False);
    return 1;
}


/************************************************************************
 * Function:	add_literal						*
 *									*
 * Description:	Adds a literal to the given symbol table and stores the	*
 *		value in the global symbol arrays.  The arrays are	*
 *		reallocated if necessary (this is ok because the	*
 *		instructions contain an index into these arrays, not an	*
 *		address).						*
 ************************************************************************/

ste *add_literal (st *table, char *name, Opcode op)
{
    ste *s;


    if (!(s = st_lookup (table, name)))
	if (table != &str_st && table != &dbl_st)
	    s = st_lookup (&import_st, name);

    if (!s) {
	s = st_insert (table, name, op);

	if (table == &var_st) {
	    if (s -> idx >= var_array_size) {
		var_array_size = var_array_size ? var_array_size << 1 : 16;
		Reallocate (var_array, descriptor, var_array_size);
	    }
	    CreateData (&var_array [s -> idx], NULL, NULL, T_Null);
	    s -> op = GlblOp;

	} else if (table == &str_st) {
	    if (s -> idx >= str_array_size) {
		str_array_size = str_array_size ? str_array_size << 1 : 16;
		Reallocate (str_array, char *, str_array_size);
	    }
	    str_array [s -> idx] = Strdup (name);

	} else if (table == &dbl_st) {
	    if (s -> idx >= dbl_array_size) {
		dbl_array_size = dbl_array_size ? dbl_array_size << 1 : 16;
		Reallocate (dbl_array, double, dbl_array_size);
	    }
	    dbl_array [s -> idx] = strtod (name, NULL);
	}
    }

    return s;
}


/************************************************************************
 * Function:	global_init						*
 *									*
 * Description:	Initializes the global symbol tables.			*
 ************************************************************************/

void global_init (void)
{
    int		i;
    char	buffer [32];
    ste        *s;
    descriptor *d;


    st_init (&var_st);
    st_init (&str_st);
    st_init (&dbl_st);
    st_init (&field_st);
    st_init (&import_st);

    add_literal (&dbl_st, "0", DblOp);
    add_literal (&dbl_st, "1", DblOp);
    add_literal (&field_st, "", FieldOp);

    for (i = 0; i < NumIntrinsics; i ++) {
	d = &var_array [add_literal (&var_st, functab [i].name, GlblOp) -> idx];
	D_Type	    (d) = T_Intrinsic;
	D_Temp	    (d) = F_False;
	D_Trapped   (d) = F_False;
	D_Intrinsic (d) = i;
    }

    for (i = 0; i < NumConstants; i ++) {
	d = &var_array [add_literal (&var_st, consttab [i].name, GlblOp) -> idx];
	sprintf (buffer, "%g", consttab [i].value),
	s = add_literal (&dbl_st, buffer, DblOp);
	D_Type	  (d) = T_Double;
	D_Temp	  (d) = F_False;
	D_Trapped (d) = AddTrap (constant_handler);
	D_Double  (d) = &dbl_array [s -> idx];
    }

    d = &var_array [add_literal (&var_st, "&null", GlblOp) -> idx];
    D_Type    (d) = T_Null;
    D_Temp    (d) = F_False;
    D_Trapped (d) = AddTrap (constant_handler);
    D_Pointer (d) = NULL;
}


/************************************************************************
 * Function:	is_global						*
 *									*
 * Description:	Returns whether the specified variable is a global	*
 *		variable.						*
 ************************************************************************/

int is_global (descriptor *var)
{
    return var_array <= var && var < var_array + var_st.num_syms;
}
