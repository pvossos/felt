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
 * File:	field.c							*
 *									*
 * Description:	This file contains the public and private function	*
 *		definitions relating to record fields and the		*
 *		corresponding virtual machine instruction.		*
 ************************************************************************/

# include "debug.h"
# include "error.h"
# include "field.h"
# include "execute.h"

# define MaxFields 160


typedef struct field {
    short type;
    short handler;
    short offset;
} *Field;

static struct field field_table [D_NumTypes] [MaxFields];


/************************************************************************
 * Function:	add_field						*
 *									*
 * Description:	Adds a field to the field table.  The table is a two-	*
 *		dimensional array indexed by type and field number.	*
 ************************************************************************/

void add_field (rtype, name, ftype, offset, handler)
    int		rtype;
    char       *name;
    int		ftype;
    unsigned	offset;
    TrapHandler	handler;
{
    ste  *s;
    Field f;


    s = st_insert (&field_st, name, FieldOp);

    if (s -> idx >= MaxFields) {
	rterror ("too may field entries");
	exit (1);
    }


    f = &field_table [rtype] [s -> idx];
    f -> type	 = ftype;
    f -> offset	 = offset;
    f -> handler = AddTrap (handler);
}


/************************************************************************
 * Function:	field_op						*
 *									*
 * Description:	Pops the descriptor on the top of the stack, looks up	*
 *		the field whose given at the next address, and places	*
 *		the result on the stack.  If the field type is an array	*
 *		then the appropriate array constructor is called.  If	*
 *		the field is a pair then the offset is added to the	*
 *		current pointer.  Otherwise, the current pointer is	*
 *		first dereferenced and then the offset is added.	*
 ************************************************************************/

int field_op ( )
{
    void       *ptr;
    descriptor *record;
    descriptor *result;
    descriptor	temp;
    int		type_error;
    int		index;
    Field	f;


    result = top ( );
    temp = *result;
    record = &temp;

    index = fetch (pc ++).ival;
    record = deref (record);


    if ((f = &field_table [D_Type (record)] [index]) -> type != T_Null) {
	type_error = F_False;
	ptr = D_Pointer (record);

	if (ptr && (D_Direct (record) || *(char **) ptr)) {

	    switch (D_Type (record)) {
	    case T_Analysis:
	    case T_Pair:
	    case T_Problem:
		ptr = (void *) ((char *) D_Pointer (record) + f -> offset);
		break;

	    default:
		ptr = (void *) (*(char **) D_Pointer (record) + f -> offset);
		break;
	    }

	    if (f -> type != T_Array) {
		D_Type    (result) = f -> type;
		D_Temp    (result) = F_False;
		D_Trapped (result) = f -> handler;
		D_Pointer (result) = ptr;
	    } else {
		CreateData (result, NULL, NULL, T_Array, ptr, 0, T_Null, 0);
		CallTrap (f -> handler, record, &result);
	    }

	} else {
	    D_Type    (result) = T_Null;
	    D_Temp    (result) = F_False;
	    D_Trapped (result) = F_False;
	    D_Pointer (result) = NULL;
	}

    } else {
	type_error = F_True;
	TypeError ("has no such field", record, NULL, NULL, F_False);
    }


    RecycleData (record);
    d_printf ("field ans =\n");
    d_PrintData (result);

    return type_error == F_True;
}
