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
 * File:	coerce.c						*
 *									*
 * Description:	This file contains the function definitions for		*
 *		coercing data from one type to another.			*
 ************************************************************************/

# include <ctype.h>
# include "trap.h"
# include "coerce.h"
# include "execute.h"
# include "allocate.h"
# include "our-stdlib.h"

# define NumStatic 16


static int count;
static char strings [NumStatic] [256];

static union {
    char  *sval;
    double dval;
    int    ival;
    char   bval;
} data [NumStatic];

static descriptor array [NumStatic];


/************************************************************************
 * Function:	strict							*
 *									*
 * Description:	Trapped variable handler for strictly typed array	*
 *		members.						*
 ************************************************************************/

static int strict (dest, src)
    descriptor  *dest;
    descriptor **src;
{
    /* This data is never recycled. */

    if (!src)
	return 0;


    *src = CoerceData (*src, D_Type (dest));

    if (D_Type (dest) != D_Type (*src)) {
	TypeError ("=", dest, *src, NULL, F_False);
	return 1;
    }


    switch (D_Type (dest)) {
    case T_Byte:
	*D_Byte (dest) = *D_Byte (*src);
	break;


    case T_Int:
	*D_Int (dest) = *D_Int (*src);
	break;


    case T_Double:
	*D_Double (dest) = *D_Double (*src);
	break;


    default:
	TypeError ("=", dest, *src, NULL, F_False);
	return 1;
    }


    return 0;
}


/************************************************************************
 * Function:	CoerceData						*
 *									*
 * Description:	Coerces the data of a descriptor from one type to	*
 *		another.  The following types may be coerced:		*
 *									*
 *			double	-> string, integer, byte		*
 *			integer	-> string, double, byte			*
 *			byte	-> string, double, integer		*
 *			string	-> double, integer, byte	  +	*
 *			matrix	-> string, double, integer, byte  +	*
 *			array	-> string, double, integer, byte  +	*
 *									*
 *		A string may be coerced to a numeric type only if the	*
 *		entire string evaluates to a number.  A matrix may be	*
 *		coerced to a scalar type only if it consists of a	*
 *		single element.  An array may be coerced to a scalar if	*
 *		it consists of a single element.  An array is		*
 *		automatically coerced to a matrix by this function if	*
 *		the element type of the array is numeric.		*
 ************************************************************************/

descriptor *CoerceData (d, type)
    descriptor *d;
    int		type;
{
    Matrix	a;
    Array	arr;
    char       *ptr;
    descriptor *coerced;
    unsigned	i;


    count = (count + 1) % NumStatic;


    if (D_Type (d) == T_MatrixPtr) {
	a = *D_MatrixPtr (d);
	d = &array [count];
	D_Type	  (d) = T_Matrix;
	D_Temp	  (d) = F_False;
	D_Trapped (d) = F_False;
	D_Matrix  (d) = a;
    }


    /* Since arrays are only used within the felt code itself, they should
       appear as matrices to the outside world. */

    if (D_Type (d) == T_Array && D_Array (d) -> length) {
	arr = D_Array (d);
	coerced = &array [count];
	count = (count + 1) % NumStatic;

	switch (arr -> type) {
	case T_Int:
	    CreateData (coerced, NULL, NULL, T_Matrix, 1, arr -> length);
	    a = D_Matrix (coerced);
	    ptr = (char *) arr -> ptr + arr -> elt_size;
	    for (i = 1; i <= arr -> length; i ++) {
		sdata (a, 1, i) = *(int *) ptr;
		ptr += arr -> elt_size;
	    }
	    RecycleData (d);
	    d = coerced;
	    break;


	case T_Byte:
	    CreateData (coerced, NULL, NULL, T_Matrix, 1, arr -> length);
	    a = D_Matrix (coerced);
	    ptr = (char *) arr -> ptr + arr -> elt_size;
	    for (i = 1; i <= arr -> length; i ++) {
		sdata (a, 1, i) = *(char *) ptr;
		ptr += arr -> elt_size;
	    }
	    RecycleData (d);
	    d = coerced;
	    break;


	case T_Double:
	    CreateData (coerced, NULL, NULL, T_Matrix, 1, arr -> length);
	    a = D_Matrix (coerced);
	    ptr = (char *) arr -> ptr + arr -> elt_size;
	    for (i = 1; i <= arr -> length; i ++) {
		sdata (a, 1, i) = *(double *) ptr;
		ptr += arr -> elt_size;
	    }
	    RecycleData (d);
	    d = coerced;
	    break;


	default:
	    break;
	}
    }


    switch (D_Type (d)) {


    /* A string can be converted to a double, integer, or byte, if the
       entire string is numeric. */

    case T_String:
	switch (type) {
	case T_Double:
	    data [count].dval = strtod (*D_String (d), &ptr);
	    while (*ptr) if (!isspace (*ptr ++)) return d;
	    RecycleData	(d);

	    d = &array [count];
	    D_Type    (d) = T_Double;
	    D_Temp    (d) = F_False;
	    D_Trapped (d) = F_False;
	    D_Double  (d) = &data [count].dval;
	    break;


	case T_Int:
	    data [count].ival = strtol (*D_String (d), &ptr, 10);
	    while (*ptr) if (!isspace (*ptr ++)) return d;
	    RecycleData	(d);

	    d = &array [count];
	    D_Type    (d) = T_Int;
	    D_Temp    (d) = F_False;
	    D_Trapped (d) = F_False;
	    D_Int     (d) = &data [count].ival;
	    break;


	case T_Byte:
	    data [count].bval = strtol (*D_String (d), &ptr, 10);
	    while (*ptr) if (!isspace (*ptr ++)) return d;
	    RecycleData	(d);

	    d = &array [count];
	    D_Type    (d) = T_Byte;
	    D_Temp    (d) = F_False;
	    D_Trapped (d) = F_False;
	    D_Byte    (d) = &data [count].bval;
	    break;


	default:
	    break;
	}
	break;


    /* A double can be converted to a string, integer, or byte. */

    case T_Double:
	switch (type) {
	case T_String:
	    sprintf (strings [count], "%g", *D_Double (d));
	    data [count].sval = strings [count];
	    RecycleData	(d);

	    d = &array [count];
	    D_Type    (d) = T_String;
	    D_Temp    (d) = F_False;
	    D_Trapped (d) = F_False;
	    D_String  (d) = &data [count].sval;
	    break;


	case T_Int:
	    data [count].ival = *D_Double (d);
	    RecycleData	(d);

	    d = &array [count];
	    D_Type    (d) = T_Int;
	    D_Temp    (d) = F_False;
	    D_Trapped (d) = F_False;
	    D_Int     (d) = &data [count].ival;
	    break;


	case T_Byte:
	    data [count].bval = *D_Double (d);
	    RecycleData	(d);

	    d = &array [count];
	    D_Type    (d) = T_Byte;
	    D_Temp    (d) = F_False;
	    D_Trapped (d) = F_False;
	    D_Byte    (d) = &data [count].bval;
	    break;


	default:
	    break;
	}
	break;


    /* An integer can be converted to a string, double, or byte. */

    case T_Int:
	switch (type) {
	case T_String:
	    sprintf (strings [count], "%d", *D_Int (d));
	    data [count].sval = strings [count];
	    RecycleData	(d);

	    d = &array [count];
	    D_Type    (d) = T_String;
	    D_Temp    (d) = F_False;
	    D_Trapped (d) = F_False;
	    D_String  (d) = &data [count].sval;
	    break;


	case T_Double:
	    data [count].dval = *D_Int (d);
	    RecycleData	(d);

	    d = &array [count];
	    D_Type    (d) = T_Double;
	    D_Temp    (d) = F_False;
	    D_Trapped (d) = F_False;
	    D_Double  (d) = &data [count].dval;
	    break;


	case T_Byte:
	    data [count].bval = *D_Byte (d);
	    RecycleData	(d);

	    d = &array [count];
	    D_Type    (d) = T_Byte;
	    D_Temp    (d) = F_False;
	    D_Trapped (d) = F_False;
	    D_Byte    (d) = &data [count].bval;
	    break;


	default:
	    break;
	}
	break;


    /* A byte can be converted to a string, double, or integer. */

    case T_Byte:
	switch (type) {
	case T_String:
	    sprintf (strings [count], "%d", *D_Byte (d));
	    data [count].sval = strings [count];
	    RecycleData	(d);

	    d = &array [count];
	    D_Type    (d) = T_String;
	    D_Temp    (d) = F_False;
	    D_Trapped (d) = F_False;
	    D_String  (d) = &data [count].sval;
	    break;


	case T_Double:
	    data [count].dval = *D_Byte (d);
	    RecycleData	(d);

	    d = &array [count];
	    D_Type    (d) = T_Double;
	    D_Temp    (d) = F_False;
	    D_Trapped (d) = F_False;
	    D_Double  (d) = &data [count].dval;
	    break;


	case T_Int:
	    data [count].ival = *D_Byte (d);
	    RecycleData	(d);

	    d = &array [count];
	    D_Type    (d) = T_Int;
	    D_Temp    (d) = F_False;
	    D_Trapped (d) = F_False;
	    D_Int     (d) = &data [count].ival;
	    break;


	default:
	    break;
	}
	break;


    /* A matrix can be converted to a string, integer, or byte if it
       consists of a single element. */

    case T_Matrix:
	a = D_Matrix (d);
	if (Mrows (a) == 1 && Mcols (a) == 1)
	    switch (type) {
	    case T_String:
		sprintf (strings [count], "%g", mdata (a, 1, 1));
		data [count].sval = strings [count];
		RecycleData (d);

		d = &array [count];
		D_Type    (d) = T_String;
		D_Temp    (d) = F_False;
		D_Trapped (d) = F_False;
		D_String  (d) = &data [count].sval;
		break;


	    case T_Double:
		data [count].dval = mdata (a, 1, 1);
		RecycleData (d);

		d = &array [count];
		D_Type    (d) = T_Double;
		D_Temp    (d) = F_False;
		D_Trapped (d) = F_False;
		D_Double  (d) = &data [count].dval;
		break;


	    case T_Int:
		data [count].ival = mdata (a, 1, 1);
		RecycleData (d);

		d = &array [count];
		D_Type    (d) = T_Int;
		D_Temp    (d) = F_False;
		D_Trapped (d) = F_False;
		D_Int     (d) = &data [count].ival;
		break;


	    case T_Byte:
		data [count].bval = mdata (a, 1, 1);
		RecycleData (d);

		d = &array [count];
		D_Type    (d) = T_Byte;
		D_Temp    (d) = F_False;
		D_Trapped (d) = F_False;
		D_Byte    (d) = &data [count].bval;
		break;


	    default:
		break;
	    }


    default:
	break;
    }

    return d;
}


/************************************************************************
 * Function:	CoerceToArray						*
 *									*
 * Description:	Coerces the data of a descriptor to an array.		*
 ************************************************************************/

descriptor *CoerceToArray (d, type)
    descriptor *d;
    int		type;
{
    Matrix	a;
    char       *ptr;
    int		handler;
    descriptor *arr;
    double	value;
    unsigned	i;
    unsigned	nc;
    unsigned	nr;


    count = (count + 1) % NumStatic;
    handler = AddTrap (strict);
    arr = &array [count];

    switch (D_Type (d)) {
    case T_Int:
	value = *D_Int (d);

	switch (type) {
	case T_Int:
	    RecycleData (d);
	    ptr = (char *) Allocate (int, 2);
	    CreateData (d, NULL, NULL, T_Array, ptr, type, 1, handler);
	    ((int *) ptr) [1] = value;
	    D_Array (d) -> temp = F_True;
	    break;


	case T_Byte:
	    RecycleData (d);
	    ptr = (char *) Allocate (char, 2);
	    CreateData (d, NULL, NULL, T_Array, ptr, type, 1, handler);
	    ((char *) ptr) [1] = value;
	    D_Array (d) -> temp = F_True;
	    break;


	case T_Double:
	    RecycleData (d);
	    ptr = (char *) Allocate (double, 2);
	    CreateData (d, NULL, NULL, T_Array, ptr, type, 1, handler);
	    ((double *) ptr) [1] = value;
	    D_Array (d) -> temp = F_True;
	    break;


	default:
	    break;
	}
	break;


    case T_Byte:
	value = *D_Byte (d);

	switch (type) {
	case T_Int:
	    RecycleData (d);
	    ptr = (char *) Allocate (int, 2);
	    CreateData (d, NULL, NULL, T_Array, ptr, type, 1, handler);
	    ((int *) ptr) [1] = value;
	    D_Array (d) -> temp = F_True;
	    break;


	case T_Byte:
	    RecycleData (d);
	    ptr = (char *) Allocate (char, 2);
	    CreateData (d, NULL, NULL, T_Array, ptr, type, 1, handler);
	    ((char *) ptr) [1] = value;
	    D_Array (d) -> temp = F_True;
	    break;


	case T_Double:
	    RecycleData (d);
	    ptr = (char *) Allocate (double, 2);
	    CreateData (d, NULL, NULL, T_Array, ptr, type, 1, handler);
	    ((double *) ptr) [1] = value;
	    D_Array (d) -> temp = F_True;
	    break;


	default:
	    break;
	}
	break;


    case T_Double:
	value = *D_Double (d);

	switch (type) {
	case T_Int:
	    RecycleData (d);
	    ptr = (char *) Allocate (int, 2);
	    CreateData (d, NULL, NULL, T_Array, ptr, type, 1, handler);
	    ((int *) ptr) [1] = value;
	    D_Array (d) -> temp = F_True;
	    break;


	case T_Byte:
	    RecycleData (d);
	    ptr = (char *) Allocate (char, 2);
	    CreateData (d, NULL, NULL, T_Array, ptr, type, 1, handler);
	    ((char *) ptr) [1] = value;
	    D_Array (d) -> temp = F_True;
	    break;


	case T_Double:
	    RecycleData (d);
	    ptr = (char *) Allocate (double, 2);
	    CreateData (d, NULL, NULL, T_Array, ptr, type, 1, handler);
	    ((double *) ptr) [1] = value;
	    D_Array (d) -> temp = F_True;
	    break;


	default:
	    break;
	}
	break;


    case T_String:
	value = strtod (*D_String (d), &ptr);
	while (*ptr) if (!isspace (*ptr ++)) return d;

	switch (type) {
	case T_Int:
	    RecycleData (d);
	    ptr = (char *) Allocate (int, 2);
	    CreateData (d, NULL, NULL, T_Array, ptr, type, 1, handler);
	    ((int *) ptr) [1] = value;
	    D_Array (d) -> temp = F_True;
	    break;


	case T_Byte:
	    RecycleData (d);
	    ptr = (char *) Allocate (char, 2);
	    CreateData (d, NULL, NULL, T_Array, ptr, type, 1, handler);
	    ((char *) ptr) [1] = value;
	    D_Array (d) -> temp = F_True;
	    break;


	case T_Double:
	    RecycleData (d);
	    ptr = (char *) Allocate (double, 2);
	    CreateData (d, NULL, NULL, T_Array, ptr, type, 1, handler);
	    ((double *) ptr) [1] = value;
	    D_Array (d) -> temp = F_True;
	    break;


	default:
	    break;
	}
	break;


    case T_Matrix:
	a = D_Matrix (d);

	if (Mrows (a) == 1) {
	    nc = Mcols (a);
	    switch (type) {
	    case T_Int:
		ptr = (char *) Allocate (int, nc + 1);
		CreateData (arr, NULL, NULL, T_Array, ptr, type, nc, handler);
		for (i = 1; i <= nc; i ++)
		    ((int *) ptr) [i] = mdata (a, 1, i);
		D_Array (arr) -> temp = F_True;
		RecycleData (d);
		d = arr;
		break;


	    case T_Byte:
		ptr = (char *) Allocate (char, nc + 1);
		CreateData (arr, NULL, NULL, T_Array, ptr, type, nc, handler);
		for (i = 1; i <= nc; i ++)
		    ((char *) ptr) [i] = mdata (a, 1, i);
		D_Array (arr) -> temp = F_True;
		RecycleData (d);
		d = arr;
		break;


	    case T_Double:
		ptr = (char *) Allocate (double, nc + 1);
		CreateData (arr, NULL, NULL, T_Array, ptr, type, nc, handler);
		for (i = 1; i <= nc; i ++)
		    ((double *) ptr) [i] = mdata (a, 1, i);
		D_Array (arr) -> temp = F_True;
		RecycleData (d);
		d = arr;
		break;


	    default:
		break;
	    }
	} else if (Mcols (a) == 1) {
	    nr = Mrows (a);
	    switch (type) {
	    case T_Int:
		ptr = (char *) Allocate (int, nr + 1);
		CreateData (arr, NULL, NULL, T_Array, ptr, type, nr, handler);
		for (i = 1; i <= nr; i ++)
		    ((int *) ptr) [i] = mdata (a, i, 1);
		D_Array (arr) -> temp = F_True;
		RecycleData (d);
		d = arr;
		break;


	    case T_Byte:
		ptr = (char *) Allocate (char, nr + 1);
		CreateData (arr, NULL, NULL, T_Array, ptr, type, nr, handler);
		for (i = 1; i <= nr; i ++)
		    ((char *) ptr) [i] = mdata (a, i, 1);
		D_Array (arr) -> temp = F_True;
		RecycleData (d);
		d = arr;
		break;


	    case T_Double:
		ptr = (char *) Allocate (double, nr + 1);
		CreateData (arr, NULL, NULL, T_Array, ptr, type, nr, handler);
		for (i = 1; i <= nr; i ++)
		    ((double *) ptr) [i] = mdata (a, i, 1);
		D_Array (arr) -> temp = F_True;
		RecycleData (d);
		d = arr;
		break;


	    default:
		break;
	    }
	}
	break;


    default:
	break;
    }

    return d;
}
