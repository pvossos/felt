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
 * File:	descriptor.c						*
 *									*
 * Description:	This file contains the public and private function	*
 *		definitions for descriptors.				*
 ************************************************************************/

# include "trap.h"
# include "debug.h"
# include "desctab.h"
# include "allocate.h"
# include "descriptor.h"
# include VAR_ARGS_INCLUDE

static descriptor *reused;


/************************************************************************
 * Function:	CreateData						*
 *									*
 * Description:	Creates new data for a descriptor using the data of	*
 *		other descriptors if they are temporary.		*
 ************************************************************************/

# ifdef UseFunctionPrototypes
void CreateData (descriptor *d, descriptor *a, descriptor *b, int type, ...)
# else
void CreateData (d, a, b, type, va_alist)
    descriptor *d;
    descriptor *a;
    descriptor *b;
    int		type;
    va_dcl
# endif
{
    void    *ptr;
    int      length;
    int      handler;
    unsigned ncols;
    unsigned nrows;
    va_list  ap;


    if (a && D_Type (a) == type && D_Writable (a)) {
	D_Type    (d) = type;
	D_Union   (d) = D_Union (a);
	D_Trapped (d) = F_False;
	D_Temp    (d) = F_True;
	reused = a;
	d_printf ("new %s (%p) from first\n", D_TypeName (d), D_Pointer (d));

    } else if (b && D_Type (b) == type && D_Writable (b)) {
	D_Type    (d) = type;
	D_Union   (d) = D_Union (b);
	D_Trapped (d) = F_False;
	D_Temp    (d) = F_True;
	reused = b;
	d_printf ("new %s (%p) from second\n", D_TypeName (d), D_Pointer (d));

    } else {
	D_Type    (d) = type;
	D_Trapped (d) = F_False;
	D_Temp    (d) = F_True;
	reused = NULL;
	VA_START (ap, type);

	switch (type) {
	case T_Null:
	    D_Pointer (d) = NULL;
	    break;


	case T_Variable:
	    D_Pointer (d) = NULL;
	    break;


	case T_Function:
	    D_Function (d) = CreateFunction (va_arg (ap, char *));
	    break;


	case T_Intrinsic:
	    D_Intrinsic (d) = va_arg (ap, int);
	    break;


	case T_String:
	    D_String (d) = New (char *);
	    *D_String (d) = Allocate (char, va_arg (ap, int));
	    break;


	case T_Double:
	    D_Double (d) = New (double);
	    break;


	case T_Int:
	    D_Int (d) = New (int);
	    break;


	case T_Byte:
	    D_Byte (d) = New (char);
	    break;


	case T_Array:
	    ptr     = va_arg (ap, void *);
	    type    = va_arg (ap, int);
	    length  = va_arg (ap, int);
	    handler = va_arg (ap, int);
	    D_Array (d) = CreateArray (ptr, type, length, handler);
	    break;


	case T_Row:
	    D_Pointer (d) = NULL;
	    break;


	case T_Matrix:
	    nrows = va_arg (ap, unsigned);
	    ncols = va_arg (ap, unsigned);
	    D_Matrix (d) = CreateFullMatrix (nrows, ncols);
	    break;


	default:
	    break;
	}

	d_printf ("new %s (%p)\n", D_TypeName (d), D_Pointer (d));
	va_end (ap);
    } 
}


/************************************************************************
 * Function:	AssignData						*
 *									*
 * Description:	Assigns the data of a descriptor to another.  The	*
 *		address of the source descriptor is passed so that it	*
 *		may returned to the caller if modified.  A trapped	*
 *		variable handler may attempt to coerce the source data	*
 *		to perform the assignment.				*
 ************************************************************************/

int AssignData (dest, srcp)
    descriptor  *dest;
    descriptor **srcp;
{
    char       *s;
    Matrix	a;
    Matrix	b;
    descriptor *src;
    descriptor *reuse;


    if (dest == *srcp)
	return 0;


    if (D_Trapped (dest) != F_False)
	return CallTrap (D_Trapped (dest), dest, srcp);

    src = *srcp;

    if (D_Temp (src) == F_True) {
	d_printf ("assignment from temporary\n");
	FreeData  (dest);
	D_Type    (dest) = D_Type (src);
	D_Temp	  (dest) = F_True;
	D_Trapped (dest) = F_False;
	D_Union   (dest) = D_Union (src);
	reused = src;

    } else {
	reused = NULL;
	D_Temp (dest) = F_True;
	d_printf ("assignment from non-temporary\n");


	switch (D_Type (src)) {
	case T_Null:
	    FreeData  (dest);
	    D_Type    (dest) = T_Null;
	    D_Temp    (dest) = F_False;
	    D_Trapped (dest) = F_False;
	    D_Pointer (dest) = NULL;
	    break;


	case T_Variable:
	    AssignData (dest, &D_Variable (src));
	    break;


	case T_Function:
	    FreeData  (dest);
	    D_Type     (dest) = T_Function;
	    D_Temp     (dest) = F_True;
	    D_Trapped  (dest) = F_False;
	    D_Function (dest) = CopyFunction (D_Function (src));
	    break;


	case T_Intrinsic:
	    FreeData	(dest);
	    D_Type	(dest) = T_Intrinsic;
	    D_Temp	(dest) = F_False;
	    D_Trapped	(dest) = F_False;
	    D_Intrinsic	(dest) = D_Intrinsic (src);
	    break;


	case T_String:
	    s = *D_String (src);
	    FreeData (dest);
	    CreateData (dest, NULL, NULL, T_String, strlen (s) + 1);
	    strcpy (*D_String (dest), s);
	    break;


	case T_Double:
	    if (!(reuse = D_Type (dest) == T_Double ? dest : NULL))
		FreeData (dest);

	    CreateData (dest, reuse, NULL, T_Double);
	    *D_Double (dest) = *D_Double (src);
	    break;


	case T_Int:
	    if (!(reuse = D_Type (dest) == T_Int ? dest : NULL))
		FreeData (dest);

	    CreateData (dest, reuse, NULL, T_Int);
	    *D_Int (dest) = *D_Int (src);
	    break;


	case T_Byte:
	    if (!(reuse = D_Type (dest) == T_Byte ? dest : NULL))
		FreeData (dest);

	    CreateData (dest, reuse, NULL, T_Byte);
	    *D_Byte (dest) = *D_Byte (src);
	    break;


	case T_Array:
	    FreeData  (dest);
	    D_Type    (dest) = T_Array;
	    D_Temp    (dest) = F_False;
	    D_Trapped (dest) = D_Trapped (src);
	    D_Array   (dest) = CopyArray (D_Array (src));
	    break;


	case T_Matrix:
	    a = D_Matrix (src);
	    reuse = NULL;
	    if (D_Type (dest) == T_Matrix) {
		b = D_Matrix (dest);
		if (Mrows (a) == Mrows (b) && Mcols (a) == Mcols (b))
		    reuse = dest;
	    }

	    if (!reuse)
		FreeData (dest);

	    if (IsCompact (a)) {
		FreeData (dest);
		D_Type	  (dest) = T_Matrix;
		D_Temp	  (dest) = F_False;
		D_Trapped (dest) = F_False;
		D_Matrix  (dest) = CreateCopyMatrix (a);
	    } else {
		CreateData (dest, reuse, NULL, T_Matrix, Mrows (a), Mcols (a));
		CopyMatrix (D_Matrix (dest), a);
	    }
	    break;


	case T_External:
	    D_Type     (dest) = T_External;
	    D_Temp     (dest) = F_False;
	    D_Trapped  (dest) = F_False;
	    D_External (dest) = D_External (src);
	    break;


	default:
	    D_Type    (dest) = D_Type (src);
	    D_Temp    (dest) = F_False;
	    D_Trapped (dest) = F_False;
	    D_Pointer (dest) = D_Pointer (src);
	    break;
	}
    }

    return 0;
}


/************************************************************************
 * Function:	AssignObject						*
 *									*
 * Description:	Special interface to AssignData() that creates a fake	*
 *		descriptor and also sets the destination descriptor to	*
 *		be non-temporary.  This is used in assigning to shared	*
 *		arguments.						*
 ************************************************************************/

int AssignObject (dest, type, temp, ptr)
    descriptor *dest;
    int		type;
    int		temp;
    void       *ptr;
{
    descriptor *src;
    descriptor	scratch;
    int		status;


    src = &scratch;

    D_Type    (src) = type;
    D_Temp    (src) = temp;
    D_Trapped (src) = F_False;
    D_Pointer (src) = ptr;

    if (!(status = AssignData (dest, &src)))
	D_Temp (dest) = F_False;

    return status;
}


/************************************************************************
 * Function:	RecycleData						*
 *									*
 * Description:	Deallocates the data of a descriptor if it is		*
 *		temporary and has not been reused by CreateData().	*
 ************************************************************************/

void RecycleData (d)
    descriptor *d;
{
    if (d && D_Temp (d) == F_True && d != reused)
	FreeData (d);
    else if (d && D_Trapped (d) != F_False && d != reused)
	CallTrap (D_Trapped (d), d, NULL);
    else if (d == reused)
	reused = NULL;
}


/************************************************************************
 * Function:	FreeData						*
 *									*
 * Description:	Deallocates the data of a descriptor.			*
 ************************************************************************/

void FreeData (d)
    descriptor *d;
{
    if (d) {
	d_printf ("freeing %s (%p)\n", D_TypeName (d), D_Pointer (d));


	if (D_Trapped (d) != F_False)
	    CallTrap (D_Trapped (d), d, NULL);

	switch (D_Type (d)) {
	case T_Null:
	    break;


	case T_Variable:
	    break;


	case T_Function:
	    DestroyFunction (D_Function (d));
	    break;


	case T_Intrinsic:
	    break;


	case T_String:
	    Deallocate (*D_String (d));
	    Delete (D_String (d));
	    break;


	case T_Double:
	    Delete (D_Double (d));
	    break;


	case T_Int:
	    Delete (D_Int (d));
	    break;


	case T_Byte:
	    Delete (D_Byte (d));
	    break;


	case T_Array:
	    DestroyArray (D_Array (d));
	    break;


	case T_Row:
	    break;


	case T_Matrix:
	    DestroyMatrix (D_Matrix (d));
	    break;


	default:
	    break;
	}
    }
}


/************************************************************************
 * Function:	PrintData						*
 *									*
 * Description:	Prints the data of a descriptor.			*
 ************************************************************************/

void PrintData (d)
    descriptor *d;
{
    if (d) {
	switch (D_Type (d)) {
	case T_Null:
	    fprintf (stderr, "\tnull\n");
	    break;


	case T_Variable:
	    PrintData (D_Variable (d));
	    break;


	case T_Function:
	    fprintf (stderr, "\tfunction %s\n", D_Function (d) -> name);
	    break;


	case T_Intrinsic:
	    fprintf (stderr, "\tintrinsic %d\n", D_Intrinsic (d));
	    break;


	case T_String:
	    fprintf (stderr, "\t%s\n", *D_String (d));
	    break;


	case T_Double:
	    fprintf (stderr, "\t%g\n", *D_Double (d));
	    break;


	case T_Int:
	    fprintf (stderr, "\t%d\n", *D_Int (d));
	    break;


	case T_Byte:
	    fprintf (stderr, "\t%d\n", *D_Byte (d));
	    break;


	case T_Array:
	    fprintf (stderr, "\tarray\n");
	    break;


	case T_Row:
	    fprintf (stderr, "\trow\n");
	    break;


	case T_Matrix:
	    PrintMatrix (D_Matrix (d), stderr);
	    break;


	case T_MatrixPtr:
	    fprintf (stderr, "\tmatrix_ptr\n");
	    break;


	case T_Constraint:
	    fprintf (stderr, "\tconstraint\n");
	    break;


	case T_Definition:
	    fprintf (stderr, "\tdefinition\n");
	    break;


	case T_Element:
	    fprintf (stderr, "\telement\n");
	    break;


	case T_Force:
	    fprintf (stderr, "\tforce\n");
	    break;


	case T_Load:
	    fprintf (stderr, "\tload\n");
	    break;


	case T_Material:
	    fprintf (stderr, "\tmaterial\n");
	    break;


	case T_Node:
	    fprintf (stderr, "\tnode\n");
	    break;


	case T_Pair:
	    fprintf (stderr, "\tpair\n");
	    break;


	case T_External:
	   fprintf (stderr, "\texternal\n");
	   break;


	default:
	    fprintf (stderr, "\twhat are you trying to print?\n");
	    break;
	}
    }
}
