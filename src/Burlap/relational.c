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
 * File:	relational.c						*
 *									*
 * Description:	This file contains the function definitions for the	*
 *		relational virtual machine instructions.		*
 ************************************************************************/

# include <string.h>
# include "debug.h"
# include "coerce.h"
# include "execute.h"
# include "relational.h"


/************************************************************************
 * Function:	eq_op							*
 *									*
 * Description:	Pops and compares the two descriptors on the top of the	*
 *		stack and places the result on the stack.  The		*
 *		following types are legal for comparison:		*
 *									*
 *		string == string -> double (string comparison)		*
 *		double == double -> double (scalar comparison)		*
 *		double == matrix -> matrix (matrix element-wise cmp)	*
 *		matrix == double -> matrix (matrix element-wise cmp)	*
 *		matrix == matrix -> matrix (matrix element-wise cmp)	*
 *		object == object -> double (object pointer comparison)	*
 *									*
 * 		An attempt is first made to coerce the operands to	*
 *		double values unless both operands are strings.		*
 ************************************************************************/

int eq_op ( )
{
    Matrix	a;
    Matrix	b;
    Matrix	c;
    double	lvalue;
    double	rvalue;
    descriptor *left;
    descriptor *right;
    descriptor *result;
    descriptor	temp;
    int		type_error;
    int		status;
    int		cmp;
    unsigned	i;
    unsigned	j;


    right = pop ( );
    result = top ( );
    temp = *result;
    left = &temp;

    left = deref (left);
    right = deref (right);

    if (D_Type (left) != T_String || D_Type (right) != T_String) {
	left = CoerceData (left, T_Double);
	right = CoerceData (right, T_Double);
    }


    status = 0;
    type_error = F_False;


    switch (D_Type (left)) {
    case T_Double:
	switch (D_Type (right)) {
	case T_Double:
	    D_Type    (result) = T_Double;
	    D_Temp    (result) = F_False;
	    D_Trapped (result) = F_False;
	    D_Double  (result) = dbllit (*D_Double (left) == *D_Double (right));
	    break;


	case T_Matrix:
	    a = D_Matrix (right);
	    CreateData (result, left, right, T_Matrix, Mrows (a), Mcols (a));
	    b = D_Matrix (result);
	    lvalue = *D_Double (left);
	    for (i = 1; i <= Mrows (a); i ++)
		for (j = 1; j <= Mcols (a); j ++)
		    sdata (b, i, j) = lvalue == mdata (a, i, j);
	    break;


	default:
	    type_error = F_True;
	    break;
	}
	break;


    case T_Matrix:
	switch (D_Type (right)) {
	case T_Double:
	    a = D_Matrix (left);
	    CreateData (result, left, right, T_Matrix, Mrows (a), Mcols (a));
	    b = D_Matrix (result);
	    rvalue = *D_Double (right);
	    for (i = 1; i <= Mrows (a); i ++)
		for (j = 1; j <= Mcols (a); j ++)
		    sdata (b, i, j) = mdata (a, i, j) == rvalue;
	    break;


	case T_Matrix:
	    a = D_Matrix (left);
	    b = D_Matrix (right);
	    CreateData (result, left, right, T_Matrix, Mrows (a), Mcols (a));
	    c = D_Matrix (result);
	    if ((status = CompareEQMatrices (c, a, b)))
		MatrixError ("==", a, b, status, F_False);
	    break;


	default:
	    type_error = F_True;
	    break;
	}
	break;


    case T_String:
	switch (D_Type (right)) {
	case T_String:
	    cmp = strcmp (*D_String (left), *D_String (right));
	    D_Type    (result) = T_Double;
	    D_Temp    (result) = F_False;
	    D_Trapped (result) = F_False;
	    D_Double  (result) = dbllit (cmp == 0);
	    break;


	default:
	    type_error = F_True;
	    break;
	}
	break;


    case T_Function:
    case T_Intrinsic:
    case T_Array:
    case T_Pair:
	if (D_Type (left) == D_Type (right)) {
	    D_Type    (result) = T_Double;
	    D_Temp    (result) = F_False;
	    D_Trapped (result) = F_False;
	    D_Double  (result) = dbllit (D_Pointer (left) == D_Pointer (right));
	} else
	    type_error = F_False;
	break;


    case T_Constraint:
    case T_Definition:
    case T_Element:
    case T_Force:
    case T_Load:
    case T_Material:
    case T_Node:
    case T_Stress:
    case T_External:
	if (D_Type (left) == D_Type (right)) {
	    cmp = *(void **) D_Pointer (left) == *(void **) D_Pointer (right);
	    D_Type    (result) = T_Double;
	    D_Temp    (result) = F_False;
	    D_Trapped (result) = F_False;
	    D_Double  (result) = dbllit (cmp);
	} else if (D_Type (right) == T_Null) {
	    cmp = *(void **) D_Pointer (left) == NULL;
	    D_Type    (result) = T_Double;
	    D_Temp    (result) = F_False;
	    D_Trapped (result) = F_False;
	    D_Double  (result) = dbllit (cmp);
	} else
	    type_error = F_True;
	break;


    case T_Null:
	switch (D_Type (right)) {
	case T_Constraint:
	case T_Definition:
	case T_Element:
	case T_Force:
	case T_Load:
	case T_Material:
	case T_Node:
	case T_Stress:
	case T_External:
	    cmp = *(void **) D_Pointer (right) == NULL;
	    D_Type    (result) = T_Double;
	    D_Temp    (result) = F_False;
	    D_Trapped (result) = F_False;
	    D_Double  (result) = dbllit (cmp);
	    break;


	default:
	    type_error = F_True;
	    break;
	}
	break;


    default:
	type_error = F_True;
	break;
    }


    if (type_error == F_True)
	TypeError ("==", left, right, NULL, F_False);


    RecycleData (left);
    RecycleData (right);
    d_printf ("eq ans =\n");
    d_PrintData (result);

    return type_error == F_True || status != 0;
}


/************************************************************************
 * Function:	ge_op							*
 *									*
 * Description:	Pops and compares the two descriptors on the top of the	*
 *		stack and places the result on the stack.  The		*
 *		following types are legal for comparison:		*
 *									*
 *		string >= string -> double (string comparison)		*
 *		double >= double -> double (scalar comparison)		*
 *		double >= matrix -> matrix (matrix element-wise cmp)	*
 *		matrix >= double -> matrix (matrix element-wise cmp)	*
 *		matrix >= matrix -> matrix (matrix element-wise cmp)	*
 *									*
 * 		An attempt is first made to coerce the operands to	*
 *		double values unless both operands are strings.		*
 ************************************************************************/

int ge_op ( )
{
    Matrix	a;
    Matrix	b;
    Matrix	c;
    double	lvalue;
    double	rvalue;
    descriptor *left;
    descriptor *right;
    descriptor *result;
    descriptor	temp;
    int		type_error;
    int		status;
    int		cmp;
    unsigned	i;
    unsigned	j;


    right = pop ( );
    result = top ( );
    temp = *result;
    left = &temp;

    left = deref (left);
    right = deref (right);

    if (D_Type (left) != T_String || D_Type (right) != T_String) {
	left = CoerceData (left, T_Double);
	right = CoerceData (right, T_Double);
    }


    status = 0;
    type_error = F_False;


    switch (D_Type (left)) {
    case T_Double:
	switch (D_Type (right)) {
	case T_Double:
	    D_Type    (result) = T_Double;
	    D_Temp    (result) = F_False;
	    D_Trapped (result) = F_False;
	    D_Double  (result) = dbllit (*D_Double (left) >= *D_Double (right));
	    break;


	case T_Matrix:
	    a = D_Matrix (right);
	    CreateData (result, left, right, T_Matrix, Mrows (a), Mcols (a));
	    b = D_Matrix (result);
	    lvalue = *D_Double (left);
	    for (i = 1; i <= Mrows (a); i ++)
		for (j = 1; j <= Mcols (a); j ++)
		    sdata (b, i, j) = lvalue >= mdata (a, i, j);
	    break;


	default:
	    type_error = F_True;
	    break;
	}
	break;


    case T_Matrix:
	switch (D_Type (right)) {
	case T_Double:
	    a = D_Matrix (left);
	    CreateData (result, left, right, T_Matrix, Mrows (a), Mcols (a));
	    b = D_Matrix (result);
	    rvalue = *D_Double (right);
	    for (i = 1; i <= Mrows (a); i ++)
		for (j = 1; j <= Mcols (a); j ++)
		    sdata (b, i, j) = mdata (a, i, j) >= rvalue;
	    break;


	case T_Matrix:
	    a = D_Matrix (left);
	    b = D_Matrix (right);
	    CreateData (result, left, right, T_Matrix, Mrows (a), Mcols (a));
	    c = D_Matrix (result);
	    if ((status = CompareGTEMatrices (c, a, b)))
		MatrixError (">=", a, b, status, F_False);
	    break;


	default:
	    type_error = F_True;
	    break;
	}
	break;


    case T_String:
	switch (D_Type (right)) {
	case T_String:
	    cmp = strcmp (*D_String (left), *D_String (right));
	    D_Type    (result) = T_Double;
	    D_Temp    (result) = F_False;
	    D_Trapped (result) = F_False;
	    D_Double  (result) = dbllit (cmp >= 0);
	    break;


	default:
	    type_error = F_True;
	    break;
	}
	break;


    default:
	type_error = F_True;
	break;
    }


    if (type_error == F_True)
	TypeError (">=", left, right, NULL, F_False);


    RecycleData (left);
    RecycleData (right);
    d_printf ("ge ans =\n");
    d_PrintData (result);

    return type_error == F_True || status != 0;
}


/************************************************************************
 * Function:	gt_op							*
 *									*
 * Description:	Pops and compares the two descriptors on the top of the	*
 *		stack and places the result on the stack.  The		*
 *		following types are legal for comparison:		*
 *									*
 *		string > string -> double (string comparison)		*
 *		double > double -> double (scalar comparison)		*
 *		double > matrix -> matrix (matrix element-wise cmp)	*
 *		matrix > double -> matrix (matrix element-wise cmp)	*
 *		matrix > matrix -> matrix (matrix element-wise cmp)	*
 *									*
 * 		An attempt is first made to coerce the operands to	*
 *		double values unless both operands are strings.		*
 ************************************************************************/

int gt_op ( )
{
    Matrix	a;
    Matrix	b;
    Matrix	c;
    double	lvalue;
    double	rvalue;
    descriptor *left;
    descriptor *right;
    descriptor *result;
    descriptor	temp;
    int		type_error;
    int		status;
    int		cmp;
    unsigned	i;
    unsigned	j;


    right = pop ( );
    result = top ( );
    temp = *result;
    left = &temp;

    left = deref (left);
    right = deref (right);

    if (D_Type (left) != T_String || D_Type (right) != T_String) {
	left = CoerceData (left, T_Double);
	right = CoerceData (right, T_Double);
    }


    status = 0;
    type_error = F_False;


    switch (D_Type (left)) {
    case T_Double:
	switch (D_Type (right)) {
	case T_Double:
	    D_Type    (result) = T_Double;
	    D_Temp    (result) = F_False;
	    D_Trapped (result) = F_False;
	    D_Double  (result) = dbllit (*D_Double (left) > *D_Double (right));
	    break;


	case T_Matrix:
	    a = D_Matrix (right);
	    CreateData (result, left, right, T_Matrix, Mrows (a), Mcols (a));
	    b = D_Matrix (result);
	    lvalue = *D_Double (left);
	    for (i = 1; i <= Mrows (a); i ++)
		for (j = 1; j <= Mcols (a); j ++)
		    sdata (b, i, j) = lvalue > mdata (a, i, j);
	    break;


	default:
	    type_error = F_True;
	    break;
	}
	break;


    case T_Matrix:
	switch (D_Type (right)) {
	case T_Double:
	    a = D_Matrix (left);
	    CreateData (result, left, right, T_Matrix, Mrows (a), Mcols (a));
	    b = D_Matrix (result);
	    rvalue = *D_Double (right);
	    for (i = 1; i <= Mrows (a); i ++)
		for (j = 1; j <= Mcols (a); j ++)
		    sdata (b, i, j) = mdata (a, i, j) > rvalue;
	    break;


	case T_Matrix:
	    a = D_Matrix (left);
	    b = D_Matrix (right);
	    CreateData (result, left, right, T_Matrix, Mrows (a), Mcols (a));
	    c = D_Matrix (result);
	    if ((status = CompareGTMatrices (c, a, b)))
		MatrixError (">", a, b, status, F_False);
	    break;


	default:
	    type_error = F_True;
	    break;
	}
	break;


    case T_String:
	switch (D_Type (right)) {
	case T_String:
	    cmp = strcmp (*D_String (left), *D_String (right));
	    D_Type    (result) = T_Double;
	    D_Temp    (result) = F_False;
	    D_Trapped (result) = F_False;
	    D_Double  (result) = dbllit (cmp > 0);
	    break;


	default:
	    type_error = F_True;
	    break;
	}
	break;


    default:
	type_error = F_True;
	break;
    }


    if (type_error == F_True)
	TypeError (">", left, right, NULL, F_False);


    RecycleData (left);
    RecycleData (right);
    d_printf ("gt ans =\n");
    d_PrintData (result);

    return type_error == F_True || status != 0;
}


/************************************************************************
 * Function:	le_op							*
 *									*
 * Description:	Pops and compares the two descriptors on the top of the	*
 *		stack and places the result on the stack.  The		*
 *		following types are legal for comparison:		*
 *									*
 *		string <= string -> double (string comparison)		*
 *		double <= double -> double (scalar comparison)		*
 *		double <= matrix -> matrix (matrix element-wise cmp)	*
 *		matrix <= double -> matrix (matrix element-wise cmp)	*
 *		matrix <= matrix -> matrix (matrix element-wise cmp)	*
 *									*
 * 		An attempt is first made to coerce the operands to	*
 *		double values unless both operands are strings.		*
 ************************************************************************/

int le_op ( )
{
    Matrix	a;
    Matrix	b;
    Matrix	c;
    double	lvalue;
    double	rvalue;
    descriptor *left;
    descriptor *right;
    descriptor *result;
    descriptor	temp;
    int		type_error;
    int		status;
    int		cmp;
    unsigned	i;
    unsigned	j;


    right = pop ( );
    result = top ( );
    temp = *result;
    left = &temp;

    left = deref (left);
    right = deref (right);

    if (D_Type (left) != T_String || D_Type (right) != T_String) {
	left = CoerceData (left, T_Double);
	right = CoerceData (right, T_Double);
    }


    status = 0;
    type_error = F_False;


    switch (D_Type (left)) {
    case T_Double:
	switch (D_Type (right)) {
	case T_Double:
	    D_Type    (result) = T_Double;
	    D_Temp    (result) = F_False;
	    D_Trapped (result) = F_False;
	    D_Double  (result) = dbllit (*D_Double (left) <= *D_Double (right));
	    break;


	case T_Matrix:
	    a = D_Matrix (right);
	    CreateData (result, left, right, T_Matrix, Mrows (a), Mcols (a));
	    b = D_Matrix (result);
	    lvalue = *D_Double (left);
	    for (i = 1; i <= Mrows (a); i ++)
		for (j = 1; j <= Mcols (a); j ++)
		    sdata (b, i, j) = lvalue <= mdata (a, i, j);
	    break;


	default:
	    type_error = F_True;
	    break;
	}
	break;


    case T_Matrix:
	switch (D_Type (right)) {
	case T_Double:
	    a = D_Matrix (left);
	    CreateData (result, left, right, T_Matrix, Mrows (a), Mcols (a));
	    b = D_Matrix (result);
	    rvalue = *D_Double (right);
	    for (i = 1; i <= Mrows (a); i ++)
		for (j = 1; j <= Mcols (a); j ++)
		    sdata (b, i, j) = mdata (a, i, j) <= rvalue;
	    break;


	case T_Matrix:
	    a = D_Matrix (left);
	    b = D_Matrix (right);
	    CreateData (result, left, right, T_Matrix, Mrows (a), Mcols (a));
	    c = D_Matrix (result);
	    if ((status = CompareLTEMatrices (c, a, b)))
		MatrixError ("<=", a, b, status, F_False);
	    break;


	default:
	    type_error = F_True;
	    break;
	}
	break;


    case T_String:
	switch (D_Type (right)) {
	case T_String:
	    cmp = strcmp (*D_String (left), *D_String (right));
	    D_Type    (result) = T_Double;
	    D_Temp    (result) = F_False;
	    D_Trapped (result) = F_False;
	    D_Double  (result) = dbllit (cmp <= 0);
	    break;


	default:
	    type_error = F_True;
	    break;
	}
	break;


    default:
	type_error = F_True;
	break;
    }


    if (type_error == F_True)
	TypeError ("<=", left, right, NULL, F_False);


    RecycleData (left);
    RecycleData (right);
    d_printf ("le ans =\n");
    d_PrintData (result);

    return type_error == F_True || status != 0;
}


/************************************************************************
 * Function:	lt_op							*
 *									*
 * Description:	Pops and compares the two descriptors on the top of the	*
 *		stack and places the result on the stack.  The		*
 *		following types are legal for comparison:		*
 *									*
 *		string < string -> double (string comparison)		*
 *		double < double -> double (scalar comparison)		*
 *		double < matrix -> matrix (matrix element-wise cmp)	*
 *		matrix < double -> matrix (matrix element-wise cmp)	*
 *		matrix < matrix -> matrix (matrix element-wise cmp)	*
 *									*
 * 		An attempt is first made to coerce the operands to	*
 *		double values unless both operands are strings.		*
 ************************************************************************/

int lt_op ( )
{
    Matrix	a;
    Matrix	b;
    Matrix	c;
    double	lvalue;
    double	rvalue;
    descriptor *left;
    descriptor *right;
    descriptor *result;
    descriptor	temp;
    int		type_error;
    int		status;
    int		cmp;
    unsigned	i;
    unsigned	j;


    right = pop ( );
    result = top ( );
    temp = *result;
    left = &temp;

    left = deref (left);
    right = deref (right);

    if (D_Type (left) != T_String || D_Type (right) != T_String) {
	left = CoerceData (left, T_Double);
	right = CoerceData (right, T_Double);
    }


    status = 0;
    type_error = F_False;


    switch (D_Type (left)) {
    case T_Double:
	switch (D_Type (right)) {
	case T_Double:
	    D_Type    (result) = T_Double;
	    D_Temp    (result) = F_False;
	    D_Trapped (result) = F_False;
	    D_Double  (result) = dbllit (*D_Double (left) < *D_Double (right));
	    break;


	case T_Matrix:
	    a = D_Matrix (right);
	    CreateData (result, left, right, T_Matrix, Mrows (a), Mcols (a));
	    b = D_Matrix (result);
	    lvalue = *D_Double (left);
	    for (i = 1; i <= Mrows (a); i ++)
		for (j = 1; j <= Mcols (a); j ++)
		    sdata (b, i, j) = lvalue < mdata (a, i, j);
	    break;


	default:
	    type_error = F_True;
	    break;
	}
	break;


    case T_Matrix:
	switch (D_Type (right)) {
	case T_Double:
	    a = D_Matrix (left);
	    CreateData (result, left, right, T_Matrix, Mrows (a), Mcols (a));
	    b = D_Matrix (result);
	    rvalue = *D_Double (right);
	    for (i = 1; i <= Mrows (a); i ++)
		for (j = 1; j <= Mcols (a); j ++)
		    sdata (b, i, j) = mdata (a, i, j) < rvalue;
	    break;


	case T_Matrix:
	    a = D_Matrix (left);
	    b = D_Matrix (right);
	    CreateData (result, left, right, T_Matrix, Mrows (a), Mcols (a));
	    c = D_Matrix (result);
	    if ((status = CompareLTMatrices (c, a, b)))
		MatrixError ("<", a, b, status, F_False);
	    break;


	default:
	    type_error = F_True;
	    break;
	}
	break;


    case T_String:
	switch (D_Type (right)) {
	case T_String:
	    cmp = strcmp (*D_String (left), *D_String (right));
	    D_Type    (result) = T_Double;
	    D_Temp    (result) = F_False;
	    D_Trapped (result) = F_False;
	    D_Double  (result) = dbllit (cmp < 0);
	    break;


	default:
	    type_error = F_True;
	    break;
	}
	break;


    default:
	type_error = F_True;
	break;
    }


    if (type_error == F_True)
	TypeError ("<", left, right, NULL, F_False);


    RecycleData (left);
    RecycleData (right);
    d_printf ("lt ans =\n");
    d_PrintData (result);

    return type_error == F_True || status != 0;
}


/************************************************************************
 * Function:	ne_op							*
 *									*
 * Description:	Pops and compares the two descriptors on the top of the	*
 *		stack and places the result on the stack.  The		*
 *		following types are legal for comparison:		*
 *									*
 *		string != string -> double (string comparison)		*
 *		double != double -> double (scalar comparison)		*
 *		double != matrix -> matrix (matrix element-wise cmp)	*
 *		matrix != double -> matrix (matrix element-wise cmp)	*
 *		matrix != matrix -> matrix (matrix element-wise cmp)	*
 *		object != object -> double (object pointer comparison)	*
 *									*
 * 		An attempt is first made to coerce the operands to	*
 *		double values unless both operands are strings.		*
 ************************************************************************/

int ne_op ( )
{
    Matrix	a;
    Matrix	b;
    Matrix	c;
    double	lvalue;
    double	rvalue;
    descriptor *left;
    descriptor *right;
    descriptor *result;
    descriptor	temp;
    int		type_error;
    int		status;
    int		cmp;
    unsigned	i;
    unsigned	j;


    right = pop ( );
    result = top ( );
    temp = *result;
    left = &temp;

    left = deref (left);
    right = deref (right);

    if (D_Type (left) != T_String || D_Type (right) != T_String) {
	left = CoerceData (left, T_Double);
	right = CoerceData (right, T_Double);
    }


    status = 0;
    type_error = F_False;


    switch (D_Type (left)) {
    case T_Double:
	switch (D_Type (right)) {
	case T_Double:
	    D_Type    (result) = T_Double;
	    D_Temp    (result) = F_False;
	    D_Trapped (result) = F_False;
	    D_Double  (result) = dbllit (*D_Double (left) != *D_Double (right));
	    break;


	case T_Matrix:
	    a = D_Matrix (right);
	    CreateData (result, left, right, T_Matrix, Mrows (a), Mcols (a));
	    b = D_Matrix (result);
	    lvalue = *D_Double (left);
	    for (i = 1; i <= Mrows (a); i ++)
		for (j = 1; j <= Mcols (a); j ++)
		    sdata (b, i, j) = lvalue != mdata (a, i, j);
	    break;


	default:
	    type_error = F_True;
	    break;
	}
	break;


    case T_Matrix:
	switch (D_Type (right)) {
	case T_Double:
	    a = D_Matrix (left);
	    CreateData (result, left, right, T_Matrix, Mrows (a), Mcols (a));
	    b = D_Matrix (result);
	    rvalue = *D_Double (right);
	    for (i = 1; i <= Mrows (a); i ++)
		for (j = 1; j <= Mcols (a); j ++)
		    sdata (b, i, j) = mdata (a, i, j) != rvalue;
	    break;


	case T_Matrix:
	    a = D_Matrix (left);
	    b = D_Matrix (right);
	    CreateData (result, left, right, T_Matrix, Mrows (a), Mcols (a));
	    c = D_Matrix (result);
	    if ((status = CompareNEQMatrices (c, a, b)))
		MatrixError ("!=", a, b, status, F_False);
	    break;


	default:
	    type_error = F_True;
	    break;
	}
	break;


    case T_String:
	switch (D_Type (right)) {
	case T_String:
	    cmp = strcmp (*D_String (left), *D_String (right));
	    D_Type    (result) = T_Double;
	    D_Temp    (result) = F_False;
	    D_Trapped (result) = F_False;
	    D_Double  (result) = dbllit (cmp != 0);
	    break;


	default:
	    type_error = F_True;
	    break;
	}
	break;


    case T_Function:
    case T_Intrinsic:
    case T_Array:
    case T_Pair:
	if (D_Type (left) == D_Type (right)) {
	    D_Type    (result) = T_Double;
	    D_Temp    (result) = F_False;
	    D_Trapped (result) = F_False;
	    D_Double  (result) = dbllit (D_Pointer (left) == D_Pointer (right));
	} else
	    type_error = F_False;
	break;


    case T_Constraint:
    case T_Definition:
    case T_Element:
    case T_Force:
    case T_Load:
    case T_Material:
    case T_Node:
    case T_Stress:
    case T_External:
	if (D_Type (left) == D_Type (right)) {
	    cmp = *(void **) D_Pointer (left) != *(void **) D_Pointer (right);
	    D_Type    (result) = T_Double;
	    D_Temp    (result) = F_False;
	    D_Trapped (result) = F_False;
	    D_Double  (result) = dbllit (cmp);
	} else if (D_Type (right) == T_Null) {
	    cmp = *(void **) D_Pointer (left) != NULL;
	    D_Type    (result) = T_Double;
	    D_Temp    (result) = F_False;
	    D_Trapped (result) = F_False;
	    D_Double  (result) = dbllit (cmp);
	} else
	    type_error = F_True;
	break;


    case T_Null:
	switch (D_Type (right)) {
	case T_Constraint:
	case T_Definition:
	case T_Element:
	case T_Force:
	case T_Load:
	case T_Material:
	case T_Node:
	case T_Stress:
	case T_External:
	    cmp = *(void **) D_Pointer (right) != NULL;
	    D_Type    (result) = T_Double;
	    D_Temp    (result) = F_False;
	    D_Trapped (result) = F_False;
	    D_Double  (result) = dbllit (cmp);
	    break;


	default:
	    type_error = F_True;
	    break;
	}
	break;


    default:
	type_error = F_True;
	break;
    }


    if (type_error == F_True)
	TypeError ("!=", left, right, NULL, F_False);


    RecycleData (left);
    RecycleData (right);
    d_printf ("ne ans =\n");
    d_PrintData (result);

    return type_error == F_True || status != 0;
}
