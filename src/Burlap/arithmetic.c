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
 * File:	arithmetic.c						*
 *									*
 * Description:	This file contains the function definitions for the	*
 *		arithmetic virtual machine instructions.		*
 ************************************************************************/

# include <math.h>
# include "debug.h"
# include "coerce.h"
# include "execute.h"
# include "arithmetic.h"


/************************************************************************
 * Function:	add_op							*
 *									*
 * Description:	Pops and adds the two descriptors on the top of the	*
 *		stack and places the result on the stack.  The		*
 *		following types are legal for addition:			*
 *									*
 *		double + double -> double (scalar addition)		*
 *		double + matrix -> matrix (matrix scaling)		*
 *		matrix + double -> matrix (matrix scaling)		*
 *		matrix + matrix -> matrix (matrix addition)		*
 *									*
 *		An attempt is first made to coerce both operands to	*
 *		double values.						*
 ************************************************************************/

int add_op ( )
{
    Matrix	a;
    Matrix	b;
    Matrix	c;
    descriptor *left;
    descriptor *right;
    descriptor *result;
    descriptor	temp;
    int		type_error;
    int		status;


    right = pop ( );
    result = top ( );
    temp = *result;
    left = &temp;


    left = CoerceData (deref (left), T_Double);
    right = CoerceData (deref (right), T_Double);


    status = 0;
    type_error = F_False;


    switch (D_Type (left)) {
    case T_Double:
	switch (D_Type (right)) {
	case T_Double:
	    CreateData (result, left, right, T_Double);
	    *D_Double (result) = *D_Double (left) + *D_Double (right);
	    break;


	case T_Matrix:
	    a = D_Matrix (right);
	    CreateData (result, left, right, T_Matrix, Mrows (a), Mcols (a));
	    b = D_Matrix (result);
	    if ((status = ScaleMatrix (b, a, 1.0, *D_Double (left))))
		MatrixError ("+", a, NULL, status, F_False);
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
	    if ((status = ScaleMatrix (b, a, 1.0, *D_Double (right))))
		MatrixError ("+", a, NULL, status, F_False);
	    break;


	case T_Matrix:
	    a = D_Matrix (left);
	    b = D_Matrix (right);
	    CreateData (result, left, right, T_Matrix, Mrows (a), Mcols (a));
	    c = D_Matrix (result);
	    if ((status = AddMatrices (c, a, b)))
		MatrixError ("+", a, b, status, F_False);
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
	TypeError ("+", left, right, NULL, F_False);


    RecycleData (left);
    RecycleData (right);
    d_printf ("add ans =\n");
    d_PrintData (result);

    return type_error == F_True || status != 0;
}


/************************************************************************
 * Function:	bkslv_op						*
 *									*
 * Description:	Pops and left divides the two descriptors on the top of	*
 *		the stack and places the result on the stack.  The	*
 *		following types are legal for left division:		*
 *									*
 *		double \ double -> double (scalar left division)	*
 *		double \ matrix -> matrix (matrix scaling)		*
 *		matrix \ double -> matrix (matrix inversion + scaling)	*
 *		matrix \ matrix -> matrix (matrix backsolve)		*
 *									*
 *		An attempt is first made to coerce the operands to	*
 *		double values.						*
 ************************************************************************/

int bkslv_op ( )
{
    Matrix	a;
    Matrix	b;
    Matrix	c;
    Matrix	l;
    Matrix	p;
    Matrix	v;
    double	lvalue;
    descriptor *left;
    descriptor *right;
    descriptor *result;
    descriptor	temp;
    int		type_error;
    int		status;
    int		info;
    unsigned	i;
    unsigned	j;
    unsigned	cols;
    unsigned	rows;


    right = pop ( );
    result = top ( );
    temp = *result;
    left = &temp;


    left = CoerceData (deref (left), T_Double);
    right = CoerceData (deref (right), T_Double);


    status = 0;
    type_error = F_False;


    switch (D_Type (left)) {
    case T_Double:
	switch (D_Type (right)) {
	case T_Double:
	    if ((lvalue = *D_Double (left)) == 0) {
		MathException ("left division by zero");
		status = 1;
	    } else {
		CreateData (result, left, right, T_Double);
		*D_Double (result) = *D_Double (right) / lvalue;
	    }
	    break;


	case T_Matrix:
	    if ((lvalue = *D_Double (left)) == 0) {
		MathException ("left division by zero");
		status = 1;
	    } else {
		a = D_Matrix (right);
		CreateData (result, NULL, right, T_Matrix, Mrows(a), Mcols(a));
		b = D_Matrix (result);
		if ((status = ScaleMatrix (b, a, 1.0 / lvalue, 0.0)))
		    MatrixError ("\\", NULL, a, status, F_False);
	    }
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
	    CreateData (result, left, NULL, T_Matrix, Mrows (a), Mcols (a));
	    b = D_Matrix (result);
	    p = CreateColumnVector (Mrows (a));
	    if ((status = LUFactorMatrix (b, a, p, &info)))
		MatrixError ("\\", a, NULL, status, F_False);
	    else if (info)
		MathException ("singular matrix in left division"), status = 1;
	    else if ((status = InvertMatrix (b, b, p)))
		MatrixError ("\\", a, NULL, status, F_False);
	    else if ((status = ScaleMatrix (b, b, *D_Double (right), 0.0)))
		MatrixError ("\\", a, NULL, status, F_False);
	    DestroyMatrix (p);
	    break;


	case T_Matrix:
	    a = D_Matrix (left);
	    b = D_Matrix (right);
	    CreateData (result, right, NULL, T_Matrix, Mrows (b), Mcols (b));
	    c = D_Matrix (result);
	    rows = Mrows (a);
	    cols = Mcols (a);


	    if (IsCompact (a) && rows == cols) {
		l = D_Temp (left) ? a : CreateCopyMatrix (a);
		CroutFactorMatrix (l);
		if (Mcols (b) == 1) {
		    CopyMatrix (c, b);
		    if ((status = CroutBackSolveMatrix (l, c)))
			MatrixError ("\\", a, b, status, F_False);
		} else {
		    v = CreateColumnVector (Mrows (b));
		    for (j = 1; j <= Mcols (b); j ++) {
			for (i = 1; i <= Mrows (b); i ++)
			    sdata (v, i, 1) = mdata (b, i, j);
			if ((status = CroutBackSolveMatrix (l, v))) {
			    MatrixError ("\\", a, b, status, F_False);
			    j = Mcols (b);
			    break;
			}
			for (i = 1; i <= Mrows (b); i ++)
			    sdata (c, i, j) = mdata (v, i, 1);
		    }
		    DestroyMatrix (v);
		}
	    } else {
		p = CreateColumnVector (rows);
		l = D_Writable (left) ? a : CreateFullMatrix (rows, cols);

		if ((status = LUFactorMatrix (l, a, p, &info)))
		    MatrixError ("\\", a, b, status, F_False);
		else if (info) {
		    MathException ("singular matrix in left division");
		    status = 1;
		} else if (Mcols (b) != 1) {
		    v = CreateColumnVector (Mrows (b));
		    for (j = 1; j <= Mcols (b); j ++) {
			for (i = 1; i <= Mrows (b); i ++)
			    sdata (v, i, 1) = mdata (b, i, j);
			if ((status = LUBackSolveMatrix (v, l, v, p))) {
			    MatrixError ("\\", a, b, status, F_False);
			    j = Mcols (b);
			    break;
			}
			for (i = 1; i <= Mrows (b); i ++)
			    sdata (c, i, j) = mdata (v, i, 1);
		    }
		    DestroyMatrix (v);
		} else if ((status = LUBackSolveMatrix (c, l, b, p)))
		    MatrixError ("\\", a, b, status, F_False);

		DestroyMatrix (p);
	    }

	    if (a != l)
		DestroyMatrix (l);
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
	TypeError ("\\", left, right, NULL, F_False);


    RecycleData (left);
    RecycleData (right);
    d_printf ("bkslv ans =\n");
    d_PrintData (result);

    return type_error == F_True || status != 0;
}


/************************************************************************
 * Function:	div_op							*
 *									*
 * Description:	Pops and right divides the two descriptors on the top	*
 *		of the stack and places the result on the stack.  The	*
 *		following types are legal for right division:		*
 *									*
 *		double / double -> double (scalar right division)	*
 *		double / matrix -> matrix (matrix inversion + scaling)	*
 *		matrix / double -> matrix (matrix scaling)		*
 *		matrix / matrix -> matrix (matrix transposed backsolve) *
 *									*
 *		An attempt is first made to coerce the operands to	*
 *		double values.						*
 ************************************************************************/

int div_op ( )
{
    Matrix	a;
    Matrix	b;
    Matrix	c;
    Matrix	l;
    Matrix	p;
    Matrix	v;
    Matrix	at;
    Matrix	bt;
    double	rvalue;
    descriptor *left;
    descriptor *right;
    descriptor *result;
    descriptor	temp;
    int		type_error;
    int		status;
    int		info;
    unsigned	i;
    unsigned	j;


    right = pop ( );
    result = top ( );
    temp = *result;
    left = &temp;


    left = CoerceData (deref (left), T_Double);
    right = CoerceData (deref (right), T_Double);


    status = 0;
    type_error = F_False;


    switch (D_Type (left)) {
    case T_Double:
	switch (D_Type (right)) {
	case T_Double:
	    if ((rvalue = *D_Double (right)) == 0) {
		MathException ("right division by zero");
		status = 1;
	    } else {
		CreateData (result, left, right, T_Double);
		*D_Double (result) = *D_Double (left) / rvalue;
	    }
	    break;


	case T_Matrix:
	    a = D_Matrix (right);
	    CreateData (result, right, NULL, T_Matrix, Mrows (a), Mcols (a));
	    b = D_Matrix (result);
	    p = CreateColumnVector (Mrows (a));
	    if ((status = LUFactorMatrix (b, a, p, &info)))
		MatrixError ("/", NULL, a, status, F_False);
	    else if (info)
		MathException ("singular matrix in right division"), status = 1;
	    else if ((status = InvertMatrix (b, b, p)))
		MatrixError ("/", NULL, a, status, F_False);
	    else if ((status = ScaleMatrix (b, b, *D_Double (left), 0.0)))
		MatrixError ("/", NULL, a, status, F_False);
	    DestroyMatrix (p);
	    break;


	default:
	    type_error = F_True;
	    break;
	}
	break;


    case T_Matrix:
	switch (D_Type (right)) {
	case T_Double:
	    if ((rvalue = *D_Double (right)) == 0) {
		MathException ("right division by zero");
		status = 1;
	    } else {
		a = D_Matrix (left);
		CreateData (result, left, NULL, T_Matrix, Mrows (a), Mcols (a));
		b = D_Matrix (result);
		if ((status = ScaleMatrix (b, a, 1.0 / rvalue, 0.0)))
		    MatrixError ("/", a, NULL, status, F_False);
	    }
	    break;


	case T_Matrix:
	    a = D_Matrix (left);
	    b = D_Matrix (right);
	    CreateData (result, left, NULL, T_Matrix, Mrows (a), Mcols (a));
	    c = D_Matrix (result);

	    at = CreateFullMatrix (Mcols (a), Mrows (a));
	    bt = CreateFullMatrix (Mcols (b), Mrows (b));
	    TransposeMatrix (at, a);
	    TransposeMatrix (bt, b);

	    p = CreateColumnVector (Mrows (bt));
	    l = bt;

	    if ((status = LUFactorMatrix (l, bt, p, &info)))
		MatrixError ("/", a, b, status, F_False);
	    else if (info) {
		MathException ("singular matrix in right division");
		status = 1;
	    } else if (Mcols (at) != 1) {
		v = CreateColumnVector (Mrows (at));
		for (j = 1; j <= Mcols (at); j ++) {
		    for (i = 1; i <= Mrows (at); i ++)
			sdata (v, i, 1) = mdata (at, i, j);
		    if ((status = LUBackSolveMatrix (v, l, v, p))) {
			MatrixError ("/", a, b, status, F_False);
			j = Mcols (at);
			break;
		    }
		    for (i = 1; i <= Mrows (at); i ++)
			sdata (c, j, i) = mdata (v, i, 1);
		}
		DestroyMatrix (v);
	    } else if ((status = LUBackSolveMatrix (at, l, at, p)))
		MatrixError ("/", a, b, status, F_False);
	    else
		TransposeMatrix (c, at);

	    DestroyMatrix (at);
	    DestroyMatrix (bt);
	    DestroyMatrix (p);
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
	TypeError ("/", left, right, NULL, F_False);


    RecycleData (left);
    RecycleData (right);
    d_printf ("div ans =\n");
    d_PrintData (result);

    return type_error == F_True || status != 0;
}


/************************************************************************
 * Function:	mod_op							*
 *									*
 * Description:	Pops and modulos the two descriptors on the top of the	*
 *		stack and places the result on the stack.  The		*
 *		following types are legal for modulo:			*
 *									*
 *		double % double -> double (scalar modulo)		*
 *		double % matrix -> matrix (matrix element-wise modulo)	*
 *		matrix % double -> matrix (matrix element-wise modulo)	*
 *		matrix % matrix -> matrix (matrix element-wise modulo)	*
 *									*
 *		An attempt is first made to coerce both operands to	*
 *		double values.						*
 ************************************************************************/

int mod_op ( )
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
    unsigned	i;
    unsigned	j;


    right = pop ( );
    result = top ( );
    temp = *result;
    left = &temp;


    left = CoerceData (deref (left), T_Double);
    right = CoerceData (deref (right), T_Double);


    status = 0;
    type_error = F_False;


    switch (D_Type (left)) {
    case T_Double:
	switch (D_Type (right)) {
	case T_Double:
	    CreateData (result, left, right, T_Double);
	    if ((rvalue = *D_Double (right)) == 0) {
		MathException ("remainder by zero");
		status = 1;
	    } else
		*D_Double (result) = fmod (*D_Double (left), rvalue);
	    break;


	case T_Matrix:
	    a = D_Matrix (right);
	    CreateData (result, left, right, T_Matrix, Mrows (a), Mcols (a));
	    b = D_Matrix (result);
	    lvalue = *D_Double (left);
	    for (i = 1; i <= Mrows (a); i ++)
		for (j = 1; j <= Mcols (a); j ++)
		    sdata (b, i, j) = fmod (lvalue, mdata (a, i, j));
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
		    sdata (b, i, j) = fmod (mdata (a, i, j), rvalue);
	    break;


	case T_Matrix:
	    a = D_Matrix (left);
	    b = D_Matrix (right);
	    CreateData (result, left, right, T_Matrix, Mrows (a), Mcols (a));
	    c = D_Matrix (result);
	    if (((status = ModMatrices (c, a, b))))
		MatrixError ("%", a, b, status, F_False);
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
	TypeError ("%", left, right, NULL, F_False);


    RecycleData (left);
    RecycleData (right);
    d_printf ("mod ans =\n");
    d_PrintData (result);

    return type_error == F_True || status != 0;
}


/************************************************************************
 * Function:	mul_op							*
 *									*
 * Description:	Pops and multiplies the two descriptors on the top of	*
 *		the stack and places the result on the stack.  The	*
 *		following types are legal for multiplication:		*
 *									*
 *		double * double -> double (scalar multiplication)	*
 *		double * matrix -> matrix (matrix scaling)		*
 *		matrix * double -> matrix (matrix scaling)		*
 *		matrix * matrix -> matrix (matrix multiplication)	*
 *									*
 * 		An attempt is first made to coerce both operands to	*
 *		double values.  If the resulting matrix consists of	*
 *		only a single scalar then the result will be a scalar.	*
 ************************************************************************/

int mul_op ( )
{
    Matrix	a;
    Matrix	b;
    Matrix	c;
    descriptor *left;
    descriptor *right;
    descriptor *result;
    descriptor	temp;
    int		type_error;
    int		status;


    right = pop ( );
    result = top ( );
    temp = *result;
    left = &temp;


    left = CoerceData (deref (left), T_Double);
    right = CoerceData (deref (right), T_Double);


    status = 0;
    type_error = F_False;


    switch (D_Type (left)) {
    case T_Double:
	switch (D_Type (right)) {
	case T_Double:
	    CreateData (result, left, right, T_Double);
	    *D_Double (result) = *D_Double (left) * *D_Double (right);
	    break;


	case T_Matrix:
	    a = D_Matrix (right);
	    CreateData (result, left, right, T_Matrix, Mrows (a), Mcols (a));
	    b = D_Matrix (result);
	    if ((status = ScaleMatrix (b, a, *D_Double (left), 0.0)))
		MatrixError ("*", a, NULL, status, F_False);
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
	    if ((status = ScaleMatrix (b, a, *D_Double (right), 0.0)))
		MatrixError ("*", a, NULL, status, F_False);
	    break;


	case T_Matrix:
	    a = D_Matrix (left);
	    b = D_Matrix (right);
	    if (Mrows (a) == 1 && Mcols (b) == 1) {
		CreateData (result, NULL, NULL, T_Double);
		if ((status = DotBProduct (D_Double (result), a, b)))
		    MatrixError ("*", a, b, status, F_False);
	    } else {
		CreateData (result, NULL, NULL, T_Matrix, Mrows (a), Mcols (b));
		c = D_Matrix (result);
		if ((status = MultiplyMatrices (c, a, b)))
		    MatrixError ("*", a, b, status, F_False);
	    }
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
	TypeError ("*", left, right, NULL, F_False);


    RecycleData (left);
    RecycleData (right);
    d_printf ("mul ans =\n");
    d_PrintData (result);

    return type_error == F_True || status != 0;
}


/************************************************************************
 * Function:	neg_op							*
 *									*
 * Description:	Pops and negates the descriptor on the top of the stack	*
 *		and places the result on the stack.  The following	*
 *		types are legal for negation:				*
 *									*
 *		- scalar -> scalar (scalar arithmetic negation)		*
 *		- matrix -> matrix (matrix element-wise negation)	*
 *									*
 *		An attempt is first made to coerce the operand to a	*
 *		double value.						*
 ************************************************************************/

int neg_op ( )
{
    Matrix	a;
    Matrix	b;
    descriptor *op;
    descriptor *result;
    descriptor	temp;
    int		type_error;
    int		status;


    result = top ( );
    temp = *result;
    op = &temp;


    op = CoerceData (deref (op), T_Double);


    status = 0;
    type_error = F_False;


    switch (D_Type (op)) {
    case T_Double:
	CreateData (result, op, NULL, T_Double);
	*D_Double (result) = - *D_Double (op);
	break;


    case T_Matrix:
	a = D_Matrix (op);
	CreateData (result, op, NULL, T_Matrix, Mrows (a), Mcols (a));
	b = D_Matrix (result);
	if ((status = ScaleMatrix (b, a, -1.0, 0.0)))
	    MatrixError ("-", NULL, a, status, F_False);
	break;


    default:
	type_error = F_True;
	break;
    }


    if (type_error == F_True)
	TypeError ("-", NULL, op, NULL, F_False);


    RecycleData (op);
    d_printf ("neg ans =\n");
    d_PrintData (result);

    return type_error == F_True || status != 0;
}


/************************************************************************
 * Function:	not_op							*
 *									*
 * Description:	Pops and logically negates the descriptor on the top of	*
 *		the stack and places the result on the stack.  The	*
 *		following types are legal for logical negation:		*
 *									*
 *		! scalar -> scalar (scalar logical negation)		*
 *		! matrix -> matrix (matrix element-wise negation)	*
 *									*
 *		An attempt is first made to coerce the operand to a	*
 *		double value.						*
 ************************************************************************/

int not_op ( )
{
    Matrix	a;
    Matrix	b;
    descriptor *op;
    descriptor *result;
    descriptor	temp;
    int		type_error;
    unsigned	i;
    unsigned	j;


    result = top ( );
    temp = *result;
    op = &temp;


    op = CoerceData (deref (op), T_Double);
    type_error = F_False;


    switch (D_Type (op)) {
    case T_Double:
	CreateData (result, op, NULL, T_Double);
	*D_Double (result) = ! *D_Double (op);
	break;


    case T_Matrix:
	a = D_Matrix (op);
	CreateData (result, op, NULL, T_Matrix, Mrows (a), Mcols (a));
	b = D_Matrix (result);
	for (i = 1; i <= Mrows (a); i ++)
	    for (j = 1; j <= Mcols (a); j ++)
		sdata (b, i, j) = ! mdata (a, i, j);
	break;


    default:
	type_error = F_True;
	break;
    }


    if (type_error == F_True)
	TypeError ("!", NULL, op, NULL, F_False);


    RecycleData (op);
    d_printf ("not ans =\n");
    d_PrintData (result);

    return type_error == F_True;
}


/************************************************************************
 * Function:	plus_op							*
 *									*
 * Description:	Pops and computes the positive of the descriptor on the	*
 *		top of the stack and places the result on the stack.	*
 *		The following types are legal:				*
 *									*
 *		+ double -> double (identity operation)			*
 *		+ matrix -> matrix (identity operation)			*
 *									*
 *		An attempt is first made to coerce the operand to a	*
 *		double value.						*
 ************************************************************************/

int plus_op ( )
{
    Matrix	a;
    Matrix	b;
    descriptor *op;
    descriptor *result;
    descriptor	temp;
    int		type_error;
    int		status;


    result = top ( );
    temp = *result;
    op = &temp;


    op = CoerceData (deref (op), T_Double);


    status = 0;
    type_error = F_False;


    switch (D_Type (op)) {
    case T_Double:
	CreateData (result, op, NULL, T_Double);
	*D_Double (result) = *D_Double (op);
	break;


    case T_Matrix:
	a = D_Matrix (op);
	CreateData (result, op, NULL, T_Matrix, Mrows (a), Mcols (a));
	b = D_Matrix (result);
	if ((status = CopyMatrix (b, a)))
	    MatrixError ("+", NULL, a, status, F_False);
	break;


    default:
	type_error = F_True;
	break;
    }


    if (type_error == F_True)
	TypeError ("+", NULL, op, NULL, F_False);


    RecycleData (op);
    d_printf ("plus ans =\n");
    d_PrintData (result);

    return type_error == F_True || status != 0;
}


/************************************************************************
 * Function:	pow_op							*
 *									*
 * Description:	Pops and exponentiates the two descriptors on the top	*
 *		of the stack and places the result on the stack.  The	*
 *		following types are legal for exponentiation:		*
 *									*
 *		double ^ double -> double (scalar exponentation)	*
 *									*
 *		An attempt is first made to coerce the operands to	*
 *		double values.						*
 ************************************************************************/

int pow_op ( )
{
    double	lvalue;
    double	rvalue;
    descriptor *left;
    descriptor *right;
    descriptor *result;
    descriptor	temp;
    int		type_error;
    int		status;


    right = pop ( );
    result = top ( );
    temp = *result;
    left = &temp;


    left = CoerceData (deref (left), T_Double);
    right = CoerceData (deref (right), T_Double);


    status = 0;
    type_error = F_False;


    switch (D_Type (left)) {
    case T_Double:
	switch (D_Type (right)) {
	case T_Double:
	    lvalue = *D_Double (left);
	    rvalue = *D_Double (right);
	    if (lvalue < 0 && rvalue != (int) rvalue) {
		MathException ("illegal base and exponent");
		status = 1;
	    } else {
		CreateData (result, left, right, T_Double);
		*D_Double (result) = pow (lvalue, rvalue);
	    }
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
	TypeError ("^", left, right, NULL, F_False);


    RecycleData (left);
    RecycleData (right);
    d_printf ("pow ans =\n");
    d_PrintData (result);

    return type_error == F_True || status != 0;
}


/************************************************************************
 * Function:	sub_op							*
 *									*
 * Description:	Pops and subtracts the two descriptors on the top of	*
 *		the stack and places the result on the stack.  The	*
 *		following types are legal for subtraction:		*
 *									*
 *		double - double -> double (scalar subtraction)		*
 *		double - matrix -> matrix (matrix scaling)		*
 *		matrix - double -> matrix (matrix scaling)		*
 *		matrix - matrix	-> matrix (matrix subtraction)		*
 *									*
 *		An attempt is first made to coerce both operands to	*
 *		double values.						*
 ************************************************************************/

int sub_op ( )
{
    Matrix	a;
    Matrix	b;
    Matrix	c;
    descriptor *left;
    descriptor *right;
    descriptor *result;
    descriptor	temp;
    int		type_error;
    int		status;


    right = pop ( );
    result = top ( );
    temp = *result;
    left = &temp;


    left = CoerceData (deref (left), T_Double);
    right = CoerceData (deref (right), T_Double);


    status = 0;
    type_error = F_False;


    switch (D_Type (left)) {
    case T_Double:
	switch (D_Type (right)) {
	case T_Double:
	    CreateData (result, left, right, T_Double);
	    *D_Double (result) = *D_Double (left) - *D_Double (right);
	    break;


	case T_Matrix:
	    a = D_Matrix (right);
	    CreateData (result, left, right, T_Matrix, Mrows (a), Mcols (a));
	    b = D_Matrix (result);
	    if ((status = ScaleMatrix (b, a, -1.0, *D_Double (left))))
		MatrixError ("-", a, NULL, status, F_False);
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
	    if ((status = ScaleMatrix (b, a, 1.0, -*D_Double (right))))
		MatrixError ("-", a, NULL, status, F_False);
	    break;


	case T_Matrix:
	    a = D_Matrix (left);
	    b = D_Matrix (right);
	    CreateData (result, left, right, T_Matrix, Mrows (a), Mcols (a));
	    c = D_Matrix (result);
	    if ((status = SubtractMatrices (c, a, b)))
		MatrixError ("-", a, b, status, F_False);
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
	TypeError ("-", left, right, NULL, F_False);


    RecycleData (left);
    RecycleData (right);
    d_printf ("sub ans =\n");
    d_PrintData (result);

    return type_error == F_True || status != 0;
}


/************************************************************************
 * Function:	trans_op						*
 *									*
 * Description:	Pops and transposes the descriptor on the top of the	*
 *		stack and places the result on the stack.  The		*
 *		following types are legal for transposition:		*
 *									*
 *		double ' -> double (identity operation)			*
 *		matrix ' -> matrix (matrix transposition)		*
 *									*
 *		An attempt is first made to coerce the operand to a	*
 *		double value.						*
 ************************************************************************/

int trans_op ( )
{
    Matrix	a;
    Matrix	b;
    descriptor *op;
    descriptor *result;
    descriptor	temp;
    int		type_error;
    int		status;


    result = top ( );
    temp = *result;
    op = &temp;


    op = CoerceData (deref (op), T_Double);


    status = 0;
    type_error = F_False;


    switch (D_Type (op)) {
    case T_Double:
	CreateData (result, op, NULL, T_Double);
	*D_Double (result) = *D_Double (op);
	break;


    case T_Matrix:
	a = D_Matrix (op);
	CreateData (result, NULL, NULL, T_Matrix, Mcols (a), Mrows (a));
	b = D_Matrix (result);
	if ((status = TransposeMatrix (b, a)))
	    MatrixError ("'", a, NULL, status, F_False);
	break;


    default:
	type_error = F_True;
	break;
    }


    if (type_error == F_True)
	TypeError ("'", op, NULL, NULL, F_False);


    RecycleData (op);
    d_printf ("trans ans =\n");
    d_PrintData (result);

    return type_error == F_True || status != 0;
}
