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
 * File:	mathfunc.c						*
 *									*
 * Description:	This file contains the function definitions for the	*
 *		math library intrinsic functions.			*
 ************************************************************************/

# include <math.h>
# include "debug.h"
# include "coerce.h"
# include "execute.h"
# include "mathfunc.h"
# include "arithmetic.h"


/************************************************************************
 * Function:	ceil_func						*
 *									*
 * Description:	Pops and computes the ceiling of the descriptor on the	*
 *		top of the stack and places the result on the stack.	*
 *		The following types are legal:				*
 *									*
 *		ceil (double) -> double (scalar function call)		*
 *		ceil (matrix) -> matrix (matrix element-wise operation)	*
 *									*
 *		An attempt is first made to coerce the argument to a	*
 *		double value.						*
 ************************************************************************/

int ceil_func (n)
    int n;
{
    Matrix	a;
    Matrix	b;
    descriptor *arg;
    descriptor *result;
    descriptor	temp;
    int		type_error;
    unsigned	i;
    unsigned	j;


    result = top ( );
    temp = *result;
    arg = &temp;


    arg = CoerceData (deref (arg), T_Double);
    type_error = F_False;


    switch (D_Type (arg)) {
    case T_Double:
	CreateData (result, arg, NULL, T_Double);
	*D_Double (result) = ceil (*D_Double (arg));
	break;


    case T_Matrix:
	a = D_Matrix (arg);
	CreateData (result, arg, NULL, T_Matrix, Mrows (a), Mcols (a));
	b = D_Matrix (result);
	for (i = 1; i <= Mrows (a); i ++)
	    for (j = 1; j <= Mcols (a); j ++)
		sdata (b, i, j) = ceil (mdata (a, i, j));
	break;


    default:
	type_error = F_True;
	break;
    }


    if (type_error == F_True)
	TypeError ("ceil", arg, NULL, NULL, F_True);

    RecycleData (arg);
    d_printf ("ceil ans =\n");
    d_PrintData (result);

    return type_error == F_True;
}


/************************************************************************
 * Function:	cos_func						*
 *									*
 * Description:	Pops and computes the cosine of the descriptor on the	*
 *		top of the stack and places the result on the stack.	*
 *		The following types are legal:				*
 *									*
 *		cos (double) -> double (scalar function call)		*
 *		cos (matrix) -> matrix (matrix element-wise operation)	*
 *									*
 * 		An attempt is first made to coerce the argument to a	*
 *		double value.						*
 ************************************************************************/

int cos_func (n)
    int n;
{
    Matrix	a;
    Matrix	b;
    descriptor *arg;
    descriptor *result;
    descriptor	temp;
    int		type_error;
    unsigned	i;
    unsigned	j;


    result = top ( );
    temp = *result;
    arg = &temp;


    arg = CoerceData (deref (arg), T_Double);
    type_error = F_False;


    switch (D_Type (arg)) {
    case T_Double:
	CreateData (result, arg, NULL, T_Double);
	*D_Double (result) = cos (*D_Double (arg));
	break;


    case T_Matrix:
	a = D_Matrix (arg);
	CreateData (result, arg, NULL, T_Matrix, Mrows (a), Mcols (a));
	b = D_Matrix (result);
	for (i = 1; i <= Mrows (a); i ++)
	    for (j = 1; j <= Mcols (a); j ++)
		sdata (b, i, j) = cos (mdata (a, i, j));
	break;


    default:
	type_error = F_True;
	break;
    }


    if (type_error == F_True)
	TypeError ("cos", arg, NULL, NULL, F_True);

    RecycleData (arg);
    d_printf ("cos ans =\n");
    d_PrintData (result);

    return type_error == F_True;
}


/************************************************************************
 * Function:	exp_func						*
 *									*
 * Description:	Pops and computes the exponential of the descriptor on	*
 *		the top of the stack and places the result on the	*
 *		stack.  The following types are legal:			*
 *									*
 *		exp (double) -> double (scalar function call)		*
 *		exp (matrix) -> matrix (matrix element-wise operation)	*
 *									*
 *		An attempt is first made to coerce the argument to a	*
 *		a double value.						*
 ************************************************************************/

int exp_func (n)
    int n;
{
    Matrix	a;
    Matrix	b;
    descriptor *arg;
    descriptor *result;
    descriptor	temp;
    int		type_error;
    unsigned	i;
    unsigned	j;


    result = top ( );
    temp = *result;
    arg = &temp;


    arg = CoerceData (deref (arg), T_Double);
    type_error = F_False;


    switch (D_Type (arg)) {
    case T_Double:
	CreateData (result, arg, NULL, T_Double);
	*D_Double (result) = exp (*D_Double (arg));
	break;


    case T_Matrix:
	a = D_Matrix (arg);
	CreateData (result, arg, NULL, T_Matrix, Mrows (a), Mcols (a));
	b = D_Matrix (result);
	for (i = 1; i <= Mrows (a); i ++)
	    for (j = 1; j <= Mcols (a); j ++)
		sdata (b, i, j) = exp (mdata (a, i, j));
	break;


    default:
	type_error = F_True;
	break;
    }


    if (type_error == F_True)
	TypeError ("exp", arg, NULL, NULL, F_True);

    RecycleData (arg);
    d_printf ("exp ans =\n");
    d_PrintData (result);

    return type_error == F_True;
}


/************************************************************************
 * Function:	fabs_func						*
 *									*
 * Description:	Pops and computes the absolute value of the descriptor	*
 *		on the top of the stack and places the result on the	*
 *		stack.  The following types are legal:			*
 *									*
 *		fabs (double) -> double	(scalar function call)		*
 *		fabs (matrix) -> matrix (matrix element-wise operation)	*
 *									*
 *		An attempt is first made to coerce the argument to a	*
 *		double value.						*
 ************************************************************************/

int fabs_func (n)
    int n;
{
    Matrix	a;
    Matrix	b;
    descriptor *arg;
    descriptor *result;
    descriptor	temp;
    int		type_error;
    unsigned	i;
    unsigned	j;


    result = top ( );
    temp = *result;
    arg = &temp;


    arg = CoerceData (deref (arg), T_Double);
    type_error = F_False;


    switch (D_Type (arg)) {
    case T_Double:
	CreateData (result, arg, NULL, T_Double);
	*D_Double (result) = fabs (*D_Double (arg));
	break;


    case T_Matrix:
	a = D_Matrix (arg);
	CreateData (result, arg, NULL, T_Matrix, Mrows (a), Mcols (a));
	b = D_Matrix (result);
	for (i = 1; i <= Mrows (a); i ++)
	    for (j = 1; j <= Mcols (a); j ++)
		sdata (b, i, j) = fabs (mdata (a, i, j));
	break;


    default:
	type_error = F_True;
	break;
    }


    if (type_error == F_True)
	TypeError ("fabs", arg, NULL, NULL, F_True);

    RecycleData (arg);
    d_printf ("fabs ans =\n");
    d_PrintData (result);

    return type_error == F_True;
}


/************************************************************************
 * Function:	floor_func						*
 *									*
 * Description:	Pops and computes the floor of the descriptor on the	*
 *		top of the stack and places the result on the stack.	*
 *		The following types are legal:				*
 *									*
 *		floor (double) -> double (scalar function call)		*
 *		floor (matrix) -> matrix (matrix element-wise op)	*
 *									*
 *		An attempt is first made to coerce the argument to a	*
 *		double value.						*
 ************************************************************************/

int floor_func (n)
    int n;
{
    Matrix	a;
    Matrix	b;
    descriptor *arg;
    descriptor *result;
    descriptor	temp;
    int		type_error;
    unsigned	i;
    unsigned	j;


    result = top ( );
    temp = *result;
    arg = &temp;


    arg = CoerceData (deref (arg), T_Double);
    type_error = F_False;


    switch (D_Type (arg)) {
    case T_Double:
	CreateData (result, arg, NULL, T_Double);
	*D_Double (result) = floor (*D_Double (arg));
	break;


    case T_Matrix:
	a = D_Matrix (arg);
	CreateData (result, arg, NULL, T_Matrix, Mrows (a), Mcols (a));
	b = D_Matrix (result);
	for (i = 1; i <= Mrows (a); i ++)
	    for (j = 1; j <= Mcols (a); j ++)
		sdata (b, i, j) = floor (mdata (a, i, j));
	break;


    default:
	type_error = F_True;
	break;
    }


    if (type_error == F_True)
	TypeError ("floor", arg, NULL, NULL, F_True);

    RecycleData (arg);
    d_printf ("floor ans =\n");
    d_PrintData (result);

    return type_error == F_True;
}


/************************************************************************
 * Function:	fmod_func						*
 *									*
 * Description:	Pops and modulos the two descriptors on the top of the	*
 *		stack and places the result on the stack.  The		*
 *		following types are legal:				*
 *									*
 *		fmod (double, double) -> double (scalar function call)	*
 *		fmod (double, matrix) -> matrix (matrix element-wise)	*
 *		fmod (matrix, double) -> matrix (matrix element-wise)	*
 *		fmod (matrix, matrix) -> matrix (matrix element-wise)	*
 *									*
 *		An attempt is first made to coerce the arguments to	*
 *		double values.  The mod operator is used to compute the	*
 *		result, which will result in a misleading message in	*
 *		the event of an error.					*
 ************************************************************************/

int fmod_func (n)
    int n;
{
    return mod_op ( );
}


/************************************************************************
 * Function:	hypot_func						*
 *									*
 * Description:	Pops and computes the euclidean distance of the two	*
 *		descriptors on the top of the stack and places the	*
 *		result on the stack.  The following types are legal:	*
 *									*
 *		hypot (double, double) -> double (scalar function call)	*
 *		hypot (matrix, matrix) -> matrix (matrix element-wise)	*
 *									*
 *		An attempt is first made to coerce the arguments to	*
 *		double values.						*
 ************************************************************************/

int hypot_func (n)
    int n;
{
    Matrix	a;
    Matrix	b;
    Matrix	c;
    descriptor *arg1;
    descriptor *arg2;
    descriptor *result;
    descriptor	temp;
    int		type_error;
    int		status;
    unsigned	i;
    unsigned	j;


    arg2 = pop ( );
    result = top ( );
    temp = *result;
    arg1 = &temp;


    arg1 = CoerceData (deref (arg1), T_Double);
    arg2 = CoerceData (deref (arg2), T_Double);


    type_error = F_False;
    status = 0;


    switch (D_Type (arg1)) {
    case T_Double:
	switch (D_Type (arg2)) {
	case T_Double:
	    CreateData (result, arg1, arg2, T_Double);
	    *D_Double (result) = hypot (*D_Double (arg1), *D_Double (arg2));
	    break;


	case T_Matrix:
	    a = D_Matrix (arg2);
	    CreateData (result, arg1, arg2, T_Matrix, Mrows (a), Mcols (a));
	    b = D_Matrix (arg1);
	    for (i = 1; i <= Mrows (a); i ++)
		for (j = 1; j <= Mcols (a); j ++)
		    sdata (b, i, j) = hypot (*D_Double (arg1), mdata (a, i, j));
	    break;


	default:
	    type_error = F_True;
	    break;
	}
	break;


    case T_Matrix:
	switch (D_Type (arg2)) {
	case T_Double:
	    a = D_Matrix (arg1);
	    CreateData (result, arg1, arg2, T_Matrix, Mrows (a), Mcols (a));
	    b = D_Matrix (result);
	    for (i = 1; i <= Mrows (a); i ++)
		for (j = 1; j <= Mcols (a); j ++)
		    sdata (b, i, j) = hypot (mdata (a, i, j), *D_Double (arg2));
	    break;


	case T_Matrix:
	    a = D_Matrix (arg1);
	    b = D_Matrix (arg2);
	    if (Mrows (a) == Mrows (b) && Mcols (a) == Mrows (a)) {
		CreateData (result, arg1, arg2, T_Matrix, Mrows (a), Mcols (a));
		c = D_Matrix (result);
		for (i = 1; i <= Mrows (a); i ++)
		    for (j = 1; j <= Mcols (a); j ++)
			sdata (c, i, j) = hypot (mdata (a,i,j), mdata (b,i,j));
	    } else {
		MatrixError ("hypot", a, b, M_SIZEMISMATCH, F_True);
		status = 1;
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
	TypeError ("hypot", arg1, arg2, NULL, F_True);


    RecycleData (arg1);
    RecycleData (arg2);
    d_printf ("hypot ans=\n");
    d_PrintData (result);

    return type_error == F_True || status != 0;
}


/************************************************************************
 * Function:	log_func						*
 *									*
 * Description:	Pops and computes the natural logarithm of the		*
 *		descriptor on the top of the stack and places the	*
 *		result on the stack.  The following types are legal:	*
 *									*
 *		log (double) -> double (scalar function call)		*
 *		log (matrix) -> matrix (matrix element-wise operation)	*
 *									*
 *		An attempt is first made to coerce the argument to a	*
 *		double value.						*
 ************************************************************************/

int log_func (n)
    int n;
{
    Matrix	a;
    Matrix	b;
    double	value;
    descriptor *arg;
    descriptor *result;
    descriptor	temp;
    int		type_error;
    int		status;
    unsigned	i;
    unsigned	j;


    result = top ( );
    temp = *result;
    arg = &temp;


    arg = CoerceData (deref (arg), T_Double);
    type_error = F_False;
    status = 0;


    switch (D_Type (arg)) {
    case T_Double:
	if ((value = *D_Double (arg)) <= 0.0) {
	    MathException ("logatithm of non-positive number");
	    status = 1;
	} else {
	    CreateData (result, arg, NULL, T_Double);
	    *D_Double (result) = log (*D_Double (arg));
	}
	break;


    case T_Matrix:
	a = D_Matrix (arg);
	CreateData (result, arg, NULL, T_Matrix, Mrows (a), Mcols (a));
	b = D_Matrix (result);
	for (i = 1; i <= Mrows (a); i ++)
	    for (j = 1; j <= Mcols (a); j ++)
		if ((value = mdata (a, i, j)) < 0.0) {
		    MathException ("logarithm of non-positive number");
		    status = 1;
		} else
		    sdata (b, i, j) = log (mdata (a, i, j));
	break;


    default:
	type_error = F_True;
	break;
    }


    if (type_error == F_True)
	TypeError ("log", arg, NULL, NULL, F_True);

    RecycleData (arg);
    d_printf ("log ans =\n");
    d_PrintData (result);

    return type_error == F_True || status != 0;
}


/************************************************************************
 * Function:	log10_func						*
 *									*
 * Description:	Pops and computes the base-10 logarithm of the		*
 *		descriptor on the top of the stack and places the	*
 *		result on the stack.  The following types are legal:	*
 *									*
 *		log10 (double) -> double (scalar function call)		*
 *		log10 (matrix) -> matrix (matrix element-wise op)	*
 *									*
 *		An attempt is first made to coerce the argument to a	*
 *		double value.						*
 ************************************************************************/

int log10_func (n)
    int n;
{
    Matrix	a;
    Matrix	b;
    double	value;
    descriptor *arg;
    descriptor *result;
    descriptor	temp;
    int		type_error;
    int		status;
    unsigned	i;
    unsigned	j;


    result = top ( );
    temp = *result;
    arg = &temp;


    arg = CoerceData (deref (arg), T_Double);
    type_error = F_False;
    status = 0;


    switch (D_Type (arg)) {
    case T_Double:
	if ((value = *D_Double (arg)) <= 0.0) {
	    MathException ("logatithm of non-positive number");
	    status = 1;
	} else {
	    CreateData (result, arg, NULL, T_Double);
	    *D_Double (result) = log10 (*D_Double (arg));
	}
	break;


    case T_Matrix:
	a = D_Matrix (arg);
	CreateData (result, arg, NULL, T_Matrix, Mrows (a), Mcols (a));
	b = D_Matrix (result);
	for (i = 1; i <= Mrows (a); i ++)
	    for (j = 1; j <= Mcols (a); j ++)
		if ((value = mdata (a, i, j)) < 0.0) {
		    MathException ("logarithm of non-positive number");
		    status = 1;
		} else
		    sdata (b, i, j) = log10 (mdata (a, i, j));
	break;


    default:
	type_error = F_True;
	break;
    }


    if (type_error == F_True)
	TypeError ("log10", arg, NULL, NULL, F_True);

    RecycleData (arg);
    d_printf ("log10 ans =\n");
    d_PrintData (result);

    return type_error == F_True || status != 0;
}


/************************************************************************
 * Function:	pow_func						*
 *									*
 * Description:	Pops and exponentiates the two descriptors on the top	*
 *		of the stack and places the result on the stack.  The	*
 *		following types are legal:				*
 *									*
 *		pow (double, double) -> double (scalar function call)	*
 *									*
 *		An attempt is first made to coerce the arguments to	*
 *		double values.  The pow operator is used to compute the	*
 *		result, which will result in a misleading message in	*
 *		the event of an error.					*
 ************************************************************************/

int pow_func (n)
    int n;
{
    return pow_op ( );
}


/************************************************************************
 * Function:	sin_func						*
 *									*
 * Description:	Pops and computes the sine of the descriptor on the top	*
 *		of the stack and places the result on the stack.  The	*
 *		following types are legal:				*
 *									*
 *		sin (double) -> double (scalar function call)		*
 *		sin (matrix) -> matrix (matrix element-wise operation)	*
 *									*
 *		An attempt is first made to coerce the argument to a	*
 *		double value.						*
 ************************************************************************/

int sin_func (n)
    int n;
{
    Matrix	a;
    Matrix	b;
    descriptor *arg;
    descriptor *result;
    descriptor	temp;
    int		type_error;
    unsigned	i;
    unsigned	j;


    result = top ( );
    temp = *result;
    arg = &temp;


    arg = CoerceData (deref (arg), T_Double);
    type_error = F_False;


    switch (D_Type (arg)) {
    case T_Double:
	CreateData (result, arg, NULL, T_Double);
	*D_Double (result) = sin (*D_Double (arg));
	break;


    case T_Matrix:
	a = D_Matrix (arg);
	CreateData (result, arg, NULL, T_Matrix, Mrows (a), Mcols (a));
	b = D_Matrix (result);
	for (i = 1; i <= Mrows (a); i ++)
	    for (j = 1; j <= Mcols (a); j ++)
		sdata (b, i, j) = sin (mdata (a, i, j));
	break;


    default:
	type_error = F_True;
	break;
    }


    if (type_error == F_True)
	TypeError ("sin", arg, NULL, NULL, F_True);

    RecycleData (arg);
    d_printf ("sin ans =\n");
    d_PrintData (result);

    return type_error == F_True;
}


/************************************************************************
 * Function:	sqrt_func						*
 *									*
 * Description:	Pops and computes the square root of the descriptor on	*
 *		the top of the stack and places the result on the	*
 *		stack.  The following types are legal:			*
 *									*
 *		sqrt (double) -> double (scalar function call)		*
 *		sqrt (matrix) -> matrix (matrix element-wise operation)	*
 *									*
 *		An attempt is first made to coerce the argument to a	*
 *		double value.  A cholesky factorization can be used to	*
 *		compute a true square root for matrices.		*
 ************************************************************************/

int sqrt_func (n)
    int n;
{
    Matrix	a;
    Matrix	b;
    double	value;
    descriptor *arg;
    descriptor *result;
    descriptor	temp;
    int		type_error;
    int		status;
    unsigned	i;
    unsigned	j;


    result = top ( );
    temp = *result;
    arg = &temp;


    arg = CoerceData (deref (arg), T_Double);
    type_error = F_False;
    status = 0;


    switch (D_Type (arg)) {
    case T_Double:
	if ((value = *D_Double (arg)) < 0.0) {
	    MathException ("square root of negative number");
	    status = 1;
	} else {
	    CreateData (result, arg, NULL, T_Double);
	    *D_Double (result) = sqrt (*D_Double (arg));
	}
	break;


    case T_Matrix:
	a = D_Matrix (arg);
	CreateData (result, arg, NULL, T_Matrix, Mrows (a), Mcols (a));
	b = D_Matrix (result);
	for (i = 1; i <= Mrows (a); i ++)
	    for (j = 1; j <= Mcols (a); j ++)
		if ((value = mdata (a, i, j)) < 0.0) {
		    MathException ("square root of negative number");
		    status = 1;
		} else
		    sdata (b, i, j) = sqrt (mdata (a, i, j));
	break;


    default:
	type_error = F_True;
	break;
    }


    if (type_error == F_True)
	TypeError ("sqrt", arg, NULL, NULL, F_True);

    RecycleData (arg);
    d_printf ("sqrt ans =\n");
    d_PrintData (result);

    return type_error == F_True || status != 0;
}


/************************************************************************
 * Function:	tan_func						*
 *									*
 * Description:	Pops and computes the tangent of the descriptor on the	*
 *		top of the stack and places the result on the stack.	*
 *		The following types are legal:				*
 *									*
 *		tan (double) -> double (scalar function call)		*
 *		tan (matrix) -> matrix (matrix element-wise operation)	*
 *									*
 *		An attempt is first made to coerce the argument to a	*
 *		double value.						*
 ************************************************************************/

int tan_func (n)
    int n;
{
    Matrix	a;
    Matrix	b;
    descriptor *arg;
    descriptor *result;
    descriptor	temp;
    int		type_error;
    unsigned	i;
    unsigned	j;


    result = top ( );
    temp = *result;
    arg = &temp;


    arg = CoerceData (deref (arg), T_Double);
    type_error = F_False;


    switch (D_Type (arg)) {
    case T_Double:
	CreateData (result, arg, NULL, T_Double);
	*D_Double (result) = tan (*D_Double (arg));
	break;


    case T_Matrix:
	a = D_Matrix (arg);
	CreateData (result, arg, NULL, T_Matrix, Mrows (a), Mcols (a));
	b = D_Matrix (result);
	for (i = 1; i <= Mrows (a); i ++)
	    for (j = 1; j <= Mcols (a); j ++)
		sdata (b, i, j) = tan (mdata (a, i, j));
	break;


    default:
	type_error = F_True;
	break;
    }


    if (type_error == F_True)
	TypeError ("tan", arg, NULL, NULL, F_True);

    RecycleData (arg);
    d_printf ("tan ans =\n");
    d_PrintData (result);

    return type_error == F_True;
}
