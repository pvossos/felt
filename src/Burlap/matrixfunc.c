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
 * File:	matrixfunc.c						*
 *									*
 * Description:	This file contains the function definitions for the	*
 *		matrix-related intrinsic functions.			*
 ************************************************************************/

# include <math.h>
# include <time.h>
# include "debug.h"
# include "error.h"
# include "coerce.h"
# include "execute.h"
# include "matrixfunc.h"
# include "our-stdlib.h"

# ifdef DOS
# define srand48 srand
# define drand48 rand
# endif

/************************************************************************
 * Function:	chol_func						*
 *									*
 * Description:	Pops and computes the cholesky factorization of the	*
 *		descriptor on the top of the stack and places the	*
 *		result on the stack.  The following types are legal:	*
 *									*
 *		chol (double) -> double (scalar square root)		*
 *		chol (matrix) -> matrix (cholesky factorization)	*
 *									*
 *		An attempt is first made to coerce the argument to a	*
 *		double value.						*
 ************************************************************************/

int chol_func (n)
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


    result = top ( );
    temp = *result;
    arg = &temp;


    arg = CoerceData (deref (arg), T_Double);


    status = 0;
    type_error = F_False;


    switch (D_Type (arg)) {
    case T_Double:
	if ((value = *D_Double (arg)) < 0.0) {
	    MathException ("cholesky factorization of negative number");
	    status = 1;
	} else {
	    CreateData (result, arg, NULL, T_Double);
	    *D_Double (result) = sqrt (value);
	}
	break;


    case T_Matrix:
	a = D_Matrix (arg);
	CreateData (result, arg, NULL, T_Matrix, Mrows (a), Mcols (a));
	b = D_Matrix (result);
	if ((status = CholeskyFactorMatrix (b, a)))
	    MatrixError ("chol", a, NULL, status, F_True);
	break;


    default:
	type_error = F_True;
	break;
    }


    if (type_error == F_True)
	TypeError ("chol", arg, NULL, NULL, F_True);


    RecycleData (arg);
    d_printf ("chol ans =\n");
    d_PrintData (result);

    return type_error == F_True || status != 0;
}


/************************************************************************
 * Function:	cols_func						*
 *									*
 * Description:	Pops the descriptor on the top of the stack and pushes	*
 *		its number of columns on the stack.  The following	*
 *		types are legal:					*
 *									*
 *		cols (double) -> double (always 1)			*
 *		cols (matrix) -> double (number of columns)		*
 *									*
 *		An attempt is first made to coerce the argument to a	*
 *		double value.						*
 ************************************************************************/

int cols_func (n)
    int n;
{
    descriptor *arg;
    descriptor *result;
    descriptor	temp;
    int		type_error;


    result = top ( );
    temp = *result;
    arg = &temp;


    arg = CoerceData (deref (arg), T_Double);
    type_error = F_False;


    switch (D_Type (arg)) {
    case T_Double:
	D_Type    (result) = T_Double;
	D_Temp    (result) = F_False;
	D_Trapped (result) = F_False;
	D_Double  (result) = dbllit (1);
	break;


    case T_Matrix:
	CreateData (result, NULL, NULL, T_Double);
	*D_Double (result) = Mcols (D_Matrix (arg));
	break;


    default:
	type_error = F_True;
	break;
    }


    if (type_error == F_True)
	TypeError ("cols", arg, NULL, NULL, F_True);


    RecycleData (arg);
    d_printf ("cols ans =\n");
    d_PrintData (result);

    return type_error == F_True;
}


/************************************************************************
 * Function:	compact_func						*
 *									*
 * Description:	Pops and compacts the descriptor on the top of the	*
 *		stack and pushes the result on the stack.  The		*
 *		following types are legal:				*
 *									*
 *		compact (double) -> double (identity operation)		*
 *		compact (matrix) -> matrix (matrix compaction)		*
 *									*
 *		An attempt is first made to coerce the argument to a	*
 *		double value.						*
 ************************************************************************/

int compact_func (n)
    int n;
{
    Matrix	a;
    Matrix	b;
    descriptor *arg;
    descriptor *result;
    descriptor	temp;
    int		type_error;
    int		status;


    result = top ( );
    temp = *result;
    arg = &temp;


    status = 0;
    type_error = F_False;
    arg = CoerceData (deref (arg), T_Double);


    switch (D_Type (arg)) {
    case T_Double:
	CreateData (result, arg, NULL, T_Double);
	*D_Double (result) = *D_Double (arg);
	break;


    case T_Matrix:
	a = D_Matrix (arg);
	if (IsCompact (a)) {
	    CreateData (result, NULL, NULL, T_Matrix, Mrows (a), Mcols (a));
	    CopyMatrix (D_Matrix (result), a);
	} else if (IsSymmetricMatrix (a)) {
	    b = MakeCompactFromFull (a);
	    D_Type    (result) = T_Matrix;
	    D_Temp    (result) = F_True;
	    D_Trapped (result) = F_False;
	    D_Matrix  (result) = b;
	} else {
	    status = M_NOTSYMMETRIC;
	    MatrixError ("compact", a, NULL, status, F_True);
	}
	break;


    default:
	type_error = F_True;
	break;
    }


    if (type_error)
	TypeError ("compact", arg, NULL, NULL, F_True);


    RecycleData (arg);
    d_printf ("compact ans =\n");
    d_PrintData (result);

    return type_error == F_True || status != 0;
}


/************************************************************************
 * Function:	det_func						*
 *									*
 * Description:	Pops and computes the determinant of the descriptor on	*
 *		the top of the stack and places the result on the	*
 *		stack.  The following types are legal:			*
 *									*
 *		det (double) -> double (identity operation)		*
 *		det (matrix) -> matrix (matrix determinant)		*
 *									*
 *		An attempt is first made to coerce the argument to a	*
 *		double value.						*
 ************************************************************************/

int det_func (n)
    int n;
{
    Matrix	a;
    Matrix	b;
    Matrix	p;
    descriptor *arg;
    descriptor *result;
    descriptor	temp;
    int		type_error;
    int		status;
    int		info;


    result = top ( );
    temp = *result;
    arg = &temp;


    arg = CoerceData (deref (arg), T_Double);
    type_error = F_False;
    status = 0;


    switch (D_Type (arg)) {
    case T_Double:
	CreateData (result, arg, NULL, T_Double);
	*D_Double (result) = *D_Double (arg);
	break;


    case T_Matrix:
	a = D_Matrix (arg);
	p = CreateColumnVector (Mrows (a));
	b = D_Writable (arg) ? a : CreateFullMatrix (Mrows (a), Mcols (a));

	if ((status = LUFactorMatrix (b, a, p, &info)))
	    MatrixError ("det", a, NULL, status, F_True);
	else if (info) {
	    MathException ("singular matrix in det() function");
	    status = 1;
	} else {
	    CreateData (result, NULL, NULL, T_Double);
	    if ((status = DeterminantMatrix (D_Double (result), b, p)))
		MatrixError ("det", b, NULL, status, F_True);
	}

	DestroyMatrix (p);
	if (a != b)
	    DestroyMatrix (b);
	break;


    default:
	type_error = F_True;
	break;
    }


    if (type_error == F_True)
	TypeError ("det", arg, NULL, NULL, F_True);


    RecycleData (arg);
    d_printf ("det ans =\n");
    d_PrintData (result);

    return type_error == F_True || status != 0;
}


/************************************************************************
 * Function:	eig_func						*
 *									*
 * Description:	Pops and computes the eigenvalue decomposition of the	*
 *		descriptor on the top of the stack and places the	*
 *		result on the stack.  The result is the vector of	*
 *		eigenvalues.  The matrix of eigenvectors of a symmetric	*
 *		matrix may be obtained by specifying it as the		*
 *		remaining parameter.  The following types are legal:	*
 *									*
 *		eig (double, ...) -> double (identity operation)	*
 *		eig (matrix, ...) -> matrix (eigenvalue decomposition)	*
 *									*
 *		An attempt is first made to coerce the first argument	*
 *		to a double value.					*
 ************************************************************************/

int eig_func (n)
    int n;
{
    Matrix	a;
    Matrix	l;
    Matrix	v;
    descriptor *arg1;
    descriptor *arg2;
    descriptor *result;
    descriptor	temp;
    int		type_error;
    int		status;


    arg2 = pop ( );
    result = top ( );
    temp = *result;
    arg1 = &temp;


    status = 0;
    type_error = F_False;
    arg1 = CoerceData (deref (arg1), T_Double);


    if (!assignable (arg2)) {
	RecycleData (arg2);
	arg2 = NULL;
    } else
	arg2 = deref (arg2);


    switch (D_Type (arg1)) {
    case T_Double:
	CreateData (result, arg1, NULL, T_Double);
	*D_Double (result) = *D_Double (arg1);

	if (arg2)
	    status = AssignObject (arg2, T_Double, F_False, dbllit (1));
	break;


    case T_Matrix:
	a = D_Matrix (arg1);
	CreateData (result, NULL, NULL, T_Matrix, Mrows (a), 1);
	l = D_Matrix (result);

	if (IsSymmetricMatrix (a)) {
	    v = CreateFullMatrix (Mrows (a), Mcols (a));

	    if ((status = SymmetricMatrixEigenModes (a, l, v, 0.0))) {
		MatrixError ("eig", a, NULL, status, F_True);
		DestroyMatrix (v);

	    } else if (!arg2)
		DestroyMatrix (v);
	    else if ((status = AssignObject (arg2, T_Matrix, F_True, v)))
		DestroyMatrix (v);

	} else if ((status = GeneralMatrixEigenModes (a, l, 0.0, 0.0)))
	    MatrixError ("eig", a, NULL, status, F_True);
	break;


    default:
	type_error = F_True;
	break;
    }


    if (type_error == F_True)
	TypeError ("eig", arg1, NULL, NULL, F_True);


    RecycleData (arg1);
    d_printf ("eig ans =\n");
    d_PrintData (result);

    return type_error == F_True || status != 0;
}


/************************************************************************
 * Function:	eye_func						*
 *									*
 * Description:	Pops two descriptors from the top of the stack and	*
 *		creates and pushes on the stack an identity matrix.	*
 *		The first descriptor specifies the number of rows and	*
 *		the second descriptor specifies the number of columns.	*
 *		The following types are legal:				*
 *									*
 *		eye (double, null)   -> matrix (matrix creation)	*
 *		eye (double, double) -> matrix (matrix creation)	*
 *									*
 *		An attempt is first made to coerce both arguments to	*
 *		double values.  The default value for the second	*
 *		argument is the value of the first argument.  If the	*
 *		resulting matrix consists of only a single scalar then	*
 *		the result will be a double scalar.			*
 ************************************************************************/

int eye_func (n)
    int n;
{
    Matrix	a;
    double	val1;
    double	val2;
    descriptor *arg1;
    descriptor *arg2;
    descriptor *result;
    descriptor *old;
    descriptor	temp;
    int		type_error;
    int		status;


    arg2 = pop ( );
    result = top ( );
    temp = *result;
    arg1 = &temp;


    arg1 = CoerceData (deref (arg1), T_Double);
    arg2 = CoerceData (deref (arg2), T_Double);


    status = 0;
    type_error = F_False;


    if (D_Type (arg2) == T_Null) {
	RecycleData (arg2);
	arg2 = arg1;
	old = NULL;
    } else
	old = arg2;


    if (D_Type (arg1) == T_Double && D_Type (arg2) == T_Double) {
	val1 = *D_Double (arg1);
	val2 = *D_Double (arg2);
	if (val1 < 1) {
	    rterror ("non-positive number of rows in eye() function");
	    status = 1;
	} else if (val2 < 1) {
	    rterror ("non-positive number of columns in eye() function");
	    status = 1;
	} else if ((int) val1 == 1 && (int) val2 == 1) {
	    D_Type    (result) = T_Double;
	    D_Temp    (result) = F_False;
	    D_Trapped (result) = F_False;
	    D_Double  (result) = dbllit (1);
	} else {
	    CreateData (result, NULL, NULL, T_Matrix, (int) val1, (int) val2);
	    a = D_Matrix (result);
	    if ((status = IdentityMatrix (a)))
		MatrixError ("eye", a, NULL, status, F_True);
	}
    } else
	type_error = F_True;


    if (type_error == F_True)
	TypeError ("eye", arg1, old, NULL, F_True);


    RecycleData (arg1);
    RecycleData (old);
    d_printf ("eye ans =\n");
    d_PrintData (result);

    return type_error == F_True || status != 0;
}


/************************************************************************
 * Function:	inv_func						*
 *									*
 * Description:	Pops and computes the inverse of the descriptor on the	*
 *		top of the stack and places the result on the stack.	*
 *		The following types are legal:				*
 *									*
 *		inv (double) -> double (scalar inversion)		*
 *		inv (matrix) -> matrix (matrix inversion)		*
 *									*
 *		An attempt is first made to coerce the argument to a	*
 *		double value.						*
 ************************************************************************/

int inv_func (n)
    int n;
{
    Matrix	a;
    Matrix	b;
    Matrix	p;
    Matrix	c;
    double	value;
    descriptor *arg;
    descriptor *result;
    descriptor	temp;
    int		type_error;
    int		status;
    int		info;


    result = top ( );
    temp = *result;
    arg = &temp;


    arg = CoerceData (deref (arg), T_Double);
    type_error = F_False;
    status = 0;


    switch (D_Type (arg)) {
    case T_Double:
	if ((value = *D_Double (arg)) == 0.0) {
	    MathException ("inverse of zero in inv() function");
	    status = 1;
	} else {
	    CreateData (result, arg, NULL, T_Double);
	    *D_Double (result) = 1.0 / *D_Double (arg);
	}
	break;


    case T_Matrix:
	a = D_Matrix (arg);
	CreateData (result, arg, NULL, T_Matrix, Mrows (a), Mcols (a));
	b = D_Matrix (result);
	c = CreateFullMatrix (Mrows (a), Mcols (a));
	p = CreateColumnVector (Mrows (a));

	if ((status = LUFactorMatrix (c, a, p, &info)))
	    MatrixError ("inv", a, NULL, status, F_True);
	else if (info)
	    MathException ("singular matrix in inv() function"), status = 1;
	else if ((status = InvertMatrix (b, c, p)))
	    MatrixError ("inv", a, NULL, status, F_True);

	DestroyMatrix (c);
	DestroyMatrix (p);
	break;


    default:
	type_error = F_True;
	break;
    }


    if (type_error == F_True)
	TypeError ("inv", arg, NULL, NULL, F_True);


    RecycleData (arg);
    d_printf ("inv ans =\n");
    d_PrintData (result);

    return type_error == F_True || status != 0;
}


/************************************************************************
 * Function:	lu_func							*
 *									*
 * Description:	Pops and computes the LU factorization of the		*
 *		descriptor on the top of the stack and places the	*
 *		result on the stack.  The result is a row permuted	*
 *		superposition of L and U, with the diagonal of L not	*
 *		being stored since it is always ones.  The individual	*
 *		L, U, and permutation matrices can be retrieved by	*
 *		specifying them as the remaining parameters.  The	*
 *		following types are legal:				*
 *									*
 *		lu (double, ...) -> double (identity operation)		*
 *		lu (matrix, ...) -> matrix (LU factorization)		*
 *									*
 *		An attempt is first made to coerce the first argument	*
 *		to a double value.					*
 ************************************************************************/

int lu_func (n)
    int n;
{
    Matrix	a;
    Matrix	b;
    Matrix	p;
    Matrix	L;
    Matrix	U;
    Matrix	P;
    descriptor *arg1;
    descriptor *arg2;
    descriptor *arg3;
    descriptor *arg4;
    descriptor *result;
    descriptor	scratch;
    descriptor	temp;
    int		type_error;
    int		status;
    int		info;


    arg4 = pop ( );
    arg3 = pop ( );
    arg2 = pop ( );
    result = top ( );
    temp = *result;
    arg1 = &temp;


    status = 0;
    type_error = F_False;
    arg1 = CoerceData (deref (arg1), T_Double);


    if (!assignable (arg2)) {
	RecycleData (arg2);
	arg2 = NULL;
    } else
	arg2 = deref (arg2);

    if (!assignable (arg3)) {
	RecycleData (arg3);
	arg3 = NULL;
    } else
	arg3 = deref (arg3);

    if (!assignable (arg4)) {
	RecycleData (arg4);
	arg4 = NULL;
    } else
	arg4 = deref (arg4);


    switch (D_Type (arg1)) {
    case T_Double:
	CreateData (result, arg1, NULL, T_Double);
	*D_Double (result) = *D_Double (arg1);
	if (arg2)
	    status = AssignObject (arg2, T_Double, F_False, dbllit (1));

	if (arg3) {
	    CreateData (&scratch, NULL, NULL, T_Double);
	    *D_Double (&scratch) = *D_Double (arg1);
	    status = AssignObject (arg3, T_Double, F_True, D_Double (&scratch));
	    if (status)
		FreeData (&scratch);
	}

	if (arg4)
	    status = AssignObject (arg4, T_Double, F_False, dbllit (1));
	break;


    case T_Matrix:
	a = D_Matrix (arg1);
	CreateData (result, arg1, NULL, T_Matrix, Mrows (a), Mcols (a));
	b = D_Matrix (result);
	p = CreateColumnVector (Mrows (a));

	if ((status = LUFactorMatrix (b, a, p, &info)))
	    MatrixError ("lu", a, NULL, status, F_True);
	else if (info)
	    MathException ("singular matrix in lu() function"), status = 1;
	else if (arg2 || arg3 || arg4) {
	    L = CreateFullMatrix (Mrows (a), Mcols (a));
	    U = CreateFullMatrix (Mrows (a), Mcols (a));
	    P = CreateFullMatrix (Mrows (a), Mcols (a));

	    if ((status = FormLUPMatrices (L, U, P, b, p)))
		MatrixError ("lu", a, NULL, status, F_True);

	    if (!arg2 || (status = AssignObject (arg2, T_Matrix, F_True, L)))
		DestroyMatrix (L);

	    if (!arg3 || (status = AssignObject (arg3, T_Matrix, F_True, U)))
		DestroyMatrix (U);

	    if (!arg4 || (status = AssignObject (arg4, T_Matrix, F_True, P)))
		DestroyMatrix (P);
	}

	DestroyMatrix (p);
	break;


    default:
	type_error = F_True;
	break;
    }


    if (type_error == F_True)
	TypeError ("lu", arg1, NULL, NULL, F_True);


    RecycleData (arg1);
    d_printf ("lu ans =\n");
    d_PrintData (result);

    return type_error == F_True || status != 0;
}


/************************************************************************
 * Function:	norm_func						*
 *									*
 * Description:	Pops two descriptors from the top of the stack and	*
 *		places the norm of the first on the stack.  The second	*
 *		descriptor specifies the type of norm.  The following	*
 *		types are legal:					*
 *									*
 *		norm (double, string) -> double (absolute value)	*
 *		norm (matrix, string) -> matrix (matrix or vector norm)	*
 *									*
 *		An attempt is first made to coerce the first argument	*
 *		to a double value and the second argument to a string	*
 *		value.  The value of the result is based on the values	*
 *		of the arguments as follows:				*
 *									*
 *			null		"1"	"2"	"fro"		*
 *		scalar	fabs		fabs	fabs	fabs		*
 *		vector	2-norm		1-norm	2-norm	frobenius-norm	*
 *		matrix	frobenius-norm	1-norm	error	frobenius-norm	*
 *									*
 *		The frobenius-norm of a vector is identical to the	*
 *		2-norm of the vector.					*
 ************************************************************************/

int norm_func (n)
    int n;
{
    Matrix	a;
    Matrix	b;
    char       *string;
    descriptor *arg1;
    descriptor *arg2;
    descriptor *result;
    descriptor	temp;
    int		type_error;
    int		status;
    int		norm;
    unsigned	i;


    arg2 = pop ( );
    result = top ( );
    temp = *result;
    arg1 = &temp;


    arg1 = CoerceData (deref (arg1), T_Double);
    arg2 = CoerceData (deref (arg2), T_String);


    norm = 0;
    status = 0;
    type_error = F_False;


    if (D_Type (arg2) != T_Null && D_Type (arg2) != T_String)
	type_error = F_True;
    else if (D_Type (arg2) == T_String) {
	string = *D_String (arg2);
	if (!strcmp (string, "1"))
	    norm = 1;
	else if (!strcmp (string, "2"))
	    norm = 2;
	else if (!strcmp (string, "inf"))
	    norm = 'i';
	else if (!strcmp (string, "fro"))
	    norm = 'f';
	else {
	    rterror ("illegal norm type in norm() function: %s", string);
	    status = 1;
	}
    }


    if (type_error == F_False && status == 0)
	switch (D_Type (arg1)) {
	case T_Double:
	    CreateData (result, arg1, NULL, T_Double);
	    *D_Double (result) = fabs (*D_Double (arg1));
	    break;


	case T_Matrix:
	    a = D_Matrix (arg1);
	    CreateData (result, NULL, NULL, T_Double);
	    if (Mrows (a) == 1) {
		b = CreateColumnVector (Mcols (a));
		for (i = 1; i <= Mcols (a); i ++)
		    sdata (b, i, 1) = mdata (a, 1, i);

		if (norm == 1) {
		    if ((status = PNormVector (D_Double (result), b, "1")))
			MatrixError ("norm", a, NULL, status, F_True);
		} else if (norm == 2 || norm == 0) {
		    if ((status = PNormVector (D_Double (result), b, "2")))
			MatrixError ("norm", a, NULL, status, F_True);
		} else if (norm == 'i') {
		    if ((status = PNormVector (D_Double (result), b, "inf")))
			MatrixError ("norm", a, NULL, status, F_True);
		} else if (norm == 'f') {
		    if ((status = FrobeniusNormMatrix (D_Double (result), b)))
			MatrixError ("norm", a, NULL, status, F_True);
		}

		DestroyMatrix (b);

	    } else if (Mcols (a) == 1) {
		if (norm == 1) {
		    if ((status = PNormVector (D_Double (result), a, "1")))
			MatrixError ("norm", a, NULL, status, F_True);
		} else if (norm == 2 || norm == 0) {
		    if ((status = PNormVector (D_Double (result), a, "2")))
			MatrixError ("norm", a, NULL, status, F_True);
		} else if (norm == 'i') {
		    if ((status = PNormVector (D_Double (result), a, "inf")))
			MatrixError ("norm", a, NULL, status, F_True);
		} else if (norm == 'f') {
		    if ((status = FrobeniusNormMatrix (D_Double (result), a)))
			MatrixError ("norm", a, NULL, status, F_True);
		}

	    } else {
		if (norm == 2) {
		    rterror ("2-norm of matrix undefined in norm() function");
		    status = 1;
		} else if (norm == 1) {
		    if ((status = PNormMatrix (D_Double (result), a, "1")))
			MatrixError ("norm", a, NULL, status, F_True);
		} else if (norm == 'i') {
		    if ((status = PNormMatrix (D_Double (result), a, "inf")))
			MatrixError ("norm", a, NULL, status, F_True);
		} else if (norm == 'f' || norm == 0) {
		    if ((status = FrobeniusNormMatrix (D_Double (result), a)))
			MatrixError ("norm", a, NULL, status, F_True);
		}
	    }
	    break;


	default:
	    type_error = F_True;
	    break;
	}


    if (type_error == F_True)
	TypeError ("norm", arg1, arg2, NULL, F_True);


    RecycleData (arg1);
    RecycleData (arg2);
    d_printf ("norm ans=\n");
    d_PrintData (result);

    return type_error == F_True || status != 0;
}


/************************************************************************
 * Function:	ones_func						*
 *									*
 * Description:	Pops two descriptors from the top of the stack and	*
 *		creates and pushes on the stack a matrix of ones.  The	*
 *		first descriptor specifies the number of rows and the	*
 *		second descriptor specifies the number of columns.  The	*
 *		following types are legal:				*
 *									*
 *		ones (double, null)   -> matrix (matrix creation)	*
 *		ones (double, double) -> matrix (matrix creation)	*
 *									*
 *		An attempt is first made to coerce the arguments to	*
 *		double values.  The default value for the second	*
 *		argument is the value of the first argument.  If the	*
 *		resulting matrix consists of only a single scalar then	*
 *		the result will be a double scalar.			*
 ************************************************************************/

int ones_func (n)
    int n;
{
    Matrix	a;
    double	val1;
    double	val2;
    descriptor *arg1;
    descriptor *arg2;
    descriptor *result;
    descriptor *old;
    descriptor	temp;
    int		type_error;
    int		status;


    arg2 = pop ( );
    result = top ( );
    temp = *result;
    arg1 = &temp;


    arg1 = CoerceData (deref (arg1), T_Double);
    arg2 = CoerceData (deref (arg2), T_Double);


    status = 0;
    type_error = F_False;


    if (D_Type (arg2) == T_Null) {
	RecycleData (arg2);
	arg2 = arg1;
	old = NULL;
    } else
	old = arg2;


    if (D_Type (arg1) == T_Double && D_Type (arg2) == T_Double) {
	val1 = *D_Double (arg1);
	val2 = *D_Double (arg2);
	if (val1 < 1) {
	    rterror ("non-positive number of rows in ones() function");
	    status = 1;
	} else if (val2 < 1) {
	    rterror ("non-positive number of columns in ones() function");
	    status = 1;
	} else if ((int) val1 == 1 && (int) val2 == 1) {
	    D_Type    (result) = T_Double;
	    D_Temp    (result) = F_False;
	    D_Trapped (result) = F_False;
	    D_Double  (result) = dbllit (0);
	} else {
	    CreateData (result, NULL, NULL, T_Matrix, (int) val1, (int) val2);
	    a = D_Matrix (result);
	    if ((status = ScaleMatrix (a, a, 0.0, 1.0)))
		MatrixError ("ones", a, NULL, status, F_True);
	}
    } else
	type_error = F_True;


    if (type_error == F_True)
	TypeError ("ones", arg1, old, NULL, F_True);


    RecycleData (arg1);
    RecycleData (old);
    d_printf ("ones ans =\n");
    d_PrintData (result);

    return type_error == F_True || status != 0;
}


/************************************************************************
 * Function:	qr_func							*
 *									*
 * Description:	Pops and computes the QR factorization of the		*
 *		descriptor on the top of the stack and places the	*
 *		result on the stack.  The result is the R matrix.  The	*
 *		Q and R matrices may be obtained by specifying them as	*
 *		the remaining parameters.  The following types are	*
 *		legal:							*
 *									*
 *		qr (double, ...) -> double (identity operation)		*
 *		qr (matrix, ...) -> matrix (QR factorization)		*
 *									*
 *		An attempt is first made to coerce the first argument	*
 *		to a double value.					*
 ************************************************************************/

int qr_func (n)
    int n;
{
    Matrix	a;
    Matrix	b;
    Matrix	q;
    Matrix	r;
    descriptor *arg1;
    descriptor *arg2;
    descriptor *arg3;
    descriptor *result;
    descriptor	scratch;
    descriptor	temp;
    int		type_error;
    int		status;
    unsigned	i;
    unsigned	j;
    unsigned	cols;
    unsigned	rows;


    arg3 = pop ( );
    arg2 = pop ( );
    result = top ( );
    temp = *result;
    arg1 = &temp;


    status = 0;
    type_error = F_False;
    arg1 = CoerceData (deref (arg1), T_Double);


    if (!assignable (arg2)) {
	RecycleData (arg2);
	arg2 = NULL;
    } else
	arg2 = deref (arg2);

    if (!assignable (arg3)) {
	RecycleData (arg3);
	arg3 = NULL;
    } else
	arg3 = deref (arg3);


    switch (D_Type (arg1)) {
    case T_Double:
	CreateData (result, arg1, NULL, T_Double);
	*D_Double (result) = *D_Double (arg1);

	if (arg2)
	    status = AssignObject (arg2, T_Double, F_False, dbllit (1));

	if (arg3) {
	    CreateData (&scratch, NULL, NULL, T_Double);
	    *D_Double (&scratch) = *D_Double (arg1);
	    status = AssignObject (arg3, T_Double, F_True, D_Double (&scratch));
	    if (status)
		FreeData (&scratch);
	}
	break;


    case T_Matrix:
	a = D_Matrix (arg1);
	rows = Mrows (a);
	cols = Mcols (a);
	CreateData (result, arg1, NULL, T_Matrix, rows, cols);
	b = D_Matrix (result);

	q = CreateFullMatrix (rows, cols);
	r = CreateFullMatrix (rows, cols);

	if ((status = QRFactorMatrix (q, r, a))) {
	    MatrixError ("qr", a, NULL, status, F_True);
	    DestroyMatrix (q);
	    DestroyMatrix (r);

	} else {
	    for (i = 2; i <= rows; i ++)
		for (j = 1; j < i; j ++)
		    sdata (r, i, j) = 0;

	    CopyMatrix (b, r);

	    if (!arg2 || (status = AssignObject (arg2, T_Matrix, F_True, q)))
		DestroyMatrix (q);

	    if (!arg3 || (status = AssignObject (arg3, T_Matrix, F_True, r)))
		DestroyMatrix (r);
	}
	break;


    default:
	type_error = F_True;
	break;
    }


    if (type_error == F_True)
	TypeError ("qr", arg1, NULL, NULL, F_True);


    RecycleData (arg1);
    d_printf ("qr ans =\n");
    d_PrintData (result);

    return type_error == F_True || status != 0;
}


/************************************************************************
 * Function:	rand_func						*
 *									*
 * Description:	Pops three descriptors from the top of the stack and	*
 *		creates and pushes on the stack a random matrix.  The	*
 *		first descriptor specifies the number of rows and the	*
 *		second descriptor specifies the number of columns.  The	*
 *		third descriptor is the seed for the random number	*
 *		generator.  The following types are legal:		*
 *									*
 *		rand (null, null, null)	      -> scalar (random double)	*
 *		rand (double, null, null)     -> matrix (random matrix)	*
 *		rand (double, double, null)   -> matrix (random matrix)	*
 *		rand (double, double, double) -> matrix (random matrix)	*
 *									*
 *		An attempt is first made to coerce all arguments to	*
 *		double values.  The default value for the second	*
 *		argument is the value of the first argument.  If the	*
 *		third argument is null then the random generator is not	*
 *		seeded.  If the resulting matrix consists of only a	*
 *		single scalar then the result will be a double scalar.	*
 ************************************************************************/

int rand_func (n)
    int n;
{
    Matrix	a;
    double	val1;
    double	val2;
    double	val3;
    descriptor *arg1;
    descriptor *arg2;
    descriptor *arg3;
    descriptor *result;
    descriptor	temp;
    int		type_error;
    int		status;
    static int  seeded;


    arg3 = pop ( );
    arg2 = pop ( );
    result = top ( );
    temp = *result;
    arg1 = &temp;


    arg1 = CoerceData (deref (arg1), T_Double);
    arg2 = CoerceData (deref (arg2), T_Double);
    arg3 = CoerceData (deref (arg3), T_Double);


    status = 0;
    val1 = val2 = val3 = 0;
    type_error = F_False;


    switch (D_Type (arg1)) {
    case T_Null:
	val1 = 1;
	arg1 = NULL;
	break;


    case T_Double:
	val1 = *D_Double (arg1);
	break;


    default:
	type_error = F_True;
	break;
    }

    switch (D_Type (arg2)) {
    case T_Null:
	val2 = val1;
	arg2 = NULL;
	break;


    case T_Double:
	val2 = *D_Double (arg2);
	break;


    default:
	type_error = F_True;
	break;
    }

    switch (D_Type (arg3)) {
    case T_Null:
	val3 = 0;
	arg3 = NULL;
	break;


    case T_Double:
	val3 = *D_Double (arg3);
	break;


    default:
	type_error = F_True;
	break;
    }


    if (type_error == F_False)
	if (val1 < 1) {
	    rterror ("non-positive number of rows in rand() function");
	    status = 1;
	} else if (val2 < 1) {
	    rterror ("non-positive number of columns in rand() function");
	    status = 1;
	} else if ((int) val1 == 1 && (int) val2 == 1) {
	    if (val3)
		srand48 ((int) val3);
	    else if (!seeded) {
		seeded = 1;
		srand48 (time (NULL));
	    }
	    CreateData (result, arg1, arg2, T_Double);
	    *D_Double (result) = drand48 ( );
	} else {
	    CreateData (result, NULL, NULL, T_Matrix, (int) val1, (int) val2);
	    a = D_Matrix (result);
	    if ((status = RandomMatrix (a, (int) val3)))
		MatrixError ("rand", a, NULL, status, F_True);
	}


    if (type_error == F_True)
	TypeError ("rand", arg1, arg2, arg3, F_True);


    RecycleData (arg1);
    RecycleData (arg2);
    RecycleData (arg3);
    d_printf ("rand ans =\n");
    d_PrintData (result);

    return type_error == F_True || status != 0;
}


/************************************************************************
 * Function:	rows_func						*
 *									*
 * Description:	Pops the descriptor on the top of the stack and pushes	*
 *		its number of rows on the stack.  The following types	*
 *		are legal:						*
 *									*
 *		rows (double) -> double (always 1)			*
 *		rows (matrix) -> double (number of rows)		*
 *									*
 *		An attempt is first made to coerce the argument to a	*
 *		double value.						*
 ************************************************************************/

int rows_func (n)
    int n;
{
    descriptor *arg;
    descriptor *result;
    descriptor	temp;
    int		type_error;


    result = top ( );
    temp = *result;
    arg = &temp;


    arg = CoerceData (deref (arg), T_Double);
    type_error = F_False;


    switch (D_Type (arg)) {
    case T_Double:
	D_Type    (result) = T_Double;
	D_Temp    (result) = F_False;
	D_Trapped (result) = F_False;
	D_Double  (result) = dbllit (1);
	break;


    case T_Matrix:
	CreateData (result, NULL, NULL, T_Double);
	*D_Double (result) = Mrows (D_Matrix (arg));
	break;


    default:
	type_error = F_True;
	break;
    }


    if (type_error == F_True)
	TypeError ("rows", arg, NULL, NULL, F_True);


    RecycleData (arg);
    d_printf ("rows ans =\n");
    d_PrintData (result);

    return type_error == F_True;
}


/************************************************************************
 * Function:	zeros_func						*
 *									*
 * Description:	Pops two descriptors from the top of the stack and	*
 *		creates and pushes on the stack a zero matrix.  The	*
 *		first descriptor specifies the number of rows and the	*
 *		second descriptor specified the number of columns.  The	*
 *		following types are legal:				*
 *									*
 *		zeros (double, null)   -> matrix (matrix creation)	*
 *		zeros (double, double) -> matrix (matrix creation)	*
 *									*
 *		An attempt is first made to coerce the arguments to	*
 *		double values.   The default value for the second	*
 *		argument is the value of the first argument.  If the	*
 *		resulting matrix consists of only a single scalar then	*
 *		the result will be a double scalar.			*
 ************************************************************************/

int zeros_func (n)
    int n;
{
    Matrix	a;
    double	val1;
    double	val2;
    descriptor *arg1;
    descriptor *arg2;
    descriptor *result;
    descriptor *old;
    descriptor	temp;
    int		type_error;
    int		status;


    arg2 = pop ( );
    result = top ( );
    temp = *result;
    arg1 = &temp;


    arg1 = CoerceData (deref (arg1), T_Double);
    arg2 = CoerceData (deref (arg2), T_Double);


    status = 0;
    type_error = F_False;


    if (D_Type (arg2) == T_Null) {
	RecycleData (arg2);
	arg2 = arg1;
	old = NULL;
    } else
	old = arg2;


    if (D_Type (arg1) == T_Double && D_Type (arg2) == T_Double) {
	val1 = *D_Double (arg1);
	val2 = *D_Double (arg2);
	if (val1 < 1) {
	    rterror ("non-positive number of rows in zeros() function");
	    status = 1;
	} else if (val2 < 1) {
	    rterror ("non-positive number of columns in zeros() function");
	    status = 1;
	} else if ((int) val1 == 1 && (int) val2 == 1) {
	    D_Type    (result) = T_Double;
	    D_Temp    (result) = F_False;
	    D_Trapped (result) = F_False;
	    D_Double  (result) = dbllit (0);
	} else {
	    CreateData (result, NULL, NULL, T_Matrix, (int) val1, (int) val2);
	    a = D_Matrix (result);
	    if ((status = ZeroMatrix (a)))
		MatrixError ("zeros", a, NULL, status, F_True);
	}
    } else
	type_error = F_True;


    if (type_error == F_True)
	TypeError ("zeros", arg1, old, NULL, F_True);


    RecycleData (arg1);
    RecycleData (old);
    d_printf ("zeros ans =\n");
    d_PrintData (result);

    return type_error == F_True || status != 0;
}
