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
 * File:	predicate.c						*
 *									*
 * Description:	This file contains the function definitions for the	*
 *		predicate intrinsic functions.				*
 ************************************************************************/

# include "debug.h"
# include "coerce.h"
# include "execute.h"
# include "predicate.h"


/************************************************************************
 * Function:	anyp_func						*
 *									*
 * Description:	Pops the descriptor on the top of the stack, determines	*
 *		whether any of the elements are nonzero, and pushes the	*
 *		result on the stack.  The following types are legal:	*
 *									*
 *		any (double) -> double (scalar comparison)		*
 *		any (matrix) -> double (matrix element-wise comparison)	*
 *									*
 *		An attempt is first made to coerce the argument to a	*
 *		double value.						*
 ************************************************************************/

int anyp_func (int n)
{
    Matrix	a;
    descriptor *arg;
    descriptor *result;
    descriptor	temp;
    unsigned	i;
    unsigned	j;
    int		type_error;
    int		cmp;


    result = top ( );
    temp = *result;
    arg = &temp;


    type_error = F_False;
    arg = CoerceData (deref (arg), T_Double);


    switch (D_Type (arg)) {
    case T_Double:
	D_Type	  (result) = T_Double;
	D_Temp	  (result) = F_False;
	D_Trapped (result) = F_False;
	D_Double  (result) = dbllit (*D_Double (arg) != 0);
	break;


    case T_Matrix:
	cmp = 0;
	a = D_Matrix (arg);
	for (i = 1; i <= Mrows (a); i ++)
	    for (j = 1; j <= Mcols (a); j ++)
		if (mdata (a, i, j)) {
		    cmp = 1;
		    break;
		}

	D_Type	  (result) = T_Double;
	D_Temp	  (result) = F_False;
	D_Trapped (result) = F_False;
	D_Double  (result) = dbllit (cmp);
	break;


    default:
	type_error = F_True;
    }


    if (type_error == F_True)
	TypeError ("any?", arg, NULL, NULL, F_True);


    RecycleData (arg);
    d_printf ("anyp ans =\n");
    d_PrintData (result);

    return type_error == F_True;
}


/************************************************************************
 * Function:	compactp_func						*
 *									*
 * Description:	Pops the descriptor on the top of the stack and		*
 *		determines whether the descriptor is a compact matrix.	*
 ************************************************************************/

int compactp_func (int n)
{
    descriptor *arg;
    descriptor *result;
    descriptor	temp;
    int		type_error;


    result = top ( );
    temp = *result;
    arg = &temp;
    arg = deref (arg);
    arg = CollapseMatrix (arg);


    type_error = F_False;

    D_Type    (result) = T_Double;
    D_Temp    (result) = F_False;
    D_Trapped (result) = F_False;

    switch (D_Type (arg)) {
    case T_Matrix:
    case T_Double:
	D_Double (result) = dbllit (IsCompact (D_Matrix (arg)) ? 1 : 0);
	break;


    default:
	type_error = F_True;
	TypeError ("compact?", arg, NULL, NULL, F_True);
	break;
    }


    RecycleData (arg);
    d_printf ("compactp ans =\n");
    d_PrintData (result);

    return type_error == F_True;
}


/************************************************************************
 * Function:	everyp_func						*
 *									*
 * Description:	Pops the descriptor on the top of the stack, determines	*
 *		whether all of the elements are nonzero, and pushes the	*
 *		result on the stack.  The following types are legal:	*
 *									*
 *		any (double) -> double (scalar comparison)		*
 *		any (matrix) -> double (matrix element-wise comparison)	*
 *									*
 *		An attempt is first made to coerce the argument to a	*
 *		double value.						*
 ************************************************************************/

int everyp_func (int n)
{
    Matrix	a;
    descriptor *arg;
    descriptor *result;
    descriptor	temp;
    unsigned	i;
    unsigned	j;
    int		type_error;
    int		cmp;


    result = top ( );
    temp = *result;
    arg = &temp;


    type_error = F_False;
    arg = CoerceData (deref (arg), T_Double);


    switch (D_Type (arg)) {
    case T_Double:
	D_Type	  (result) = T_Double;
	D_Temp	  (result) = F_False;
	D_Trapped (result) = F_False;
	D_Double  (result) = dbllit (*D_Double (arg) != 0);
	break;


    case T_Matrix:
	cmp = 1;
	a = D_Matrix (arg);
	for (i = 1; i <= Mrows (a); i ++)
	    for (j = 1; j <= Mcols (a); j ++)
		if (!mdata (a, i, j)) {
		    cmp = 0;
		    break;
		}

	D_Type	  (result) = T_Double;
	D_Temp	  (result) = F_False;
	D_Trapped (result) = F_False;
	D_Double  (result) = dbllit (cmp);
	break;


    default:
	type_error = F_True;
    }


    if (type_error == F_True)
	TypeError ("every?", arg, NULL, NULL, F_True);


    RecycleData (arg);
    d_printf ("everyp ans =\n");
    d_PrintData (result);

    return type_error == F_True;
}


/************************************************************************
 * Function:	matrixp_func						*
 *									*
 * Description:	Pops the descriptor on the top of the stack and		*
 *		determines whether the descriptor is a matrix.		*
 ************************************************************************/

int matrixp_func (int n)
{
    descriptor *arg;
    descriptor *result;
    descriptor	temp;


    result = top ( );
    temp = *result;
    arg = &temp;
    arg = deref (arg);
    arg = CoerceData (arg, T_Double);


    D_Type    (result) = T_Double;
    D_Temp    (result) = F_False;
    D_Trapped (result) = F_False;

    switch (D_Type (arg)) {
    case T_Matrix:
	D_Double (result) = dbllit (1);
	break;


    default:
	D_Double (result) = dbllit (0);
	break;
    }


    RecycleData (arg);
    d_printf ("matrixp ans =\n");
    d_PrintData (result);

    return 0;
}


/************************************************************************
 * Function:	nullp_func						*
 *									*
 * Description:	Pops the descriptor on the top of the stack and		*
 *		determines whether the descriptor is null.		*
 ************************************************************************/

int nullp_func (int n)
{
    descriptor *arg;
    descriptor *result;
    descriptor	temp;


    result = top ( );
    temp = *result;
    arg = &temp;
    arg = deref (arg);


    D_Type    (result) = T_Double;
    D_Temp    (result) = F_False;
    D_Trapped (result) = F_False;

    switch (D_Type (arg)) {
    case T_Null:
	D_Double (result) = dbllit (1);
	break;


    case T_Constraint:
    case T_Definition:
    case T_Element:
    case T_Force:
    case T_Load:
    case T_Material:
    case T_Node:
	D_Double (result) = dbllit (* (void **) D_Pointer (arg) == NULL);
	break;


    default:
	D_Double (result) = dbllit (0);
	break;
    }


    RecycleData (arg);
    d_printf ("nullp ans =\n");
    d_PrintData (result);

    return 0;
}


/************************************************************************
 * Function:	scalarp_func						*
 *									*
 * Description:	Pops the descriptor on the top of the stack and		*
 *		determines whether the descriptor is a scalar.		*
 ************************************************************************/

int scalarp_func (int n)
{
    descriptor *arg;
    descriptor *result;
    descriptor	temp;


    result = top ( );
    temp = *result;
    arg = &temp;
    arg = deref (arg);
    arg = CoerceData (arg, T_Double);


    D_Type    (result) = T_Double;
    D_Temp    (result) = F_False;
    D_Trapped (result) = F_False;

    switch (D_Type (arg)) {
    case T_Double:
	D_Double (result) = dbllit (1);
	break;


    default:
	D_Double (result) = dbllit (0);
	break;
    }


    RecycleData (arg);
    d_printf ("scalarp ans =\n");
    d_PrintData (result);

    return 0;
}


/************************************************************************
 * Function:	symmetricp_func						*
 *									*
 * Description:	Pops the descriptor on the top of the stack and		*
 *		determines whether the descriptor is a symmetric	*
 *		matrix.							*
 ************************************************************************/

int symmetricp_func (int n)
{
    descriptor *arg;
    descriptor *result;
    descriptor	temp;
    int		type_error;


    result = top ( );
    temp = *result;
    arg = &temp;
    arg = deref (arg);
    arg = CoerceData (arg, T_Double);

    type_error = F_False;


    D_Type    (result) = T_Double;
    D_Temp    (result) = F_False;
    D_Trapped (result) = F_False;

    switch (D_Type (arg)) {
    case T_Matrix:
	D_Double (result) = dbllit (IsSymmetricMatrix (D_Matrix (arg)) ? 1 : 0);
	break;


    case T_Double:
	D_Double (result) = dbllit (1);
	break;


    default:
	type_error = F_True;
	break;
    }


    if (type_error == F_True)
	TypeError ("symmetric?", arg, NULL, NULL, F_True);


    RecycleData (arg);
    d_printf ("symmetricp ans =\n");
    d_PrintData (result);

    return 0;
}
