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
 * File:	fefunc.c						*
 *									*
 * Description:	This file contains the function definitions for the	*
 *		finite element intrinsic functions.			*
 ************************************************************************/

# include <stdio.h>
# include <string.h>
# include <stdlib.h>
# include "felt.h"
# include "trap.h"
# include "apply.h"
# include "debug.h"
# include "error.h"
# include "coerce.h"
# include "fefunc.h"
# include "execute.h"
# include "problem.h"
# include "allocate.h"
# include "definition.h"
# include "pathsearch.h"
# include "renumber.hpp"

static struct {
    Definition definition;
    Function   set_up;
    Function   stress;
} burlap_defs [32];

static int num_defs;

/* These are only in misc.h. */

extern "C" {
double ElementArea(Element e, unsigned int n);
double ElementLength(Element element, unsigned int coords);
}

/************************************************************************
 * Function:	is_static						*
 *									*
 * Description:	Returns nonzero if the problem is a static problem, and	*
 *		zero otherwise.						*
 ************************************************************************/

static int is_static (void)
{
    return problem.mode == Static || problem.mode == StaticThermal;
}


/************************************************************************
 * Function:	check_analysis						*
 *									*
 * Description:	Returns nonzero if the analysis parameters are properly	*
 *		initialized, and zero otherwise.			*
 ************************************************************************/

static int check_analysis (const char *func)
{
    if (CheckAnalysisParameters (problem.mode))
	return 0;

    if (problem.mode == TransientThermal) {
	analysis.numdofs = 1;
	analysis.dofs [1] = Tx;
    }

    if (analysis.numnodes * analysis.numdofs == 0) {
	rterror ("missing analysis parameters");
	return 1;
    }

    return 1;
}


/************************************************************************
 * Function:	is_permutation						*
 *									*
 * Description:	Returns nonzero if the array is a permutation vector	*
 *		suitable for node renumbering, otherwise zero.		*
 ************************************************************************/

static int is_permutation (const char *func, Array array)
{
    int  i;
    int  j;
    int  k;
    int  n;
    int *ptr;


    if ((n = array -> length) != problem.num_nodes) {
	rterror ("illegal array length (%d) in %s()", n, func);
	return 1;
    } 

    ptr = (int *) array -> ptr;

    for (i = 1; i <= n; i ++) {
	k = ptr [i];

	if (k < 1 || k > n) {
	    rterror ("illegal node number (%d) in %s()", k, func);
	    return 0;
	}

	for (j = i + 1; j <= n; j ++)
	    if (ptr [j] == k) {
		rterror ("duplicate node number (%d) in %s()", k, func);
		return 0;
	    }
    }

    return 1;
}


/************************************************************************
 * Function:	element_set_up						*
 *									*
 * Description:	Calls the burlap code for the set-up function for the	*
 *		specified element.  The FElt routines expect to call	*
 *		a C function for the element set-up step.  In this case	*
 *		a new element definition has been added and has a	*
 *		burlap function as its set-up function.  We simply	*
 *		look up the definition in the private array and call	*
 *		the approriate function with the arguments transformed	*
 *		from C objects into burlap descriptors.			*
 ************************************************************************/

/* ATTN: changed arg list from (Element element, int mass_mode) */
static int element_set_up (Element element, char mass_mode, int tangent)
{
    int		i;
    int		status;
    descriptor *arg1;
    descriptor *arg2;
    descriptor *function;
    descriptor *result;
    int imass_mode;

    status = 1;
    imass_mode = mass_mode;

    for (i = 0; i < num_defs; i ++)
	if (burlap_defs [i].definition == element -> definition) {
	    function = push ( );
	    arg1 = push ( );
	    arg2 = push ( );

	    D_Type     (function) = T_Function;
	    D_Temp     (function) = F_False;
	    D_Trapped  (function) = F_False;
	    D_Function (function) = burlap_defs [i].set_up;

	    D_Type    (arg1) = T_Element;
	    D_Temp    (arg1) = F_False;
	    D_Trapped (arg1) = F_False;
	    arg1 -> u.ptr = &element;

	    D_Type    (arg2) = T_Int;
	    D_Temp    (arg2) = F_False;
	    D_Trapped (arg2) = F_False;
	    D_Int     (arg2) = &imass_mode;

	    if (!function_call (function, 2)) {
		result = pop ( );
		result = CoerceData (deref (result), T_Int);
		status = D_Type (result) == T_Int ? *D_Int (result) : 1;
		RecycleData (result);
	    }
	}

    return status;
}


/************************************************************************
 * Function:	element_stress						*
 *									*
 * Description:	Calls the burlap code for the stress function for the	*
 *		specified element.  The FElt routines expect to call	*
 *		a C function for the element stress computation.  In	*
 *		this case a new element definition has been added and	*
 *		has a burlap function as its stress function.  We	*
 *		simply look up the definition in the private array and	*
 *		call the approriate function with the arguments		*
 *		transformed from C objects into burlap descriptors.	*
 ************************************************************************/

static int element_stress (Element element)
{
    int		i;
    int		status;
    descriptor *arg1;
    descriptor *function;
    descriptor *result;


    status = 1;

    for (i = 0; i < num_defs; i ++)
	if (burlap_defs [i].definition == element -> definition) {
	    function = push ( );
	    arg1 = push ( );

	    D_Type     (function) = T_Function;
	    D_Temp     (function) = F_False;
	    D_Trapped  (function) = F_False;
	    D_Function (function) = burlap_defs [i].stress;

	    D_Type    (arg1) = T_Element;
	    D_Temp    (arg1) = F_False;
	    D_Trapped (arg1) = F_False;
	    arg1 -> u.ptr = &element;

	    if (!function_call (function, 1)) {
		result = top ( );
		result = CoerceData (deref (result), T_Int);
		status = D_Type (result) == T_Int ? *D_Int (result) : 1;
		RecycleData (result);
	    }
	}

    return status;
}


/************************************************************************
 * Function:	check_matrix						*
 *									*
 * Description:	Verifies that the specified matrix is the specified	*
 *		size and that it is compact, or can be compacted.	*
 ************************************************************************/

static Matrix check_matrix (const char *func, Matrix a)
{
    Matrix   b;
    unsigned size;


    size = problem.num_nodes * problem.num_dofs;

    if (Mrows (a) != size || Mcols (a) != size) {
	MatrixError (func, a, NULL, M_SIZEMISMATCH, F_True);
	return NULL;
    }

    if (IsCompact (a))
	return a;

    if (!(b = MakeCompactFromFull (a))) {
	MatrixError (func, a, NULL, M_NOTCOMPACT, F_True);
	return NULL;
    }

    return b;
}


/************************************************************************
 * Function:	check_dofs						*
 *									*
 * Description:	Verifies that the specified descriptor is a proper	*
 *		array of the degrees of freedom.			*
 ************************************************************************/

static unsigned *check_dofs (descriptor *d, const char *function, unsigned int *num_dofs)
{
    unsigned  i;
    unsigned *dofs;
    Array     array;
    int       last;


    array = D_Array (d);
    if (array -> length != 6) {
	rterror ("improper array length in %s()", function);
	return NULL;
    } else if (D_Type (array) != T_Int) {
	rterror ("improper array type in %s()", function);
	return NULL;
    } else {
	last = 0;
	*num_dofs = 0;
	dofs = (unsigned *) array -> ptr;
	for (i = 1; i <= 6; i ++)
	    if (dofs [i] > 6) {
		rterror ("illegal DOF in %s()", function);
		return NULL;
	    } else if (dofs [i] && dofs [i] != last + 1) {
		rterror ("illegal DOF array in %s()", function);
		return NULL;
	    } else if (dofs [i]) {
		last = dofs [i];
		(*num_dofs) ++;
	    }

	if (*num_dofs == 0) {
	    rterror ("no active dofs in %s()", function);
	    return NULL;
	}

	return dofs;
    }
}


/************************************************************************
 * Function:	area_func						*
 *									*
 * Description:	Pops and computes the area of the descriptor on the	*
 *		top of the stack and places the result on the stack.	*
 *		This function is only defined for planar elements.	*
 ************************************************************************/

int area_func (int n)
{
    Element	e;
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
    arg = deref (arg);


    switch (D_Type (arg)) {
    case T_Element:
	e = *D_Element (arg);
	if (e -> definition -> shape == Planar) {
	    CreateData (result, NULL, NULL, T_Double);
	    *D_Double (result) = ElementArea (e, e -> definition -> shapenodes);
	} else {
	    rterror ("area of non-planar element is undefined");
	    status = 1;
	}
	break;


    default:
	type_error = F_True;
	break;
    }


    if (type_error == F_True)
	TypeError ("area", arg, NULL, NULL, F_True);


    RecycleData (arg);
    d_printf ("area ans =\n");
    d_PrintData (result);

    return type_error == F_True || status != 0;
}


/************************************************************************
 * Function:	felt_func						*
 *									*
 * Description:	Pops the descriptor on the top of the stack and treats	*
 *		it as the name of a FElt file to load.  The path named	*
 *		by the FELT_PATH environment variable is searched for	*
 *		the file.  The result is the full path name of the	*
 *		loaded file.  The following types are legal:		*
 *									*
 *		felt (string) -> string (FElt file load)		*
 *									*
 *		An attempt is first made to coerce the argument to a	*
 *		string value.						*
 ************************************************************************/

int felt_func (int n)
{
    char	*name;
    static char	*path;
    descriptor	*arg;
    descriptor	*result;
    descriptor	 temp;
    int		 type_error;


    result = top ( );
    temp = *result;
    arg = &temp;


    type_error = F_False;
    arg = CoerceData (deref (arg), T_String);


    if (D_Type (arg) == T_Null) {
	read_felt (NULL);
	CreateData (result, NULL, NULL, T_Null);

    } else if (D_Type (arg) == T_String) {
	if (!path)
	    path = getenv ("FELT_PATH");

	name = pathsearch (path, *D_String (arg), ".flt", F_True);
	CreateData (result, NULL, NULL, T_String, strlen (name) + 1);
	strcpy (*D_String (result), name);

	if (read_felt (name)) {
	    rterror ("unable to read '%s'", name);
	    **D_String (result) = 0;
	}

    } else
	type_error = F_True;


    if (type_error == F_True)
	TypeError ("felt", arg, NULL, NULL, F_True);


    RecycleData (arg);
    return type_error == F_True;
}


/************************************************************************
 * Function:	length_func						*
 *									*
 * Description:	Pops and computes the length of the descriptor on the	*
 *		top of the stack and places the result on the stack.	*
 *		The following types are legal:				*
 *									*
 *		length (string)  -> scalar (number of characters)	*
 *		length (scalar)  -> scalar (always one)			*
 *		length (matrix)  -> scalar (number of elements)		*
 *		length (array)   -> scalar (number of elements)		*
 *		length (element) -> scalar (length of linear element)	*
 *									*
 *		An attempt is first made to coerce the argument to a	*
 *		double value if the argument is not a string.		*
 ************************************************************************/

int length_func (int n)
{
    Matrix	a;
    Element	e;
    descriptor *arg;
    descriptor *result;
    descriptor	temp;
    int		type_error;
    int		status;


    result = top ( );
    temp = *result;
    arg = &temp;


    arg = deref (arg);
    type_error = F_False;
    status = 0;

    if (D_Type (arg) != T_String)
	arg = CoerceData (arg, T_Double);


    switch (D_Type (arg)) {
    case T_String:
	CreateData (result, NULL, NULL, T_Double);
	*D_Double (result) = strlen (*D_String (arg));
	break;


    case T_Double:
	D_Type	  (result) = T_Double;
	D_Temp	  (result) = F_False;
	D_Trapped (result) = F_False;
	D_Double  (result) = dbllit (1);
	break;


    case T_Matrix:
	a = D_Matrix (arg);
	CreateData (result, NULL, NULL, T_Double);
	*D_Double (result) = Mrows (a) * Mcols (a);
	break;


    case T_Array:
	CreateData (result, NULL, NULL, T_Double);
	*D_Double (result) = D_Array (arg) -> length;
	break;


    case T_Element:
	e = *D_Element (arg);
	if (e -> definition -> shape == Linear) {
	    CreateData (result, NULL, NULL, T_Double);
	    *D_Double (result) = ElementLength (e, 3);
	} else {
	    rterror ("length of non-linear element is undefined");
	    status = 1;
	}
	break;


    default:
	type_error = F_True;
	break;
    }


    if (type_error == F_True)
	TypeError ("length", arg, NULL, NULL, F_True);


    RecycleData (arg);
    d_printf ("length ans =\n");
    d_PrintData (result);

    return type_error == F_True || status != 0;
}


/************************************************************************
 * Function:	volume_func						*
 *									*
 * Description:	Not available yet.					*
 ************************************************************************/

int volume_func (int n)
{
    rterror ("volume() function is not available");
    return 1;
}


/************************************************************************
 * Function:	add_definition_func					*
 *									*
 * Description:	Pops the descriptors on the top of the stack and uses	*
 *		them as structure members of an element definition	*
 *		structure and places the result of adding the		*
 *		definition on the stack.				*
 ************************************************************************/

int add_definition_func (int n)
{
    descriptor *arg1;
    descriptor *arg2;
    descriptor *arg3;
    descriptor *arg4;
    descriptor *arg5;
    descriptor *arg6;
    descriptor *arg7;
    descriptor *arg8;
    descriptor *arg9;
    descriptor *result;
    descriptor	temp;
    Definition	definition;
    int		type_error;
    int		success;
    int		status;
    unsigned	i;
    unsigned	num_dofs;
    unsigned   *dofs;


    arg9 = pop ( );
    arg8 = pop ( );
    arg7 = pop ( );
    arg6 = pop ( );
    arg5 = pop ( );
    arg4 = pop ( );
    arg3 = pop ( );
    arg2 = pop ( );
    result = top ( );
    temp = *result;
    arg1 = &temp;


    status = 0;
    type_error = F_False;

    arg1 = CoerceData (deref (arg1), T_String);
    arg2 = CoerceData (deref (arg2), T_Function);
    arg3 = CoerceData (deref (arg3), T_Function);
    arg4 = CoerceData (deref (arg4), T_Int);
    arg5 = CoerceData (deref (arg5), T_Int);
    arg6 = CoerceData (deref (arg6), T_Int);
    arg7 = CoerceData (deref (arg7), T_Int);
    arg9 = CoerceData (deref (arg9), T_Int);

    arg8 = deref (arg8);
    arg8 = CollapseMatrix (arg8);
    arg8 = CoerceToArray (arg8, T_Int);


    if (D_Type (arg1) != T_String)
	type_error = F_True;
    else if (D_Type (arg2) != T_Function)
	type_error = F_True;
    else if (D_Type (arg3) != T_Function)
	type_error = F_True;
    else if (D_Type (arg4) != T_Int)
	type_error = F_True;
    else if (D_Type (arg5) != T_Int)
	type_error = F_True;
    else if (D_Type (arg6) != T_Int)
	type_error = F_True;
    else if (D_Type (arg7) != T_Int)
	type_error = F_True;
    else if (D_Type (arg8) != T_Array)
	type_error = F_True;
    else if (!(dofs = check_dofs (arg8, "add_definition", &num_dofs)))
	status = 1;
    else {
	if (!(definition = LookupDefinition (*D_String (arg1)))) {
	    definition = New (struct definition);
	    definition -> name	      = *D_String (arg1);
	    definition -> setup	      = element_set_up;
	    definition -> stress      = element_stress;
	    definition -> shape	      = (Shape) *D_Int (arg4);
	    definition -> numnodes    = *D_Int (arg5);
	    definition -> shapenodes  = *D_Int (arg6);
	    definition -> numstresses = *D_Int (arg7);
	    definition -> numdofs     = num_dofs;
	    definition -> retainK     = D_Type (arg9) != T_Null;

	    for (i = 1; i <= 6; i ++)
		definition -> dofs [i] = dofs [i];

	    burlap_defs [num_defs].definition = definition;
	    burlap_defs [num_defs].set_up = CopyFunction (D_Function (arg2));
	    burlap_defs [num_defs].stress = CopyFunction (D_Function (arg3));
	    num_defs ++;

	    AddDefinition (definition);
	    success = F_True;
	} else
	    success = F_False;

	D_Type	  (result) = T_Double;
	D_Temp	  (result) = F_False;
	D_Trapped (result) = F_False;
	D_Double  (result) = dbllit (success ? 0 : 1);
    }


    if (type_error == F_True)
	TypeError ("add_definition", NULL, NULL, NULL, F_True);


    RecycleData (arg1);
    RecycleData (arg2);
    RecycleData (arg3);
    RecycleData (arg4);
    RecycleData (arg5);
    RecycleData (arg6);
    RecycleData (arg7);
    RecycleData (arg8);

    return type_error == F_True || status != 0;
}


/************************************************************************
 * Function:	remove_definition_func					*
 *									*
 * Description:	Pops the descriptor from the top of the stack and	*
 *		removes the element definition named by the descriptor.	*
 *		If the definition is successfully removed then a zero	*
 *		descriptor is pushed on the stack.  Otherwise, a one	*
 *		descriptor is pushed on the stack.  The following types	*
 *		are legal:						*
 *									*
 *		remove_definition (string) -> double			*
 *									*
 *		An attempt is first made to coerce the argument to a	*
 *		string value.						*
 ************************************************************************/

int remove_definition_func (int n)
{
    descriptor *arg;
    descriptor *result;
    descriptor	temp;
    Definition	definition;
    int		type_error;
    int		success;
    unsigned	i;


    result = top ( );
    temp = *result;
    arg = &temp;


    type_error = F_False;
    arg = CoerceData (deref (arg), T_String);


    switch (D_Type (arg)) {
    case T_String:
	success = F_True;
	if ((definition = LookupDefinition (*D_String (arg)))) {
	    for (i = 1; i <= problem.num_elements; i ++)
		if (problem.elements [i] -> definition == definition) {
		    printf ("remove_definition: definition still in use\n");
		    success = F_False;
		    break;
		}

	    if (success == F_True) {
		RemoveDefinition (definition);
		for (i = 0; i < num_defs; i ++)
		    if (burlap_defs [i].definition == definition) {
			DestroyFunction (burlap_defs [i].set_up);
			DestroyFunction (burlap_defs [i].stress);
			burlap_defs [i] = burlap_defs [-- num_defs];
			Delete (definition);
			break;
		    }
	    }
	}

	D_Type	  (result) = T_Double;
	D_Temp	  (result) = F_False;
	D_Trapped (result) = F_False;
	D_Double  (result) = dbllit (success ? 0 : 1);
	break;


    default:
	type_error = F_True;
	break;
    }


    if (type_error == F_True)
	TypeError ("remove_definition", arg, NULL, NULL, F_True);


    RecycleData (arg);
    return type_error == F_True;
}


/************************************************************************
 * Function:	assemble_func						*
 *									*
 * Description:	Interface to ConstructStiffness() & ConstructDynamic().	*
 *		The stiffness matrix is returned.  The damping and mass	*
 *		matrices may be returned by specifying them as the	*
 *		parameters.  The active DOFs must have previously been	*
 *		computed.						*
 ************************************************************************/

int assemble_func (int n)
{
    Matrix	K;
    Matrix	M;
    Matrix	C;
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

    if (!assignable (arg1)) {
	RecycleData (arg1);
	arg1 = NULL;
    } else
	arg1 = deref (arg1);

    if (!assignable (arg2)) {
	RecycleData (arg2);
	arg2 = NULL;
    } else
	arg2 = deref (arg2);


    if (!problem.num_nodes) {
	rterror ("no FElt problem loaded in assemble()");
	status = 1;

    } else if (!problem.num_dofs) {
	rterror ("no active DOFs in assemble()");
	status = 1;

    } else {
	if (is_static ( )) {
	    if ((K = ConstructStiffness (&status))) {
		D_Type	  (result) = T_Matrix;
		D_Temp	  (result) = F_True;
		D_Trapped (result) = F_False;
		D_Matrix  (result) = K;

	    } else
		CreateData (result, NULL, NULL, T_Null);

	} else {
	    if (!ConstructDynamic (&K, &M, &C)) {
		D_Type	  (result) = T_Matrix;
		D_Temp	  (result) = F_True;
		D_Trapped (result) = F_False;
		D_Matrix  (result) = K;

		if (!arg1 || AssignObject (arg1, T_Matrix, F_True, M))
		    DestroyMatrix (M);

		if (!arg2 || AssignObject (arg2, T_Matrix, F_True, C))
		    DestroyMatrix (C);

	    } else
		CreateData (result, NULL, NULL, T_Null);
	}
    }

    return type_error == F_True || status != 0;
}


/************************************************************************
 * Function:	clear_nodes_func					*
 *									*
 * Description:	Interface to ClearNodes().  The return value is always	*
 *		a null descriptor.					*
 ************************************************************************/

int clear_nodes_func (int n)
{
    descriptor *result;


    if (!problem.num_nodes) {
	rterror ("no FElt problem loaded in clear_nodes()");
	return 1;
    }

    result = push ( );
    CreateData (result, NULL, NULL, T_Null);
    ClearNodes ( );

    return 0;
}


/************************************************************************
 * Function:	compute_modes_func					*
 *									*
 * Description:	Interface to ComputeEigenModes().  The active DOFs must	*
 *		have previously been computed.  The return value is the	*
 *		vector of eigenvalues.  The matrix of eigenvectors can	*
 *		of retrieved by specifying it as the remaining		*
 *		parameter.  The following types are legal:		*
 *									*
 *		compute_modes (matrix, matrix, ...) -> matrix		*
 *									*
 *		An attempt is first made to coerce the first two	*
 *		arguments to matrices.					*
 ************************************************************************/

int compute_modes_func (int n)
{
    Matrix	l;
    Matrix	x;
    Matrix	K;
    Matrix	M;
    descriptor *arg1;
    descriptor *arg2;
    descriptor *arg3;
    descriptor *result;
    descriptor	temp;
    int		type_error;
    int		status;


    arg3 = pop ( );
    arg2 = pop ( );
    result = top ( );
    temp = *result;
    arg1 = &temp;


    status = 0;
    type_error = F_False;

    arg1 = CoerceData (deref (arg1), T_Matrix);
    arg2 = CoerceData (deref (arg2), T_Matrix);

    if (!assignable (arg3)) {
	RecycleData (arg3);
	arg3 = NULL;
    } else
	arg3 = deref (arg3);


    if (!problem.num_nodes) {
	rterror ("no FElt problem loaded in compute_modes()");
	status = 1;

    } else if (!problem.num_dofs) {
	rterror ("no active DOFs in compute_modes()");
	status = 1;

    } else if (D_Type (arg1) == T_Matrix && D_Type (arg2) == T_Matrix) {
	K = D_Matrix (arg1);
	M = D_Matrix (arg2);

	if ((status = ComputeEigenModes (K, M, &l, &x)))
	    MatrixError ("compute_modes", M, NULL, status, F_True);
	else {
	    D_Type	  (result) = T_Matrix;
	    D_Temp	  (result) = F_True;
	    D_Trapped (result) = F_False;
	    D_Matrix  (result) = l;

	    if (!arg3 || AssignObject (arg3, T_Matrix, F_True, x))
		DestroyMatrix (x);
	}

    } else
	type_error = F_True;


    if (type_error == F_True)
	TypeError ("compute_modes", arg1, arg2, arg3, F_True);


    RecycleData (arg1);
    RecycleData (arg2);

    return type_error == F_True || status != 0;
}


/************************************************************************
 * Function:	compute_stresses_func					*
 *									*
 * Description:	Not available yet.					*
 ************************************************************************/

int compute_stresses_func (int n)
{
    rterror ("compute_stresses() function is not available");
    return 1;
}


/************************************************************************
 * Function:	construct_forces_func					*
 *									*
 * Description:	Interface to ConstructForceVector() and			*
 *		AssembleTransientForce().  The active DOFs must have	*
 *		previously been computed.  The following types are	*
 *		legal:							*
 *									*
 *		construct_forces ( )	  -> matrix (force vector)	*
 *		construct_forces (double) -> matrix (force vector)	*
 *									*
 *		An attempt is first made to coerce the argument to a	*
 *		double value.						*
 ************************************************************************/

int construct_forces_func (int n)
{
    double	t;
    Matrix	v;
    descriptor *arg;
    descriptor *result;
    descriptor	temp;
    unsigned	nrows;
    int		type_error;
    int		status;


    result = top ( );
    temp = *result;
    arg = &temp;


    status = 0;
    type_error = F_False;
    arg = CoerceData (deref (arg), T_Double);


    if (!problem.num_nodes) {
	rterror ("no FElt problem loaded in construct_forces()");
	status = 1;

    } else if (!problem.num_dofs) {
	rterror ("no active DOFs in construct_forces()");
	status = 1;

    } else {
	if (is_static ( )) {
	    v = ConstructForceVector ( );

	    D_Type    (result) = T_Matrix;
	    D_Temp    (result) = F_True;
	    D_Trapped (result) = F_False;
	    D_Matrix  (result) = v;

	} else if (D_Type (arg) == T_Double || D_Type (arg) == T_Null) {
	    nrows = problem.num_nodes * problem.num_dofs;
	    CreateData (result, NULL, NULL, T_Matrix, nrows, 1);
	    v = D_Matrix (result);
	    t = D_Type (arg) == T_Double ? *D_Double (arg) : 0;
	    AssembleTransientForce (t, v);

	} else
	    type_error = F_True;
    }


    if (type_error == F_True)
	TypeError ("construct_forces", arg, NULL, NULL, F_True);


    RecycleData (arg);
    return type_error == F_True || status != 0;
}


/************************************************************************
 * Function:	find_dofs_func						*
 *									*
 * Description:	Interface to FindDOFS().  The return value is the	*
 *		number of degrees of freedom.				*
 ************************************************************************/

int find_dofs_func (int n)
{
    ste	       *s;
    descriptor *dofs;
    descriptor *result;


    if (!problem.num_nodes) {
	rterror ("no FElt problem loaded in find_dofs()");
	return 1;
    }

    FindDOFS ( );

    s = st_lookup (&var_st, "dofs_num");
    dofs = global (s -> idx);
    D_Array (dofs) -> length = problem.num_dofs;

    result = push ( );
    CreateData (result, NULL, NULL, T_Double);
    *D_Double (result) = problem.num_dofs;

    return 0;
}


/************************************************************************
 * Function:	global_dof_func						*
 *									*
 * Description: Interface to GlobalDOF().  The active DOFs must have	*
 *		previously been computed and the local DOF must be in	*
 *		range.  The following types are legal:			*
 *									*
 *		global_dof (int, int)  -> int (global DOF)		*
 *		global_dof (node, int) -> int (global DOF)		*
 *									*
 *		An attempt is first made to coerce both arguments to	*
 *		integer values.						*
 ************************************************************************/

int global_dof_func (int n)
{
    descriptor *arg1;
    descriptor *arg2;
    descriptor *result;
    descriptor	temp;
    int		type_error;
    int		status;
    int		dof;


    arg2 = pop ( );
    result = top ( );
    temp = *result;
    arg1 = &temp;


    status = 0;
    type_error = F_False;

    arg1 = CoerceData (deref (arg1), T_Int);
    arg2 = CoerceData (deref (arg2), T_Int);


    if (!problem.num_nodes) {
	rterror ("no FElt problem loaded in global_dof()");
	status = 1;

    } else if (!problem.num_dofs) {
	rterror ("no active DOFs in global_dof()");
	status = 1;

    } else if (D_Type (arg2) == T_Int) {
	dof = *D_Int (arg2);

	if (dof < 1 || dof > 6) {
	    rterror ("illegal DOF in global_dof()");
	    status = 1;
	} else if (D_Type (arg1) == T_Int) {
	    CreateData (result, NULL, NULL, T_Double);
	    *D_Double (result) = GlobalDOF (*D_Int (arg1), dof);
	} else if (D_Type (arg1) == T_Node) {
	    CreateData (result, NULL, NULL, T_Double);
	    *D_Double (result) = GlobalDOF ((*D_Node (arg1)) -> number, dof);
	} else
	    type_error = F_True;

    } else
	type_error = F_True;


    if (type_error == F_True)
	TypeError ("global_dof", arg1, arg2, NULL, F_True);


    RecycleData (arg1);
    RecycleData (arg2);

    return type_error == F_True || status != 0;
}


/************************************************************************
 * Function:	integrate_hyperbolic_func				*
 *									*
 * Description:	Interface to IntegrateHyperbolicDE().  The active DOFs	*
 *		must have previously been computed and the matrices	*
 *		must be of the proper size and either be compact	*
 *		matrices or be able to be compacted.  Additionally, the	*
 *		renumbering vector must be a proper permutation vector.	*
 *		The following types are legal:				*
 *									*
 *		integrate_hyperbolic (mtx, mtx, mtx)        -> matrix	*
 *		integrate_hyperbolic (mtx, mtx, mtx, array) -> matrix	*
 *									*
 *		An attempt is first made to coerce the first three	*
 *		arguments to matrices and the last argument to an array	*
 *		of integers.						*
 ************************************************************************/

int integrate_hyperbolic_func (int n)
{
    Matrix	K;
    Matrix	M;
    Matrix	C;
    Matrix	D;
    descriptor *arg1;
    descriptor *arg2;
    descriptor *arg3;
    descriptor *result;
    descriptor	temp;
    int		type_error;
    int		status;


    arg3 = pop ( );
    arg2 = pop ( );
    result = top ( );
    temp = *result;
    arg1 = &temp;


    status = 0;
    type_error = F_False;

    arg1 = CoerceData (deref (arg1), T_Matrix);
    arg2 = CoerceData (deref (arg2), T_Matrix);
    arg3 = CoerceData (deref (arg3), T_Matrix);


    if (!problem.num_nodes) {
	rterror ("no FElt problem loaded in integrate_hyperbolic()");
	status = 1;

    } else if (!problem.num_dofs) {
	rterror ("no active DOFs in integrate_hyperbolic()");
	status = 1;

    } else if (!check_analysis ("integrate_hyperbolic")) {
	status = 1;

    } else {
	if (D_Type (arg1) != T_Matrix)
	    type_error = F_True;
	else if (D_Type (arg2) != T_Matrix)
	    type_error = F_True;
	else if (D_Type (arg3) != T_Matrix)
	    type_error = F_True;
    }


    if (type_error == F_False && status == 0) {
	K = check_matrix ("integrate_hyperbolic", D_Matrix (arg1));
	M = K ? check_matrix ("integrate_hyperbolic", D_Matrix (arg2)) : NULL;
	C = K && M ? check_matrix ("integrate_hyperbolic", D_Matrix (arg3)) : NULL;

	if (K && M && C) {
	    if ((D = IntegrateHyperbolicDE (K, M, C))) {
		D_Type	  (result) = T_Matrix;
		D_Temp	  (result) = F_True;
		D_Trapped (result) = F_False;
		D_Matrix  (result) = D;
	    } else
		status = 1;
	} else
	    status = 1;

	if (K && D_Matrix (arg1) != K)
	    DestroyMatrix (K);

	if (M && D_Matrix (arg2) != M)
	    DestroyMatrix (M);

	if (C && D_Matrix (arg3) != C)
	    DestroyMatrix (C);
    }


    if (type_error == F_True)
	TypeError ("integrate_hyperbolic", arg1, arg2, arg3, F_True);


    RecycleData (arg1);
    RecycleData (arg2);
    RecycleData (arg3);

    return type_error == F_True || status != 0;
}


/************************************************************************
 * Function:	integrate_parabolic_func				*
 *									*
 * Description:	Interface to IntegrateParabolicDE().  The active DOFs	*
 *		must have previously been computed and the matrices	*
 *		must be of the proper size and either be compact	*
 *		matrices or be able to be compacted.  Additionally, the	*
 *		renumbering vector must be a proper permutation vector.	*
 *		The following types are legal:				*
 *									*
 *		integrate_parabolic (matrix, matrix)        -> matrix	*
 *		integrate_parabolic (matrix, matrix, array) -> matrix	*
 *									*
 *		An attempt is first made to coerce the first two	*
 *		arguments to matrices and the last argument to an array	*
 *		of integers.						*
 ************************************************************************/

int integrate_parabolic_func (int n)
{
    Matrix	K;
    Matrix	M;
    Matrix	D;
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

    arg1 = CoerceData (deref (arg1), T_Matrix);
    arg2 = CoerceData (deref (arg2), T_Matrix);


    if (!problem.num_nodes) {
	rterror ("no FElt problem loaded in integrate_parabolic()");
	status = 1;

    } else if (!problem.num_dofs) {
	rterror ("no active DOFs in integrate_parabolic()");
	status = 1;

    } else if (!check_analysis ("integrate_parabolic")) {
	status = 1;

    } else {
	if (D_Type (arg1) != T_Matrix)
	    type_error = F_True;
	else if (D_Type (arg2) != T_Matrix)
	    type_error = F_True;
    }


    if (type_error == F_False && status == 0) {
	K = check_matrix ("integrate_parabolic", D_Matrix (arg1));
	M = K ? check_matrix ("integrate_parabolic", D_Matrix (arg2)) : NULL;

	if (K && M) {
	    if ((D = IntegrateParabolicDE (K, M))) {
		D_Type	  (result) = T_Matrix;
		D_Temp	  (result) = F_True;
		D_Trapped (result) = F_False;
		D_Matrix  (result) = D;
	    } else
		status = 1;
	} else {
	    rterror ("compact matrices required in integrate_parabolic()");
	    status = 1;
	}

	if (K && D_Matrix (arg1) != K)
	    DestroyMatrix (K);

	if (M && D_Matrix (arg2) != M)
	    DestroyMatrix (M);
    }


    if (type_error == F_True)
	TypeError ("integrate_parabolic", arg1, arg2, NULL, F_True);


    RecycleData (arg1);
    RecycleData (arg2);

    return type_error == F_True || status != 0;
}


/************************************************************************
 * Function:	local_dof_func						*
 *									*
 * Description:	Interface to LocalDOF().  The active DOFs must have	*
 *		previously been computed and the global DOF must be in	*
 *		range.  The number of the node is returned instead of	*
 *		the node object since the nodes may have been		*
 *		renumbered.  The local DOF can be retrieved by		*
 *		specifying it as the remaining parameter.  The		*
 *		following types are legal:				*
 *									*
 *		local_dof (int)      -> int (node number)		*
 *		local_dof (int, int) -> int (node number)		*
 *									*
 *		An attempt is first made to coerce the first argument	*
 *		to an integer value.					*
 ************************************************************************/

int local_dof_func (int n)
{
    descriptor *arg1;
    descriptor *arg2;
    descriptor *result;
    descriptor	temp;
    unsigned	node;
    unsigned	local_dof;
    int		global_dof;
    int		type_error;
    int		status;


    arg2 = pop ( );
    result = top ( );
    temp = *result;
    arg1 = &temp;


    status = 0;
    type_error = F_False;
    arg1 = CoerceData (deref (arg1), T_Int);


    if (!assignable (arg2)) {
	RecycleData (arg2);
	arg2 = NULL;
    } else
	arg2 = deref (arg2);


    if (!problem.num_nodes) {
	rterror ("no FElt problem loaded in local_dof()");
	status = 1;

    } else if (!problem.num_dofs) {
	rterror ("no active DOFs in local_dof()");
	status = 1;

    } else if (D_Type (arg1) == T_Int) {
	global_dof = *D_Int (arg1);

	if (global_dof < 1 || global_dof > problem.num_nodes*problem.num_dofs) {
	    rterror ("illegal DOF in local_dof()");
	    status = 1;
	} else {
	    LocalDOF (global_dof, &node, &local_dof);
	    CreateData (result, NULL, NULL, T_Double);
	    *D_Double (result) = node;

	    if (arg2)
		status = AssignObject (arg2, T_Int, F_False, &local_dof);
	}
    } else
	type_error = F_True;


    if (type_error == F_True)
	TypeError ("local_dof", arg1, NULL, NULL, F_True);


    RecycleData (arg1);

    return type_error == F_True || status != 0;
}


/************************************************************************
 * Function:	remove_constrained_func					*
 *									*
 * Description:	Interface to RemoveConstrainedMatrixDOF().  The active	*
 *		DOFs must have previously been computed and the matrix	*
 *		must be of the proper size and either be compact or be	*
 *		able to be compacted.  The following types are legal:	*
 *									*
 *		remove_constrained (matrix) -> matrix			*
 *									*
 *		An attempt is first made to coerce the argument to a	*
 *		matrix.							*
 ************************************************************************/

int remove_constrained_func (int n)
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
    arg = CoerceData (deref (arg), T_Matrix);


    if (!problem.num_nodes) {
	rterror ("no FElt problem loaded in remove_constrained()");
	status = 1;

    } else if (!problem.num_dofs) {
	rterror ("no active DOFs in remove_constrained()");
	status = 1;

    } else if (D_Type (arg) == T_Matrix) {
	if ((a = check_matrix ("remove_constrained", D_Matrix (arg)))) {
	    if ((b = RemoveConstrainedMatrixDOF (a))) {
		D_Type	  (result) = T_Matrix;
		D_Temp	  (result) = F_True;
		D_Trapped (result) = F_False;
		D_Matrix  (result) = b;
	    } else
		status = 1;

	    if (a != D_Matrix (arg))
		DestroyMatrix (a);
	} else
	   status = 1;

    } else
	type_error = F_True;


    if (type_error == F_True)
	TypeError ("remove_constrained", arg, NULL, NULL, F_True);


    RecycleData (arg);
    return type_error == F_True || status != 0;
}


/************************************************************************
 * Function:	renumber_nodes_func					*
 *									*
 * Description:	Interface to RenumberNodes().  The return value is the	*
 *		permutation vector of node numbers.			*
 ************************************************************************/

int renumber_nodes_func (int n)
{
    unsigned   *v;
    unsigned	num_elts;
    unsigned	num_nodes;
    descriptor *result;
    int		handler;


    if (!problem.num_nodes) {
	rterror ("no FElt problem loaded in renumber_nodes()");
	return 1;
    }


    num_elts = problem.num_elements;
    num_nodes = problem.num_nodes;
    v = RenumberNodes (problem.nodes, problem.elements, num_nodes, num_elts);

    result = push ( );
    handler = AddTrap (strict_assignment);

    CreateData (result, NULL, NULL, T_Array, v, T_Int, num_nodes, handler);
    D_Trapped (result) = AddTrap (array_assignment);

    return 0;
}


/************************************************************************
 * Function:	restore_numbers_func					*
 *									*
 * Description:	Interface to RestoreNodeNumbers().  The array must be	*
 *		proper permutation vector.  The following types		*
 *		are legal:						*
 *									*
 *		restore_numbers (array) -> null (renumber nodes)	*
 *									*
 *		An attempt is first made to coerce the argument to an	*
 *		array of integer values.				*
 ************************************************************************/

int restore_numbers_func (int n)
{
    descriptor *arg;
    descriptor *result;
    descriptor	temp;
    unsigned   *ptr;
    int		type_error;
    int		status;


    result = top ( );
    temp = *result;
    arg = &temp;


    status = 0;
    type_error = F_False;
    arg = CoerceToArray (deref (arg), T_Int);


    if (!problem.num_nodes) {
	rterror ("no FElt problem loaded in restore_numbers()");
	status = 1;

    } else if (D_Type (arg) == T_Array) {
	if ((status = is_permutation ("restore_numbers", D_Array (arg)))) {
	    ptr = (unsigned *) D_Array (arg) -> ptr;
	    CreateData (result, NULL, NULL, T_Null);
	    RestoreNodeNumbers (problem.nodes, ptr, problem.num_nodes);
	}

    } else
	type_error = F_True;


    RecycleData (arg);

    return type_error == F_True || status != 0;
}


/************************************************************************
 * Function:	set_up_func						*
 *									*
 * Description:	Not available yet.					*
 ************************************************************************/

int set_up_func (int n)
{
    rterror ("set_up() function is not available");
    return 1;
}


/************************************************************************
 * Function:	solve_displacements_func				*
 *									*
 * Description:	Interface to SolveForDisplacements().  The active DOFs	*
 *		must have previously been computed and the matrices	*
 *		must be of the proper size and either be compact	*
 *		matrices of be able to be compacted.  The following	*
 *		types are legal:					*
 *									*
 *		solve_displacements (matrix, matrix) -> matrix		*
 *									*
 *		An attempt is first made to coerce both arguments to	*
 *		matrices.						*
 ************************************************************************/

int solve_displacements_func (int n)
{
    Matrix	K;
    Matrix	f;
    Matrix	d;
    descriptor *arg1;
    descriptor *arg2;
    descriptor *result;
    descriptor	temp;
    unsigned	size;
    int		type_error;
    int		status;


    arg2 = pop ( );
    result = top ( );
    temp = *result;
    arg1 = &temp;


    status = 0;
    type_error = F_False;

    arg1 = CoerceData (deref (arg1), T_Matrix);
    arg2 = CoerceData (deref (arg2), T_Matrix);


    if (!problem.num_nodes) {
	rterror ("no FElt problem loaded in solve_displacements()");
	status = 1;

    } else if (!problem.num_dofs) {
	rterror ("no active DOFs in solve_displacements()");
	status = 1;

    } else if (D_Type (arg1) == T_Matrix && D_Type (arg2) == T_Matrix) {
	if ((K = check_matrix ("solve_displacements", D_Matrix (arg1)))) {
	    f = D_Matrix (arg2);

	    size = problem.num_nodes * problem.num_dofs;

	    if (Mcols (f) != 1 || Mrows (f) != size) {
		MatrixError ("solve_displacements", f, NULL, M_SIZEMISMATCH, F_True);
		status = 1;
	    } else {
		f = CreateCopyMatrix (f);	/* f is overwritten with d */

		if ((d = SolveForDisplacements (K, f))) {
		    D_Type    (result) = T_Matrix;
		    D_Temp    (result) = F_True;
		    D_Trapped (result) = F_False;
		    D_Matrix  (result) = d;
		} else {
		    DestroyMatrix (f);
		    status = 1;
		}
	    }

	    if (K != D_Matrix (arg1))
		DestroyMatrix (K);

	} else
	    status = 1;

    } else
	type_error = F_True;


    if (type_error == F_True)
	TypeError ("solve_displacements", arg1, arg2, NULL, F_False);


    RecycleData (arg1);
    RecycleData (arg2);

    return type_error == F_True || status != 0;
}


/************************************************************************
 * Function:	zero_constrained_func					*
 *									*
 * Description:	Interface to ZeroConstrainedMatrixDOF().  The active	*
 *		DOFs must have previously been computed and the matrix	*
 *		must be of the proper size.  The following types are	*
 *		legal:							*
 *									*
 *		zero_constrained (col-vector) -> col_vector		*
 *		zero_constrained (matrix)     -> matrix			*
 *									*
 *		An attempt is first made to coerce the argument to a	*
 *		matrix.							*
 ************************************************************************/

int zero_constrained_func (int n)
{
    Matrix	a;
    descriptor *arg;
    descriptor *result;
    descriptor	temp;
    unsigned	size;
    int		type_error;
    int		status;


    result = top ( );
    temp = *result;
    arg = &temp;


    status = 0;
    type_error = F_False;
    arg = CoerceData (deref (arg), T_Matrix);


    if (!problem.num_nodes) {
	rterror ("no FElt problem loaded in zero_constrained()");
	status = 1;

    } else if (!problem.num_dofs) {
	rterror ("no active DOFs in zero_constrained()");
	status = 1;

    } else if (D_Type (arg) == T_Matrix) {
	a = D_Matrix (arg);
	size = problem.num_nodes * problem.num_dofs;

	if (IsColumnVector (a) && Mrows (a) == size) {
	    CreateData (result, arg, NULL, T_Matrix, size, 1);
	    ZeroConstrainedMatrixDOF (D_Matrix (result), a);

	} else if (IsSymmetricMatrix (a) && Mrows (a) == size) {
	    CreateData (result, arg, NULL, T_Matrix, size, size);
	    ZeroConstrainedMatrixDOF (D_Matrix (result), a);

	} else {
	    MatrixError ("zero_constrained", a, NULL, M_SIZEMISMATCH, F_True);
	    status = 1;
	}

    } else
	type_error = F_True;


    if (type_error == F_True)
	TypeError ("zero_constrained", arg, NULL, NULL, F_True);


    RecycleData (arg);
    return type_error == F_True || status != 0;
}
