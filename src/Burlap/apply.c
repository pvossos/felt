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
 * File:	apply.c							*
 *									*
 * Description:	This file contains the function definitions for the	*
 *		virtual machine instructions related to function and	*
 *		array application and function return.			*
 ************************************************************************/

# include "trap.h"
# include "apply.h"
# include "debug.h"
# include "error.h"
# include "lexer.h"
# include "coerce.h"
# include "execute.h"
# include "functab.h"
# include "our-stdlib.h"
# include "pathsearch.h"


static descriptor *ret_val;
static descriptor  ret_obj;
static Function	   top_of_stack;

static int array_index		PROTO ((descriptor *, int));
static int matrix_index		PROTO ((descriptor *, int));
static int intrinsic_call	PROTO ((descriptor *, int));
static int function_search	PROTO ((descriptor *, int));
static int external_call	PROTO ((descriptor *, int));
static int submatrix_assignment	PROTO ((descriptor *, descriptor **));


/************************************************************************
 * Function:	submatrix_assignment					*
 *									*
 * Description:	Trapped variable handler for submatrix assignment.  The	*
 *		handler verifies that the source is the same size and	*
 *		type as the destination submatrix.			*
 ************************************************************************/

static int submatrix_assignment (dest, src)
    descriptor  *dest;
    descriptor **src;
{
    Matrix	a;
    Matrix	b;
    int		status;


    /* RecycleData() calls the function with only one argument. */

    if (!src) {
	D_Temp    (dest) = F_True;
	D_Trapped (dest) = F_False;
	RecycleData (dest);
	return 0;
    }


    *src = CoerceData (*src, T_Double);

    switch (D_Type (*src)) {
    case T_Double:
	a = D_Matrix (dest);
	if (Mrows (a) == 1 && Mcols (a) == 1) {
	    sdata (a, 1, 1) = *D_Double (*src);
	    return 0;
	}

	TypeError ("=", dest, *src, NULL, F_False);
	return 1;


    case T_Matrix:
	a = D_Matrix (*src);
	b = D_Matrix (dest);
	if ((status = CopyMatrix (b, a)))
	    MatrixError ("=", b, a, status, F_False);
	return status;


    default:
	TypeError ("=", dest, *src, NULL, F_False);
	return 1;
    }
}


/************************************************************************
 * Function:	function_call						*
 *									*
 * Description:	Calls the function whose name and arguments are on the	*
 *		stack.  After the call returns the name and arguments	*
 *		are removed from the stack and the return value is	*
 *		placed on the top of the stack.  If too few arguments	*
 *		are given to the function then null descriptors on	*
 *		pushed in their place.  If too many arguments are given	*
 *		then they are silently discarded.  All value arguments	*
 *		are dereferenced in place on the stack.			*
 ************************************************************************/

int function_call (object, num_args)
    descriptor *object;
    int		num_args;
{
    int		i;
    int		j;
    descriptor *d;
    descriptor *fp;
    descriptor *arg;
    descriptor *args;
    descriptor *locals;
    descriptor *result;
    descriptor  temp;
    Function	function;
    Function	old_top;


    fp = sp - num_args;
    args = sp - num_args + 1;
    function = D_Function (object);

    old_top = top_of_stack;
    top_of_stack = function;


    /* Remove any extra arguments. */

    if (function -> num_args < num_args)
	for (i = function -> num_args; i < num_args; i ++)
	    RecycleData (pop ( ));


    /* Add any necessary arguments. */

    else if (function -> num_args > num_args)
	for (i = num_args; i < function -> num_args; i ++) {
	    arg = push ( );
	    D_Type    (arg) = T_Null;
	    D_Temp    (arg) = F_False;
	    D_Trapped (arg) = F_False;
	    D_Pointer (arg) = NULL;
	}


    /* Dereference any value arguments in place on the stack. */

    for (i = function -> num_args - 1, j = 0; i >= 0; i --, j ++) {
	arg = ntop (i);


	/* If the argument is a value argument then make a copy of it in
	   place on the stack. */

	if (function -> arg_types [j] == ValueArg) {
	    d = &temp;
	    *d = *arg;
	    d = deref (d);

	    D_Type    (arg) = T_Null;
	    D_Temp    (arg) = F_False;
	    D_Trapped (arg) = F_False;
	    D_Pointer (arg) = NULL;
	    AssignData (arg, &d);


	/* Otherwise, the argument is shared.  If the argument is not
	   assignable and is not temporary then we have to copy it. */

	} else if (!assignable (arg) && D_Temp (arg) == F_False) {
	    d = &temp;
	    *d = *arg;
	    d = deref (d);

	    D_Type    (arg) = T_Null;
	    D_Temp    (arg) = F_False;
	    D_Trapped (arg) = F_False;
	    D_Pointer (arg) = NULL;
	    AssignData (arg, &d);
	}

	D_Temp    (arg) = F_False;
	D_Trapped (arg) = F_False;
    }


    /* Push null descriptors for the local variables. */

    locals = sp + 1;
    for (i = 0; i < function -> num_locals; i ++) {
	arg = push ( );
	D_Type    (arg) = T_Null;
	D_Temp    (arg) = F_False;
	D_Trapped (arg) = F_False;
	D_Pointer (arg) = NULL;
    }


    /* Call the function.  If the function fails then show the stack. */

    if (execute (function -> cs, locals, args) != -1) {
	fprintf (stderr, "%s:%u: ", *strlit (curr_file_num), curr_line_num);
	fprintf (stderr, "%s (", function -> name);
	for (i = 0; i < function -> num_args; i ++)
	    fprintf (stderr, "%s%s", i ? "," : "", D_TypeName (args + i));
	fprintf (stderr, ")\n");
	top_of_stack = old_top;
	return 1;
    }

    top_of_stack = old_top;

    d_printf ("freeing locals: ");
    d_printf ("%d + %d\n", function -> num_args, function -> num_locals);
    for (i = 0; i < function -> num_args + function -> num_locals; i ++)
	FreeData (pop ( ));

    RecycleData (pop ( ));

    result = push ( );
    *result = *ret_val;

    d_printf ("rtn ans =\n");
    d_PrintData (result);

    return 0;
}


/************************************************************************
 * Function:	intrinsic_call						*
 *									*
 * Description:	Calls the intrinsic function whose index and arguments	*
 *		are on the top of the stack.  The parameter list is	*
 *		adjusted as in the case of a normal function call.	*
 *		However, a special case is made since an intrinsic	*
 *		function may take a variable number of arguments.  The	*
 *		arguments are not dereferenced.				*
 ************************************************************************/

static int intrinsic_call (object, num_args)
    descriptor *object;
    int		num_args;
{
    int		i;
    int		nargs;
    int		index;
    int		num_passed;
    descriptor *arg;
    descriptor *result;



    /* If the function accepts a fixed number of arguments then set up the
       stack by pushing or popping extra descriptors. */

    if ((nargs = functab [index = D_Intrinsic (object)].num_args) >= 0) {


	/* Remove any extra arguments. */

	if (nargs < num_args)
	    for (i = nargs; i < num_args; i ++)
		RecycleData (pop ( ));


	/* Add any necessary arguments. */

	else if (nargs > num_args)
	    for (i = num_args; i < nargs; i ++) {
		arg = push ( );
		D_Type    (arg) = T_Null;
		D_Temp    (arg) = F_False;
		D_Trapped (arg) = F_False;
		D_Pointer (arg) = NULL;
	    }

	num_passed = nargs;

    } else
	num_passed = num_args;


    /* Call the intrinsic function. */

    if (functab [index].func (num_passed))
	return 1;

    ret_val = pop ( );
    RecycleData (pop ( ));

    result = push ( );
    *result = *ret_val;
    return 0;
}


/************************************************************************
 * Function:	matrix_index						*
 *									*
 * Description:	Indexes the descriptor whose indices are on the top of	*
 *		the stack and places the result on the stack.  The	*
 *		following types and indices are legal, where a vector	*
 *		is a matrix with either a single row or column:		*
 *									*
 *		scalar     (1)   -> scalar				*
 *		scalar     (1,1) -> scalar				*
 *		row-vector (x)   -> row-vector				*
 *		row_vector (x,1) -> row-vector				*
 *		col-vector (x)   -> col-vector				*
 *		col-vector (1,x) -> col-vector				*
 *		matrix     (x)   -> col-vector				*
 *		matrix     (x,y) -> matrix				*
 *									*
 *		An index must either be a scalar or a vector whose	*
 *		elements are contiguous and increasing.  The result	*
 *		will be a trapped variable.  The trap handler will	*
 *		verify that the object to be assigned must be of the	*
 *		same type and size.					*
 ************************************************************************/

static int matrix_index (object, num_args)
    descriptor *object;
    int		num_args;
{
    Matrix	a;
    Matrix	b;
    Matrix	index;
    descriptor *arg2;
    descriptor *arg1;
    descriptor *result;
    descriptor *orig;
    descriptor	temp;
    double	value;
    int		type_error;
    int		status;
    unsigned	i;
    unsigned	dim;
    unsigned	nrows;
    unsigned	ncols;
    unsigned	s_row;
    unsigned	s_col;
    unsigned	e_row;
    unsigned	e_col;


    /* Check the number of arguments. */

    if (num_args == 0 || num_args > 2) {
	rterror ("incorrect number of indices for %s", D_TypeName (object));
	return 1;
    }


    /* Set up the stack. */

    arg2 = num_args == 2 ? pop ( ) : NULL;
    arg1 = pop ( );
    result = top ( );
    temp = *result;
    object = &temp;
    orig = object;
    object = deref (object);


    /* Initialize the arguments. */

    status = 0;
    type_error = F_False;
    arg1 = CoerceData (deref (arg1), T_Double);
    if (arg2)
	arg2 = CoerceData (deref (arg2), T_Double);

    s_row = e_row = s_col = e_col = 0;


    /* Compute the starting and ending row indices. */

    switch (D_Type (arg1)) {
    case T_Double:
	s_row = e_row = *D_Double (arg1);
	break;


    case T_Matrix:
	index = D_Matrix (arg1);
	if (Mrows (index) == 1) {
	    s_row = mdata (index, 1, 1);
	    e_row = mdata (index, 1, Mcols (index));
	    for (i = 2; i <= Mcols (index); i ++)
		if (mdata (index, 1, i) != s_row + i - 1) {
		    rterror ("improper row index in index expression");
		    status = 1;
		    break;
		}

	} else if (Mcols (index) == 1) {
	    s_row = mdata (index, 1, 1);
	    e_row = mdata (index, Mrows (index), 1);
	    for (i = 2; i <= Mrows (index); i ++)
		if (mdata (index, i, 1) != s_row + i - 1) {
		    rterror ("improper row index in index expression");
		    status = 1;
		    break;
		}

	} else {
	    rterror ("matrix used as row index in index expression");
	    status = 1;
	}
	break;


    case T_Row:
	if (D_Type (object) == T_Double)
	    s_row = e_row = 1;
	else if (D_Type (object) == T_Matrix) {
	    s_row = 1;
	    e_row = Mrows (D_Matrix (object));
	}
	break;


    default:
	TypeError ("in index expression", arg1, NULL, NULL, F_False);
	type_error = F_True;
	break;
    }


    /* Compute the starting and ending column indices. */

    if (arg2 && type_error == F_False && status == 0) {
	switch (D_Type (arg2)) {
	case T_Double:
	    s_col = e_col = *D_Double (arg2);
	    break;


	case T_Matrix:
	    index = D_Matrix (arg2);
	    if (Mrows (index) == 1) {
		s_col = mdata (index, 1, 1);
		e_col = mdata (index, 1, Mcols (index));
		for (i = 2; i <= Mcols (index); i ++)
		    if (mdata (index, 1, i) != s_col + i - 1) {
			rterror ("improper column index in index expression");
			status = 1;
			break;
		    }

	    } else if (Mcols (index) == 1) {
		s_col = mdata (index, 1, 1);
		e_col = mdata (index, Mrows (index), 1);
		for (i = 2; i <= Mrows (index); i ++)
		    if (mdata (index, i, 1) != s_col + i - 1) {
			rterror ("improper column index in index expression");
			status = 1;
			break;
		    }

	    } else {
		rterror ("matrix used as column index in index expression");
		status = 1;
	    }
	    break;


	case T_Row:
	    if (D_Type (object) == T_Double)
		s_col = e_col = 1;
	    else if (D_Type (object) == T_Matrix) {
		s_col = 1;
		e_col = Mcols (D_Matrix (object));
	    }
	    break;


	default:
	    TypeError ("in index expression", arg2, NULL, NULL, F_False);
	    type_error = F_True;
	    break;
	}
    } else
	s_col = e_col = 1;


    /* Perform the indexing. */

    if (type_error == F_False && status == 0) {
	switch (D_Type (object)) {
	case T_Int:
	case T_Byte:
	case T_Double:
	    if (s_row != 1 || e_row != 1) {
		rterror ("row index is out of range (must be one)");
		status = 1;

	    } else if (s_col != 1 || e_col != 1) {
		rterror ("column index is out of range (must be one)");
		status = 1;


	    /* If the object cannot be assignable then we can just copy the
	       object. */

	    } else if (!D_Trapped (object) && D_Type (orig) != T_Variable) {
		CreateData (result, NULL, NULL, T_Double);
		*D_Double (result) = *D_Double (object);


	    /* If the object is not trapped then change the double into a
	       matrix with one row and column.  The result is a submatrix of
	       the new matrix.  This way, the matrix library does the
	       reference counting for us. */

	    } else if (D_Trapped (object) == F_False) {
		value = *D_Double (object);
		FreeData (object);
		CreateData (object, NULL, NULL, T_Matrix, 1, 1);

		a = D_Matrix (object);
		sdata (a, 1, 1) = value;
		D_Temp (object) = F_False;

		b = CreateSubsectionMatrix (a, 1, 1, 1, 1);
		D_Type	  (result) = T_Matrix;
		D_Temp	  (result) = F_False;
		D_Trapped (result) = AddTrap (submatrix_assignment);
		D_Matrix  (result) = b;


	    /* Otherwise, we don't need to do anything (I think).  Note that
	       all bytes and integers are trapped, so this case will be
	       executed. */

	    } else {
		/* Result should be the object itself, as located on the
		   interpreter stack, as opposed to our stack. */
	    }
	    break;


	case T_Matrix:
	    a = D_Matrix (object);
	    nrows = Mrows (a);
	    ncols = Mcols (a);


	    /* Handle a missing second argument. */

	    if (!arg2)


		/* If only one argument was specified and the object is a row
		   vector then swap the indices. */

		if (Mrows (a) == 1) {
		    dim = s_row;
		    s_row = s_col;
		    s_col = dim;
		    dim = e_row;
		    e_row = e_col;
		    e_col = dim;


		/* If only one argument was specified and the object is a
		   matrix then return the entire column. */

		} else if (Mcols (a) != 1) {
		    s_col = s_row;
		    e_col = e_row;
		    s_row = 1;
		    e_row = Mrows (a);
		}


	    /* Check the indices. */

	    if (s_row < 1 || s_row > nrows) {
		rterror ("row index is out of range (1 .. %u)", nrows);
		status = 1;
	    } else if (e_row < s_row || e_row > nrows) {
		rterror ("row index is out of range (1 .. %u)", nrows);
		status = 1;
	    } else if (s_col < 1 || s_col > ncols) {
		rterror ("column index is out of range (1 .. %u)", ncols);
		status = 1;
	    } else if (e_col < s_col || e_col > ncols) {
		rterror ("column index is out of range (1 .. %u)", ncols);
		status = 1;

	    } else {


		/* Compute the subsection. */

		if (IsCompact (a)) {
		    b = MakeFullFromCompact (a);
		    DestroyMatrix (a);
		    D_Matrix (object) = a = b;
		}

		b = CreateSubsectionMatrix (a, s_row, s_col, e_row, e_col);

		if (b) {
		    D_Type    (result) = T_Matrix;
		    D_Temp    (result) = F_False;
		    D_Trapped (result) = AddTrap (submatrix_assignment);
		    D_Matrix  (result) = b;
		} else
		    status = 1;
	    }
	    break;
	}
    }


    /* Clean up and return. */

    RecycleData (arg2);
    RecycleData (arg1);
    RecycleData (object);
    d_printf ("index ans =\n");
    d_PrintData (result);

    return type_error == F_True || status != 0;
}


/************************************************************************
 * Function:	array_index						*
 *									*
 * Description:	Indexes the array descriptor whose index is on the top	*
 *		of the stack and places the result on the stack.  The	*
 *		following types are legal:				*
 *									*
 *		array (double) -> element (array indexing)		*
 *		array (vector) -> array   (subarray creation)		*
 *									*
 *		An attempt is first made to coerce the index to a	*
 *		double value.						*
 ************************************************************************/

static int array_index (object, num_args)
    descriptor *object;
    int		num_args;
{
    Array	array;
    Matrix	index;
    void       *ptr;
    descriptor *arg;
    descriptor *result;
    descriptor	temp;
    unsigned	i;
    unsigned	end;
    unsigned	start;
    int		len;
    int		type;
    int		handler;
    int		type_error;
    int		status;


    /* Check the number of arguments. */

    if (num_args != 1) {
	rterror ("incorrect number of indices for array");
	return 1;
    }


    /* Set up the stack. */

    arg = pop ( );
    result = top ( );
    temp = *result;
    object = &temp;
    object = deref (object);


    status = 0;
    type_error = F_False;

    start = end = 0;
    arg = CoerceData (deref (arg), T_Double);
    array = D_Array (object);


    switch (D_Type (arg)) {
    case T_Double:
	start = end = *D_Double (arg);
	break;


    case T_Matrix:
	index = D_Matrix (arg);
	if (Mrows (index) == 1) {
	    start = mdata (index, 1, 1);
	    end = mdata (index, 1, Mcols (index));
	    for (i = 2; i <= Mcols (index); i ++)
		if (mdata (index, 1, i) != start + i - 1) {
		    rterror ("improper array index in index expression");
		    status = 1;
		    break;
		}

	} else if (Mcols (index) == 1) {
	    start = mdata (index, 1, 1);
	    end = mdata (index, Mrows (index), 1);
	    for (i = 2; i <= Mrows (index); i ++)
		if (mdata (index, i, 1) != start + i - 1) {
		    rterror ("improper array index in index expression");
		    status = 1;
		    break;
		}

	} else {
	    rterror ("matrix used as array index in index expression");
	    status = 1;
	}
	break;


    case T_Row:
	start = 1;
	end = array -> length;
	break;


    default:
	type_error = F_True;
	break;
    }


    if (type_error == F_False && status == 0) {
	if (array -> length == 0) {
	    rterror ("index is out of range (no valid array members)");
	    status = 1;
	} else if (start < 1 || start > array -> length) {
	    rterror ("index is out of range (1 .. %u)", array -> length);
	    status = 1;
	} else if (end < start || end > array -> length) {
	    rterror ("index is out of range (1 .. %u)", array -> length);
	    status = 1;

	} else if (start == end) {
	    ptr = (void *) ((char *) array -> ptr + start * array -> elt_size);
	    D_Type    (result) = array -> type;
	    D_Temp    (result) = F_False;
	    D_Trapped (result) = array -> handler;
	    D_Pointer (result) = ptr;

	} else {
	    start --;	/* all arrays are unit offset */
	    ptr = (void *) ((char *) array -> ptr + start * array -> elt_size);
	    type = array -> type;
	    len = end - start;
	    handler = array -> handler;
	    CreateData (result, NULL, NULL, T_Array, ptr, type, len, handler);
	    D_Array (result) -> elt_size = array -> elt_size;
	    D_Trapped (result) = D_Trapped (object);
	}
    }


    if (type_error == F_True)
	TypeError ("in index expression", arg, NULL, NULL, F_False);


    RecycleData (arg);
    RecycleData (object);
    d_printf ("index ans =\n");
    d_PrintData (result);

    return type_error == F_True || status != 0;
}


/************************************************************************
 * Function:	function_search						*
 *									*
 * Description:	Searches the global symbol table for a symbol with the	*
 *		same name as the specified null descriptor.  If such a	*
 *		symbol exists and is a function, then it is used as the	*
 *		basis for a function call.  Otherwise, if there is a	*
 *		file in the include-search path with the same name then	*
 *		it is included and the search is made again in the	*
 *		global symbol table.					*
 ************************************************************************/

static int function_search (object, num_args)
    descriptor *object;
    int		num_args;
{
    int		 idx;
    ste		*s;
    char	*name;
    char	*file;
    descriptor	*d;
    static char *path;


    if (D_Type (object) != T_Variable) {
	TypeError ("function call to", NULL, object, NULL, F_False);
	return 1;
    }

    idx = D_Variable (object) - varp;

    if (idx < 0 || (top_of_stack && idx > top_of_stack -> num_locals)) {
	TypeError ("function call to", NULL, object, NULL, F_False);
	return 1;
    }
	
    if (top_of_stack) {
	name = top_of_stack -> local_names [idx];
	if ((s = st_lookup (&var_st, name))) {
	    d = global (s -> idx);
	    d = deref (d);

	    switch (D_Type (d)) {
	    case T_Function:
		return function_call (d, num_args);


	    case T_Intrinsic:
		return intrinsic_call (d, num_args);


	    default:
		d = deref (d);
		TypeError ("function call to", NULL, d, NULL, F_False);
		return 1;
	    }
	}
    } else
	name = st_index (&var_st, idx) -> name;


    if (!path)
	path = getenv ("BURLAP_PATH");

    if ((file = pathsearch (path, name, ".b", F_False))) {
	printf ("including %s\n", file);
	bfinclude (file);
	if ((s = st_lookup (&var_st, name))) {
	    d = global (s -> idx);
	    d = deref (d);

	    switch (D_Type (d)) {
	    case T_Function:
		return function_call (d, num_args);


	    case T_Intrinsic:
		return intrinsic_call (d, num_args);


	    default:
		d = deref (d);
		TypeError ("function call to", NULL, d, NULL, F_False);
		return 1;
	    }
	}
    }

    TypeError ("function call to", NULL, deref (object), NULL, F_False);
    return 1;
}


/************************************************************************
 * Function:	external_call						*
 *									*
 * Description:	Calls the external C function pointed to by the		*
 *		specified descriptor.  This mechanism is used to call	*
 *		the set-up and stress functions of the built-in FElt	*
 *		elements.  The first argument must be an element and	*
 *		the second argument, if specified, must be an integer.	*
 ************************************************************************/

static int external_call (object, num_args)
    descriptor *object;
    int		num_args;
{
    void      **ptr;
    descriptor *arg1;
    descriptor *arg2;
    descriptor *result;
    descriptor	temp;
    int		type_error;
    int		mode;
    int		i;


    if (num_args < 1) {
	rterror ("incorrect number of arguments for %s", D_TypeName (object));
	return 1;
    }

    for (i = 2; i < num_args; i ++)
	RecycleData (pop ( ));

    arg2 = num_args >= 2 ? pop ( ) : NULL;
    result = top ( );
    temp = *result;
    arg1 = &temp;


    type_error = F_False;
    arg1 = CoerceData (deref (arg1), T_Double);

    if (arg2)
	arg2 = CoerceData (deref (arg2), T_Int);


    if (D_Type (arg1) == T_Element) {
	CreateData (result, NULL, NULL, T_Double);
	ptr = (void **) D_Pointer (arg1);
	mode = arg2 && D_Type (arg2) == T_Int ? *D_Int (arg2) : 0;
	*D_Double (result) = (*D_External (object)) (*ptr, mode);
    } else
	type_error = F_True;


    if (type_error == F_True)
	TypeError ("in C function call", arg1, arg2, NULL, F_False);


    RecycleData (arg1);
    RecycleData (arg2);

    return type_error == F_True;
}


/************************************************************************
 * Function:	apply_op						*
 *									*
 * Description:	Pops the descriptors on the top of the stack and	*
 *		performs a function or array application.  The		*
 *		following types are legal:				*
 *									*
 *		null      (...)  (function search)			*
 *		array     (...)  (array indexing)			*
 *		double    (...)  (scalar indexing)			*
 *		matrix    (...)  (matrix indexing)			*
 *		function  (...)  (user-defined function call)		*
 *		intrinsic (...)  (intrinsic function call)		*
 *		external  (...)  (external C function call)		*
 *									*
 *		No coercion is performed since the result of an index	*
 *		is a variable.						*
 ************************************************************************/

int apply_op ( )
{
    int		num_args;
    descriptor *object;


    num_args = fetch (pc ++).ival;
    object = ntop (num_args);
    object = deref (object);


    switch (D_Type (object)) {
    case T_Null:
	return function_search (ntop (num_args), num_args);


    case T_Array:
	return array_index (object, num_args);


    case T_Int:
    case T_Byte:
    case T_Double:
    case T_Matrix:
	return matrix_index (object, num_args);


    case T_Function:
	return function_call (object, num_args);


    case T_Intrinsic:
	return intrinsic_call (object, num_args);


    case T_External:
	return external_call (object, num_args);


    default:
	TypeError ("function call to", NULL, object, NULL, F_False);
	return 1;
    }
}


/************************************************************************
 * Function:	rtn_op							*
 *									*
 * Description:	Returns from a function call.  If the descriptor on the	*
 *		top of the stack is not a global variable (i.e. it is	*
 *		local to the current function) then the descriptor is	*
 *		dereferenced.  The descriptor is saved in a static	*
 *		location and a termination code is returned so that	*
 *		the current stack frame will be erased and execution	*
 *		will return to the caller.				*
 ************************************************************************/

int rtn_op ( )
{
    descriptor *d;


    d = pop ( );
    ret_val = &ret_obj;

    if (!is_global (d)) {
	d_printf ("returning =\n");
	d_PrintData (d);
	d = deref (d);
	D_Temp (d) = F_False;
	D_Type     (ret_val) = T_Null;
	D_Temp     (ret_val) = F_False;
	D_Trapped  (ret_val) = F_False;
	D_Pointer  (ret_val) = NULL;
	AssignData (ret_val, &d);
	D_Temp     (ret_val) = F_True;
    } else
	*ret_val = *d;

    d_printf ("returning = %p (%s)\n", D_Pointer (d), D_TypeName (d));
    d_printf ("rtn\n");
    return -1;
}
