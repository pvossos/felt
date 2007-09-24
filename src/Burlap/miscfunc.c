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
 * File:	miscfunc.c						*
 *									*
 * Description:	This file contains the function definitions for various	*
 *		miscellaneous intrinsic functions.			*
 ************************************************************************/

# include <errno.h>
# include <string.h>
# include <stdlib.h>
# include "debug.h"
# include "error.h"
# include "lexer.h"
# include "coerce.h"
# include "execute.h"
# include "miscfunc.h"
# include "our-stdlib.h"
# include "pathsearch.h"
# include "interactive.h"

# ifndef ReadBufferSize
# define ReadBufferSize 4096
# endif


/************************************************************************
 * Function:	concat_func						*
 *									*
 * Description:	Pops and concatenates the two descriptors on the top of	*
 *		the stack and places the result on the stack.  The	*
 *		following types are legal for concatenation:		*
 *									*
 *		concat (string, string) -> string (concatenation)	*
 *									*
 *		An attempt is first made to coerce both arguments to	*
 *		string values.						*
 ************************************************************************/

int concat_func (n)
    int n;
{
    char       *s1;
    char       *s2;
    descriptor *arg1;
    descriptor *arg2;
    descriptor *result;
    descriptor	temp;
    int		type_error;
    int		length;


    arg2 = pop ( );
    result = top ( );
    temp = *result;
    arg1 = &temp;


    type_error = F_False;
    arg1 = CoerceData (deref (arg1), T_String);
    arg2 = CoerceData (deref (arg2), T_String);


    if (D_Type (arg1) == T_String && D_Type (arg2) == T_String) {
	s1 = *D_String (arg1);
	s2 = *D_String (arg2);
	length = strlen (s1) + strlen (s2) + 1;
	CreateData (result, NULL, NULL, T_String, length);
	strcpy (*D_String (result), s1);
	strcat (*D_String (result), s2);
    } else
	type_error = F_True;


    if (type_error == F_True)
	TypeError ("concat", arg1, arg2, NULL, F_True);


    RecycleData (arg1);
    RecycleData (arg2);
    d_printf ("concat ans =\n");
    d_PrintData (result);

    return type_error == F_True;
}


/************************************************************************
 * Function:	eval_func						*
 *									*
 * Description:	Not available yet.					*
 ************************************************************************/

int eval_func (n)
    int n;
{
    rterror ("eval() function is not available");
    return 1;
}


/************************************************************************
 * Function:	exit_func						*
 *									*
 * Description:	Pops the descriptor on the top of the stack and uses it	*
 *		as an exit value for the program.  If a null argument	*
 *		is given the zero is used as the return value.  The	*
 *		following types are legal:				*
 *									*
 *		exit ()	       -> nothing (program termination)		*
 *		exit (integer) -> nothing (program termination)		*
 *									*
 *		An attempt is first made to coerce the argument to an	*
 *		integer value.						*
 ************************************************************************/

int exit_func (n)
    int n;
{
    descriptor *arg;
    descriptor *result;
    descriptor	temp;
    int		type_error;


    result = top ( );
    temp = *result;
    arg = &temp;


    type_error = F_False;
    arg = CoerceData (deref (arg), T_Int);


    switch (D_Type (arg)) {
    case T_Null:
	exit (0);


    case T_Int:
	exit (*D_Int (arg));


    default:
	type_error = F_True;
	break;
    }


    RecycleData (arg);
    return type_error == F_True;
}


/************************************************************************
 * Function:	history_func						*
 *									*
 * Description:	Pops the descriptor on the top of the stack and uses it	*
 *		as the number of history items to print.  The following	*
 *		types are legal:					*
 *									*
 *		history (null)	 -> double (number of items printed)	*
 *		history (double) -> double (number of items printed)	*
 *									*
 *		An attempt is first made to coerce the argument to a	*
 *		double value.						*
 ************************************************************************/

int history_func (n)
    int n;
{
    descriptor *arg;
    descriptor *result;
    descriptor	temp;
    int		type_error;


    result = top ( );
    temp = *result;
    arg = &temp;


    type_error = F_False;
    arg = CoerceData (deref (arg), T_Double);


    switch (D_Type (arg)) {
    case T_Double:
	CreateData (result, arg, NULL, T_Double);
	*D_Double (result) = print_history ((int) *D_Double (arg));
	break;


    case T_Null:
	CreateData (result, NULL, NULL, T_Double);
	*D_Double (result) = print_history (0);
	break;


    default:
	type_error = F_True;
	break;
    }


    if (type_error == F_True)
	TypeError ("history", arg, NULL, NULL, F_True);


    RecycleData (arg);
    return type_error == F_True;
}


/************************************************************************
 * Function:	include_func						*
 *									*
 * Description:	Pops the descriptor on the top of the stack and treats	*
 *		it as the name of a file to include.  The path named by	*
 *		the BURLAP_PATH environment variable is searched for	*
 *		the file.  The result is the full path name of the file	*
 *		that was included.  The following types are legal:	*
 *									*
 *		include (string) -> string (file inclusion)		*
 *									*
 *		An attempt is first made to coerce the argument to a	*
 *		string value.						*
 ************************************************************************/

int include_func (n)
    int n;
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


    if (D_Type (arg) == T_String) {
	if (!path)
	    path = getenv ("BURLAP_PATH");

	name = pathsearch (path, *D_String (arg), ".b", F_True);
	CreateData (result, NULL, NULL, T_String, strlen (name) + 1);
	strcpy (*D_String (result), name);
	RecycleData (arg);

	if (bfinclude (name)) {
	    rterror ("unable to include '%s'", name);
	    **D_String (result) = 0;
	}

    } else {
	TypeError ("include", arg, NULL, NULL, F_True);
	type_error = F_True;
	RecycleData (arg);
    }

    return type_error == F_True;
}


/************************************************************************
 * Function:	load_func						*
 *									*
 * Description:	Not available yet.					*
 ************************************************************************/

int load_func (n)
    int n;
{
    rterror ("load() function is not available");
    return 1;
}


/************************************************************************
 * Function:	read_func						*
 *									*
 * Description:	Reads a line from standard input, creates a string	*
 *		descriptor from it, and places the result on the stack.	*
 *		A null descriptor is returned upon end-of-file.		*
 ************************************************************************/

int read_func (n)
    int n;
{
    descriptor *result;
    char       *ptr;
    char	buffer [ReadBufferSize];


    if (fgets (buffer, sizeof (buffer), stdin) != NULL) {
	result = push ( );
	if (*(ptr = &buffer [strlen (buffer) - 1]) == '\n')
	    *ptr = 0;
	CreateData (result, NULL, NULL, T_String, strlen (buffer));
	strcpy (*D_String (result), buffer);

    } else {
	result = push ( );
	D_Type	  (result) = T_Null;
	D_Temp	  (result) = F_False;
	D_Trapped (result) = F_False;
	D_Pointer (result) = NULL;
    }


    d_printf ("read ans =\n");
    d_PrintData (result);

    return 0;
}


/************************************************************************
 * Function:	reads_func						*
 *									*
 * Description:	Reads a string from standard input, creates a string	*
 *		descriptor from it, and places the result on the stack.	*
 *		A null descriptor is returned upon end-of-file.		*
 ************************************************************************/

int reads_func (n)
    int n;
{
    descriptor *result;
    char	buffer [ReadBufferSize];


    if (scanf ("%s", buffer) != EOF) {
	result = push ( );
	CreateData (result, NULL, NULL, T_String, strlen (buffer));
	strcpy (*D_String (result), buffer);

    } else {
	result = push ( );
	D_Type	  (result) = T_Null;
	D_Temp	  (result) = F_False;
	D_Trapped (result) = F_False;
	D_Pointer (result) = NULL;
    }


    d_printf ("reads ans =\n");
    d_PrintData (result);

    return 0;
}


/************************************************************************
 * Function:	save_func						*
 *									*
 * Description:	Not available yet.					*
 ************************************************************************/

int save_func (n)
    int n;
{
    rterror ("save() function is not available");
    return 1;
}


/************************************************************************
 * Function:	system_func						*
 *									*
 * Description:	Pops the descriptor on the top of the stack and uses it	*
 *		as input to the system() function to execute a command.	*
 *		The result is the return value of the system() function	*
 *		and is pushed on the stack as a double value.  The	*
 *		following types are legal:				*
 *									*
 *		system (string) -> double (command invocation)		*
 *									*
 *		An attempt is first made to coerce the argument to a	*
 *		string value.						*
 ************************************************************************/

int system_func (n)
    int n;
{
    descriptor *arg;
    descriptor *result;
    descriptor	temp;
    int		type_error;


    result = top ( );
    temp = *result;
    arg = &temp;


    type_error = F_False;
    arg = CoerceData (deref (arg), T_String);


    if (D_Type (arg) == T_String) {
	CreateData (result, NULL, NULL, T_Double);
	*D_Double (result) = system (*D_String (arg));
    } else
	type_error = F_True;


    if (type_error == F_True)
	TypeError ("system", arg, NULL, NULL, F_True);


    RecycleData (arg);
    return type_error == F_True;
}


/************************************************************************
 * Function:	type_func						*
 *									*
 * Description:	Pops and computes the type of the descriptor on the top	*
 *		of the stack and places the result on the stack.  The	*
 *		result is a string descriptor containing the name of	*
 *		type of the argument.					*
 ************************************************************************/

int type_func (n)
    int n;
{
    descriptor *arg;
    descriptor *result;
    descriptor	temp;


    result = top ( );
    temp = *result;
    arg = &temp;

    arg = deref (arg);
    arg = CollapseMatrix (arg);

    CreateData (result, NULL, NULL, T_String, strlen (D_TypeName (arg) + 1));
    strcpy (*D_String (result), D_TypeName (arg));


    RecycleData (arg);
    d_printf ("type ans =\n");
    d_PrintData (result);

    return 0;
}
