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
 * File:	execute.c						*
 *									*
 * Description:	This file contains the public and private function and	*
 *		variable definitions for execution of the virtual	*
 *		machine code.						*
 ************************************************************************/

# include <string.h>
# include <stdarg.h>
# include "debug.h"
# include "error.h"
# include "status.h"
# include "execute.h"
# include "exectab.h"

# ifndef StackSize
# define StackSize 4096
# endif


static descriptor the_stack [StackSize];

Address	    pc;
descriptor *varp;
descriptor *argp = the_stack;
descriptor *sp = the_stack;
descriptor *stack = the_stack;


/************************************************************************
 * Function:	SaveState						*
 *									*
 * Description:	Saves the current state of the virtual machine.		*
 ************************************************************************/

void SaveState (ExecState *state)
{
    state -> pc   = pc;
    state -> cs   = cs;
    state -> sp   = sp;
    state -> argp = argp;
    state -> varp = varp;

    state -> line = curr_line_num;
    state -> file = curr_file_num;
}


/************************************************************************
 * Function:	RestoreState						*
 *									*
 * Description:	Restores previously saved state information.		*
 ************************************************************************/

void RestoreState (ExecState *state)
{
    pc	 = state -> pc;
    cs	 = state -> cs;
    sp	 = state -> sp;
    argp = state -> argp;
    varp = state -> varp;

    curr_line_num = state -> line;
    curr_file_num = state -> file;
    curr_file_name = *strlit (curr_file_num);
}


/************************************************************************
 * Function:	execute							*
 *									*
 * Description:	Executes a code segment of the virtual machine.		*
 ************************************************************************/

int execute (Code code, descriptor *vars, descriptor *args)
{
    ExecState state;
    int       status;


    SaveState (&state);

    pc   = 0;
    cs   = code;
    varp = vars;
    argp = args;

    while ((status = exectab [fetch (pc ++).op] ( )) == 0);

    RestoreState (&state);

    d_printf ("clearing stack\n");
    while (sp != state.sp)
	RecycleData (pop ( ));

    return status;
}


/************************************************************************
 * Function:	TypeError						*
 *									*
 * Description:	Reports a type error for an operator or function.	*
 ************************************************************************/

void TypeError (const char *op, const descriptor *a, const descriptor *b, const descriptor *c, int is_func)
{
    char a_type[128];
    char b_type[128];
    char c_type[128];
    char msg[] = "type error in expression";


    strcpy(a_type, a ? D_TypeName (a) : "");
    strcpy(b_type, b ? D_TypeName (b) : "");
    strcpy(c_type, c ? D_TypeName (c) : "");


    if (is_func == F_False) {
	if (a && b && c)
	    rterror ("%s: %s %s %s %s %s", msg, a_type, op, b_type, op, c_type);
	else if (a && c)
	    rterror ("%s: %s %s %s", msg, a_type, op, c_type);
	else if (a && b)
	    rterror ("%s: %s %s %s", msg, a_type, op, b_type);
	else if (a)
	    rterror ("%s: %s %s", msg, a_type, op);
	else if (b)
	    rterror ("%s: %s %s", msg, op, b_type);
	else
	    rterror ("%s: %s", msg, op);

    } else {
	if (a && b && c)
	    rterror ("%s: %s (%s, %s, %s)", msg, op, a_type, b_type, c_type);
	else if (a && b)
	    rterror ("%s: %s (%s, %s)", msg, op, a_type, b_type);
	else if (a)
	    rterror ("%s: %s (%s)", msg, op, a_type);
	else
	    rterror ("%s: %s ()", msg, op);
    }
}


/************************************************************************
 * Function:	MatrixError						*
 *									*
 * Description:	Reports a matrix error.					*
 ************************************************************************/

void MatrixError (const char *op, const Matrix a, const Matrix b, int s, int is_func)
{
    char *msg;
    char  a_size [32];
    char  b_size [32];


    if (a)
	sprintf (a_size, "(%u x %u)", Mrows (a), Mcols (a));
    else
	sprintf (a_size, "(1 x 1)");

    if (b)
	sprintf (b_size, "(%u x %u)", Mrows (b), Mcols (b));
    else
	sprintf (b_size, "(1 x 1)");


    strcpy(msg,is_func == F_True ? "() function" : " expression");

    switch (s) {
    case M_RDONLY:
	rterror ("read only matrix in expression (op = %s)", op);
	break;


    case M_COMPACT:
	rterror ("compact matrix unexpected in %s%s", op, msg);
	break;


    case M_FULL:
	rterror ("full matrix unexpected in %s%s", op, msg);
	break;


    case M_NOTSYMMETRIC:
	rterror ("symmetric matrix required in %s%s", op, msg);
	break;


    case M_SIZEMISMATCH:
	if (is_func == F_True)
	    rterror ("argument size mismatch in %s() function: %s", op, a_size);
	else
	    rterror ("size mismatch in expression: %s %s %s", a_size,op,b_size);
	break;


    case M_NOTCOLUMN:
	rterror ("column vector required in %s%s", op, msg);
	break;


    case M_NOTROW:
	rterror ("row vector required in %s%s", op, msg);
	break;


    case M_NOTSQUARE:
	rterror ("square matrix required in %s%s", op, msg);
	break;


    case M_NOOVERWRITE:
	rterror ("cannot overwrite matrix (op = %s)", op);
	break;


    case M_NOTEXIST:
	rterror ("matrix does not exist (op = %s)", op);
	break;


    case M_NOTFULL:
	rterror ("full matrix required in %s%s", op, msg);
	break;


    case M_NOTCOMPACT:
	rterror ("compact matrix required in %s%s", op, msg);
	break;


    case M_BADSUBSECTION:
	rterror ("illegal subsection (op = %s)", op);
	break;


    case M_NOTPOSITIVEDEFINITE:
	rterror ("positive definite matrix required in %s%s", op, msg);
	break;


    default:
	rterror ("unknown matrix error (op=%s)", op);
	break;
    }
}


/************************************************************************
 * Function:	MathException						*
 *									*
 * Description:	Reports a mathematic exception.				*
 ************************************************************************/

void MathException (const char *s)
{
    rterror ("exception in expression: %s", s);
}
