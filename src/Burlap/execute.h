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
 * File:	execute.h						*
 *									*
 * Description:	This file contains the public function, type, and	*
 *		variable definitions for execution of the virtual	*
 *		machine code.						*
 ************************************************************************/

# ifndef _EXECUTE_H
# define _EXECUTE_H
# include "globals.h"		/* global variable array declaration */
# include "descriptor.h"	/* Code and descriptor types	     */

# define top()		sp
# define pop()		sp --
# define push()		++ sp
# define ntop(n)	(sp - (n))
# define argument(x)	(argp + (x))
# define local(x)	(varp + (x))
# define global(x)	(var_array + (x))
# define dbllit(x)	(dbl_array + (x))
# define strlit(x)	(str_array + (x))
# define deref(d)	(D_Type (d) == T_Variable ? D_Variable (d) : (d))
# define assignable(d)  (D_Type (d) == T_Variable || D_Trapped (d) != F_False)

typedef struct {
    Code	cs;
    Address     pc;
    descriptor *sp;
    descriptor *argp;
    descriptor *varp;
    int		line;
    int		file;
} ExecState;


extern Address	   pc;			/* program counter	  */
extern descriptor *sp;			/* stack pointer	  */
extern descriptor *argp;		/* argument pointer	  */
extern descriptor *varp;		/* local variable pointer */
extern descriptor *stack;		/* run-time stack	  */

extern int  execute	  PROTO ((Code, descriptor *, descriptor *));
extern void SaveState	  PROTO ((ExecState *));
extern void RestoreState  PROTO ((ExecState *));
extern void TypeError	  PROTO ((char *, descriptor *, descriptor *, descriptor *, int));
extern void MatrixError	  PROTO ((char *, Matrix, Matrix, int, int));
extern void MathException PROTO ((char *));

# endif /* _EXECUTE_H */
