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
 * File:	globals.h						*
 *									*
 * Description:	This file contains the function and variable		*
 *		declarations for the global symbols.  Global symbols	*
 *		include string and numeric literals and variables that	*
 *		are within the global scope.				*
 ************************************************************************/

# ifndef _GLOBALS_H
# define _GLOBALS_H
# include "symbol.h"		/* symbol table and symbol table entry types */
# include "descriptor.h"	/* descriptor type			     */


/* Symbol tables for global variables (local variables are on the stack),
   string literals, numeric literals, and record fields. */

extern st var_st;			/* global variable symbol table	  */
extern st str_st;			/* string literal symbol table	  */
extern st dbl_st;			/* double literal symbol table	  */
extern st field_st;			/* record field symbol table	  */
extern st import_st;			/* imported variable symbol table */

extern unsigned var_array_size;		/* size of global variable array  */
extern unsigned str_array_size;		/* size of string literal array	  */
extern unsigned dbl_array_size;		/* size of double literal array	  */

extern descriptor *var_array;		/* array of global variables	  */
extern char	 **str_array;		/* array of string literals	  */
extern double	  *dbl_array;		/* array of double literals	  */


extern ste *add_literal PROTO ((st *, char *, Opcode));
extern void global_init PROTO ((void));
extern int  is_global   PROTO ((descriptor *));

# endif /* _GLOBALS_H */
