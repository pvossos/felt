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
 * File:	function.h						*
 *									*
 * Description:	This file contains the function and type declarations	*
 *		for function blocks.					*
 ************************************************************************/

# ifndef _FUNCTION_H
# define _FUNCTION_H
# include "codegen.h"			/* Code type definition	  */

# define ValueArg	0
# define SharedArg	1

typedef struct function {
    char  *name;			/* function name	     */
    Code   cs;				/* code segment		     */
    int    num_args;			/* number of arguments	     */
    int    num_locals;			/* number of local variables */
    int    ref_count;			/* reference count	     */
    char  *arg_types;			/* array of argument types   */
    char **local_names;			/* names of local variables  */
} *Function;


Function CreateFunction (char *name);

Function CopyFunction (Function func);

void DestroyFunction (Function func);

# endif /* _FUNCTION_H */
