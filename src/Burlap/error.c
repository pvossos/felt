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
 * File:	error.c							*
 *									*
 * Description:	This file contains the function definitions for the	*
 *		error handling mechanism.				*
 ************************************************************************/

# include <stdio.h>
# include "error.h"
# include "lexer.h"
# include VAR_ARGS_INCLUDE


int   curr_line_num;			/* current line number */
int   curr_file_num;			/* current file number */
char *curr_file_name;			/* current file name   */


/************************************************************************
 * Function:	rterror							*
 *									*
 * Description:	Prints a run-time error message.			*
 ************************************************************************/

# ifdef UseFunctionPrototypes
void rterror (char *format, ...)
# else
void rterror (format, va_alist)
    char *format;
    va_dcl
# endif
{
    va_list ap;


    VA_START (ap, format);
    fprintf (stderr, "%s:%d: ", curr_file_name, curr_line_num);
    vfprintf (stderr, format, ap);
    fprintf (stderr, "\n");
    va_end (ap);
}


/************************************************************************
 * Function:	cterror							*
 *									*
 * Description:	Prints a compile-time error message.			*
 ************************************************************************/

# ifdef UseFunctionPrototypes
void cterror (char *format, ...)
# else
void cterror (format, va_alist)
    char *format;
    va_dcl
# endif
{
    va_list ap;


    VA_START (ap, format);
    fprintf (stderr, "%s:%d: ", file_name, line_num);
    vfprintf (stderr, format, ap);
    fprintf (stderr, "\n");
    va_end (ap);
}
