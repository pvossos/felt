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
 *		error handling routines.				*
 ************************************************************************/

# include <stdio.h>
# include <stdlib.h>
# include <stdarg.h>
# include "error.h"
# include "problem.h"

/************************************************************************
 * Function:	error							*
 *									*
 * Description:	Prints an error message specified as a format string	*
 *		and arguments to standard error.  If the current line	*
 *		exists (is not zero) then current file and line are	*
 *		printed before the error message.			*
 ************************************************************************/

void error (char *format, ...)
{
    va_list ap;


    va_start (ap, format);

    fprintf (stderr, "yardstick: ");

    vfprintf (stderr, format, ap);
    fprintf (stderr, "\n");
    va_end (ap);

    problem.num_errors ++;
}


/************************************************************************
 * Function:	Fatal							*
 *									*
 * Description:	Prints an error message specified as a format string	*
 		and arguments to standard error and exits the program.	*
 ************************************************************************/

void Fatal (char *format, ...)
{
    va_list ap;


    va_start (ap, format);
    fprintf (stderr, "yardstick: ");
    vfprintf (stderr, format, ap);
    fprintf (stderr, "\n");
    va_end (ap);
    exit (1);
}
