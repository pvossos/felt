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
 * File:	detail.c						*
 *									*
 * Description:	This file contains the function definitions for the	*
 *		"extraneous" detail output routines.			*
 ************************************************************************/

# include <stdio.h>
# include "problem.h"
# include VAR_ARGS_INCLUDE


static FILE	*detail_fp = NULL;


/************************************************************************
 * Function:	SetDetailStream						*
 *									*
 * Description:	Turns on (or off) and sets the stream that describes	*
 *		where detail messages should be printed.  To toggle  	*
 *		detail messages off, set the stream to NULL        	*
 ************************************************************************/

void
SetDetailStream(FILE *fp)
{
    detail_fp = fp;
}

FILE*
GetDetailStream(void)
{
   return detail_fp;
}

/************************************************************************
 * Function:	detail							*
 *									*
 * Description:	checks the state of the detail print flag and if it	*
 *		is on, prints a message to the current detail stream	*
 ************************************************************************/

void
detail(char *format, ...)
{
    va_list ap;

    if (!detail_fp)
       return;

    VA_START (ap, format);

    vfprintf (detail_fp, format, ap);
    fprintf (detail_fp, "\n");
    va_end (ap);
}
