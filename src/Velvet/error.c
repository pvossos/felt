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
 * Description:	This file contains the error handling routines.		*
 ************************************************************************/

# include <stdio.h>
# include <unistd.h>
# include <stdlib.h>
# include <X11/Intrinsic.h>
# include "OutputDialog.h"
# include "fe.h"
# include "util.h"
# include "error.h"
# include "Tree.h"
# include "problem.h"
# include "globals.h"
# include "procedures.h"


static Boolean buffer_errors = False;
static Boolean errors = False;
static FILE *output;

#ifdef _CONVEX_SOURCE
# define TEMPNAME(a,b) tmpnam((b))
#else
# define TEMPNAME(a,b) tempnam((a), (b))
#endif

/************************************************************************
 * Function:	 error							*
 *									*
 * Description:	 Error prints the error message specified as a format	*
 *		 string and arguments to the error dialog.		*
 ************************************************************************/

# if NeedVarargsPrototypes
void error (char *format, ...)
# else
void error (format, va_alist)
    char *format;
    va_dcl
# endif
{
    va_list ap;


    if (buffer_errors == True) {
	va_start (ap, format);
	if (problem.line)
	    fprintf (output, "%s:%d: ", problem.filename, problem.line);
	vfprintf (output, format, ap);
	fprintf (output, "\n");
	va_end (ap);
	errors = True;
    } else {
	va_start (ap, format);
	OutputDialogVprintf (error_dialog, format, ap);
	CenterOnWidget (OutputDialogShell (error_dialog), toplevel, True);
	WarpToCenter (OutputDialogShell (error_dialog));
	OutputDialogSelect (error_dialog, "Error", "okay");
	va_end (ap);
    }

    problem.num_errors ++;
}


/************************************************************************
 * Function:	 Fatal							*
 *									*
 * Description:	 Fatal prints the error message specified as a format	*
 *		 string and arguments to standard error and exits the	*
 *		 program.						*
 ************************************************************************/

void Fatal (char *format, ...)
{
    va_list ap;

    va_start (ap, format);
    fprintf (stderr, "velvet: ");
    vfprintf (stderr, format, ap);
    fprintf (stderr, "\n");
    va_end (ap);
    exit (1);
}


void BufferErrors (Boolean flag)
{
    static char *name;


    if (flag == True && buffer_errors == False) {
	name = TEMPNAME(NULL, "velv");
	if ((output = fopen (name, "w")) == NULL) {
	    error ("Could not create temporary file for output.");
	    return;
	}
	buffer_errors = flag;
	errors = False;

    } else if (flag == False && buffer_errors == True) {
	fclose (output);
	if (errors == True) {
	    OutputDialogView (output_dialog, name, 10, 80);
	    CenterOnScreen (OutputDialogShell (output_dialog), False);
	    WarpToCenter (OutputDialogShell (output_dialog));
            OutputDialogPopup (output_dialog, "FElt Output", "dismiss",
                               OutputButtonActions, NULL);
	}

	(void) unlink (name);
	free ((char *) name);
	buffer_errors = flag;
    }
}
