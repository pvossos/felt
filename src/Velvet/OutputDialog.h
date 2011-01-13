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
 * File:	OutputDialog.h						*
 *									*
 * Description:	This file contains the public function and type		*
 *		declarations for the output dialog box.			*
 ************************************************************************/

# ifndef _OutputDialog_h
# define _OutputDialog_h

# include <stdarg.h>

typedef struct output_dialog *OutputDialog;

OutputDialog OutputDialogCreate (Widget parent, String name, String *buttons, Cardinal num_buttons);

String OutputDialogSelect (OutputDialog outputd, String title, String preferred);

void OutputDialogPopup (OutputDialog outputd, String title, String preferred, XtCallbackProc callback, XtPointer client_data);

void OutputDialogPopdown (OutputDialog outputd);

void OutputDialogView (OutputDialog outputd, String file_name, Cardinal max_lines, Cardinal max_columns);

void OutputDialogPrintf (OutputDialog outputd, String format, ...);

void OutputDialogVprintf (OutputDialog outputd, String format, va_list ap);

Widget OutputDialogShell (OutputDialog outputd);

# endif /* _OutputDialog_h */
