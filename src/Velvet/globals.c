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

/*************************************************************************
*
* File:		globals.c
*
* Description:	Contains declarations for the uninitialized global
*		variables
*
**************************************************************************/

# include <X11/Intrinsic.h>
# include "fe.h"
# include "OutputDialog.h"

/* Global widgets */

XtAppContext app_context;

Widget drawing;
Widget entry;
Widget statusline;
Widget toplevel;
Widget viewport;
Widget abortbutton;
Widget quitbutton;

Pixmap checkmark;

/* Global dialogs */

OutputDialog proceed_dialog;
OutputDialog error_dialog;
OutputDialog qsave_dialog;
OutputDialog output_dialog;


/* Some stuff that we need to initialize */

Boolean  changeflag = False;

Boolean	 edit_mode  = False;

char filename [2048] = {""};

Node displayed_node = NULL;
Element displayed_element = NULL;

Boolean sensitive_menus = True;
