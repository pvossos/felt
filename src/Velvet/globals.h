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
* File:		globals.h
*
* Description:	This file contains various global information for
*		the velvet routines.
*
*************************************************************************/

# ifndef _GLOBALS_H
# define _GLOBALS_H
# include "OutputDialog.h"

/* Global widgets */

extern XtAppContext app_context;

extern Widget drawing;
extern Widget entry;
extern Widget statusline;
extern Widget toplevel;
extern Widget viewport;
extern Widget quitbutton;
extern Widget abortbutton;

extern Pixmap checkmark;

/* Global dialogs */

extern OutputDialog proceed_dialog;
extern OutputDialog error_dialog;
extern OutputDialog qsave_dialog;
extern OutputDialog output_dialog;


/* some global values */

extern Boolean   changeflag;
extern Boolean	 edit_mode;
extern Boolean   sensitive_menus;

extern char filename [ ];

extern Node displayed_node;
extern Element displayed_element;

extern Tree      figure_tree;

# endif
