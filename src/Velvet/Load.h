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
 * File:	Load.h							*
 *									*
 * Description:	This file contains the public function and type		*
 *		declarations for the load dialog box.			*
 ************************************************************************/

# ifndef _Load_h
# define _Load_h
# include "fe.h"
# include "Tree.h"

typedef struct load_dialog *LoadDialog;

typedef struct {
    LoadDialog	dialog;
    Distributed	load;
    Boolean	deleted;
    Boolean	proceed;
} LoadDialogInfo;


extern LoadDialog LoadDialogCreate (
# if NeedFunctionPrototypes
    Widget			/* parent      */,
    String			/* name        */,
    String			/* title       */,
    XtCallbackProc		/* callback    */,
    XtPointer			/* client_data */
# endif
);


extern void LoadDialogPopup (
# if NeedFunctionPrototypes
    LoadDialog			/* load_dialog */
# endif
);


extern void LoadDialogUpdate (
# if NeedFunctionPrototypes
    LoadDialog			/* load_dialog	*/,
    Tree			/* load_tree	*/
# endif
);


extern Distributed LoadDialogActive (
# if NeedFunctionPrototypes
    LoadDialog			/* load_dialog */
# endif
);


extern void LoadDialogDisplay (
# if NeedFunctionPrototypes
    LoadDialog			/* load_dialog */,
    Distributed			/* load	       */
# endif
);

# endif /* _Load_h */
