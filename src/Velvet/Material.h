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
 * File:	Material.h						*
 *									*
 * Description:	This file contains the public function and type		*
 *		declarations for the material dialog box.		*
 ************************************************************************/

# ifndef _Material_h
# define _Material_h
# include "fe.h"
# include "Tree.h"

typedef struct material_dialog *MaterialDialog;

typedef struct {
    MaterialDialog dialog;
    Material	   material;
    Boolean	   deleted;
    Boolean	   proceed;
} MaterialDialogInfo;


extern MaterialDialog MaterialDialogCreate (
# if NeedFunctionPrototypes
    Widget			/* parent      */,
    String			/* name        */,
    String			/* title       */,
    XtCallbackProc		/* callback    */,
    XtPointer			/* client_data */
# endif
);


extern void MaterialDialogPopup (
# if NeedFunctionPrototypes
    MaterialDialog		/* material_dialog */
# endif
);


extern void MaterialDialogUpdate (
# if NeedFunctionPrototypes
    MaterialDialog		/* material_dialog */,
    Tree			/* material_tree   */
# endif
);


extern Material MaterialDialogActive (
# if NeedFunctionPrototypes
    MaterialDialog		/* material_dialog */
# endif
);


extern void MaterialDialogDisplay (
# if NeedFunctionPrototypes
    MaterialDialog		/* material_dialog */,
    Material			/* material	   */
# endif
);

# endif /* _Material_h */
