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
 * File:	Force.h							*
 *									*
 * Description:	This file contains the public function and type		*
 *		declarations for the force dialog box.			*
 ************************************************************************/

# ifndef _Force_h
# define _Force_h
# include "fe.h"
# include "Tree.h"

typedef struct force_dialog *ForceDialog;

typedef struct {
    ForceDialog	dialog;
    Force	force;
    Boolean	deleted;
    Boolean	proceed;
} ForceDialogInfo;


extern ForceDialog ForceDialogCreate (
# if NeedFunctionPrototypes
    Widget			/* parent      */,
    String			/* name        */,
    String			/* title       */,
    XtCallbackProc		/* callback    */,
    XtPointer			/* client_data */
# endif
);


extern void ForceDialogPopup (
# if NeedFunctionPrototypes
    ForceDialog			/* force_dialog */
# endif
);


extern void ForceDialogUpdate (
# if NeedFunctionPrototypes
    ForceDialog			/* force_dialog */,
    Tree			/* force_tree   */
# endif
);


extern Force ForceDialogActive (
# if NeedFunctionPrototypes
    ForceDialog			/* force_dialog */
# endif
);


extern void ForceDialogDisplay (
# if NeedFunctionPrototypes
    ForceDialog			/* force_dialog */,
    Force			/* force	*/
# endif
);

# endif /* _Force_h */
