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
 * File:	Constraint.h						*
 *									*
 * Description:	This file contains the public function and type		*
 *		declarations for the constraint dialog box.		*
 ************************************************************************/

# ifndef _Constraint_h
# define _Constraint_h
# include "Tree.h"
# include "fe.h"

typedef struct constraint_dialog *ConstraintDialog;

typedef struct {
    ConstraintDialog dialog;
    Constraint	     constraint;
    Boolean	     deleted;
    Boolean	     proceed;
} ConstraintDialogInfo;


extern ConstraintDialog ConstraintDialogCreate (
# if NeedFunctionPrototypes
    Widget			/* parent      */,
    String			/* name        */,
    String			/* title       */,
    XtCallbackProc		/* callback    */,
    XtPointer			/* client_data */
# endif
);


extern void ConstraintDialogPopup (
# if NeedFunctionPrototypes
    ConstraintDialog		/* constraint_dialog */
# endif
);


extern void ConstraintDialogUpdate (
# if NeedFunctionPrototypes
    ConstraintDialog		/* constraint_dialog */,
    Tree			/* constraint_tree   */
# endif
);


extern Constraint ConstraintDialogActive (
# if NeedFunctionPrototypes
    ConstraintDialog		/* constraint_dialog */
# endif
);


extern void ConstraintDialogDisplay (
# if NeedFunctionPrototypes
    ConstraintDialog		/* constraint_dialog */,
    Constraint			/* constraint	     */
# endif
);

# endif /* _Constraint_h */
