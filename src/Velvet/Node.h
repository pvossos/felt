/*
    This file is part of the FElt finite element analysis package.
    Copyright (C) 1993-1997 Jason I. Gobat and Darren C. Atkinson

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
 * File:	Node.h							*
 *									*
 * Description:	This file contains the public function and type		*
 *		declarations for the node dialog box.			*
 ************************************************************************/

# ifndef _Node_h
# define _Node_h
# include "Tree.h"
# include "fe.h"

typedef struct node_dialog *NodeDialog;

typedef struct {
    NodeDialog dialog;
    Node       node;
    Boolean    deleted;
    Boolean    proceed;
    Boolean    moved;
} NodeDialogInfo;


extern NodeDialog NodeDialogCreate (
# if NeedFunctionPrototypes
    Widget			/* parent      */,
    String			/* name        */,
    String			/* title       */,
    XtCallbackProc		/* callback    */,
    XtPointer			/* client_data */
# endif
);


extern void NodeDialogPopup (
# if NeedFunctionPrototypes
    NodeDialog			/* node_dialog */
# endif
);


extern void NodeDialogUpdate (
# if NeedFunctionPrototypes
    NodeDialog			/* node_dialog	   */,
    Tree			/* node_tree	   */,
    Tree			/* force_tree	   */,
    Tree			/* constraint_tree */
# endif
);


extern Node NodeDialogActive (
# if NeedFunctionPrototypes
    NodeDialog			/* node_dialog */
# endif
);


extern void NodeDialogDisplay (
# if NeedFunctionPrototypes
    NodeDialog			/* node_dialog */,
    Node			/* node        */
# endif
);

# endif /* _Node_h */
