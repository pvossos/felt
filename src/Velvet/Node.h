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
 * File:	Node.h							*
 *									*
 * Description:	This file contains the public function and type		*
 *		declarations for the node dialog box.			*
 ************************************************************************/

# ifndef _Node_h
# define _Node_h

# include "problem.h"
# include "Tree.h"
# include "fe.h"

/*----------------------------------------------------------------------*/

typedef struct node_dialog *NodeDialog;

typedef struct {
    NodeDialog dialog;
    Node       node;
    Boolean    deleted;
    Boolean    proceed;
    Boolean    moved;
} NodeDialogInfo;


NodeDialog NodeDialogCreate (Widget parent, String name, String title, XtCallbackProc callback, XtPointer closure);

void NodeDialogPopup (NodeDialog noded);

void NodeDialogUpdate (NodeDialog noded, Problem::NodeSet *nodes, Problem::ForceSet *forces, Tree constraints);

Node NodeDialogActive (NodeDialog noded);

void NodeDialogDisplay (NodeDialog noded, Node node);

/*----------------------------------------------------------------------*/

# endif /* _Node_h */
