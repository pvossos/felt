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
 * File:	callbacks.c						*
 *									*
 * Description:	This file contains the callbacks for the various object	*
 *		dialogs.						*
 ************************************************************************/

# include <stdio.h>
# include <X11/Intrinsic.h>
# include "Constraint.h"
# include "Force.h"
# include "Element.h"
# include "Load.h"
# include "Material.h"
# include "Node.h"
# include "problem.h"
# include "globals.h"
# include "Drawing.h"
# include "Colors.h"
# include "vfe.h"
# include "util.h"
# include "error.h"
# include "procedures.h"
# include "callbacks.h"

extern ColorsDialog	colors_d;

static Boolean	   first_time;
static Constraint  deleted_constraint;
static Distributed deleted_load;
static Force	   deleted_force;
static Material	   deleted_material;


/************************************************************************
 * Function:	CheckOk							*
 *									*
 * Description:	Checks if the user want to proceed with a deletion.	*
 ************************************************************************/

static int CheckOk (String object, const char *name)
{
    String selected;
    char   title [64];


    OutputDialogPrintf (proceed_dialog, "%s %s is still in use.  Proceed?",
			object, name);

    sprintf (title, "Delete %s", object);
    CenterOnWidget (OutputDialogShell (proceed_dialog), toplevel, True);
    WarpToCenter (OutputDialogShell (proceed_dialog));
    selected = OutputDialogSelect (proceed_dialog, title, "okay");

    return selected && !strcmp (selected, "okay");
}


/************************************************************************
 * Function:	CheckConstraintReferences				*
 *									*
 * Description:	Tree iterator to check references to a constraint.	*
 ************************************************************************/

static int CheckConstraintReferences (Item item)
{
    Node node;


    node = (Node) item;

    if (node -> constraint == deleted_constraint) {
        error ("Constraint %s is still in use.", deleted_constraint -> name.c_str());
	return 1;
    }

    return 0;
}


/************************************************************************
 * Function:	ConstraintDialogChanged					*
 *									*
 * Description:	Callback for when a change occurs in the constraint	*
 *		dialog.							*
 ************************************************************************/

void ConstraintDialogChanged (Widget w, XtPointer client_data, XtPointer call_data)
{
    Node		  node;
    NodeDialog		  node_d;
    ConstraintDialogInfo *info;


    info = (ConstraintDialogInfo *) call_data;
    node_d = (NodeDialog) client_data;

    if (info -> deleted == True) {
	deleted_constraint = info -> constraint;
	TreeSetIterator (problem.node_tree, CheckConstraintReferences);
	info -> proceed = TreeIterate (problem.node_tree) ? False : True;
    }

    if (info -> proceed == True) {
	changeflag = True;
	NodeDialogUpdate (node_d, NULL, NULL, problem.constraint_tree);
	node = NodeDialogActive (node_d);
	if (node != NULL && info -> constraint == node -> constraint)
	    NodeDialogDisplay (node_d, node);

        ColorsDialogUpdateObjectList (colors_d, problem.constraint_tree, info -> deleted);
    }
}


/************************************************************************
 * Function:	ElementDialogChanged					*
 *									*
 * Description:	Callback for when a change occurs in the element	*
 *		dialog.							*
 ************************************************************************/

void ElementDialogChanged (Widget w, XtPointer client_data, XtPointer call_data)
{
    unsigned	       i;
    unsigned	       numnodes;
    Drawn	       drawn;
    ElementDialogInfo *info;


    info = (ElementDialogInfo *) call_data;
    numnodes = info -> element -> definition -> numnodes;

    if (info -> deleted == True) {
	drawn = (Drawn) info -> element -> aux;
	if (drawn -> figure != NULL) {
	    DW_SetAutoRedraw (drawing, False);
	    DW_RemoveFigure  (drawing, drawn -> figure);
	    DW_RemoveFigure  (drawing, drawn -> label);
	    DW_SetAutoRedraw (drawing, True);
	}

	for (i = 1; i <= numnodes; i ++)
	    if (info -> element -> node [i] != NULL)
		((Drawn) (info -> element -> node [i] -> aux)) -> ref_count --;

    } else
	for (i = 1; i <= numnodes; i ++)
	    if (info -> original -> node [i] != info -> element -> node [i]) {
            MoveElement (info -> element, info -> original -> node.c_ptr1());
            break;
	    }

    changeflag = True;
}


/************************************************************************
 * Function:	CheckForceReferences					*
 *									*
 * Description:	Tree iterator to check references to a force.		*
 ************************************************************************/

static int CheckForceReferences (Item item)
{
    Node node;


    node = (Node) item;

    if (node -> force == deleted_force) {
	if (first_time) {
	    if (!CheckOk ("Force", deleted_force -> name.c_str()))
		return 1;
	    first_time = False;
	}
	node -> force = NULL;
    }

    return 0;
}


/************************************************************************
 * Function:	ForceDialogChanged					*
 *									*
 * Description:	Callback for when a change occurs in the force dialog.	*
 ************************************************************************/

void ForceDialogChanged (Widget w, XtPointer client_data, XtPointer call_data)
{
    Boolean	     displayed;
    Node	     node;
    ForceDialogInfo *info;
    NodeDialog	     node_d;


    info = (ForceDialogInfo *) call_data;
    node_d = (NodeDialog) client_data;

    node = NodeDialogActive (node_d);
    displayed = node != NULL && node -> force == info -> force;

    if (info -> deleted == True) {
	first_time = True;
	deleted_force = info -> force;

	TreeSetIterator (problem.node_tree, CheckForceReferences);
	if (TreeIterate (problem.node_tree)) {
	    info -> proceed = False;
	    return;
	}
    }

    changeflag = True;
    NodeDialogUpdate (node_d, NULL, problem.force_tree, NULL);

    ColorsDialogUpdateObjectList (colors_d, problem.force_tree, info -> deleted);

    if (displayed == True)
	NodeDialogDisplay (node_d, node);
}


/************************************************************************
 * Function:	CheckLoadReferences					*
 *									*
 * Description:	Tree iterator to check references to a load.		*
 ************************************************************************/

static int CheckLoadReferences (Item item)
{
    Cardinal i;
    Cardinal j;
    Element  element;


    element = (Element) item;

    for (i = 1; i <= element -> numdistributed; i ++)
	if (element -> distributed [i] == deleted_load) {
	    if (first_time) {
            if (!CheckOk ("Load", deleted_load -> name.c_str()))
		    return 1;
		first_time = False;
	    }
	    for (j = i + 1; j <= element -> numdistributed; j ++)
		element -> distributed [j - 1] = element -> distributed [j];
	    element -> numdistributed --;
	}

    return 0;
}


/************************************************************************
 * Function:	LoadDialogChanged					*
 *									*
 * Description:	Callback for when a change occurs in the load dialog.	*
 ************************************************************************/

void LoadDialogChanged (Widget w, XtPointer client_data, XtPointer call_data)
{
    Cardinal	    i;
    Boolean	    displayed;
    Element	    element;
    LoadDialogInfo *info;
    ElementDialog   element_d;


    info = (LoadDialogInfo *) call_data;
    element_d = (ElementDialog) client_data;

    displayed = False;
    if ((element = ElementDialogActive (element_d)) != NULL)
	for (i = 1; i <= element -> numdistributed; i ++)
	    if (element -> distributed [i] == info -> load)
		displayed = True;


    if (info -> deleted == True) {
	first_time = True;
	deleted_load = info -> load;

	TreeSetIterator (problem.element_tree, CheckLoadReferences);
	if (TreeIterate (problem.element_tree)) {
	    info -> proceed = False;
	    return;
	}

    }

    changeflag = True;
    ElementDialogUpdate (element_d, NULL, NULL, problem.distributed_tree, NULL);

    ColorsDialogUpdateObjectList (colors_d, problem.distributed_tree, info -> deleted);

    if (displayed == True)
	ElementDialogDisplay (element_d, element);
}


/************************************************************************
 * Function:	CheckMaterialReferences					*
 *									*
 * Description:	Tree iterator to check references to a material.	*
 ************************************************************************/

static int CheckMaterialReferences (Item item)
{
    Element element;


    element = (Element) item;

    if (element -> material == deleted_material) {
        error ("Material %s is still in use.", deleted_material -> name.c_str());
	return 1;
    }

    return 0;
}


/************************************************************************
 * Function:	MaterialDialogChanged					*
 *									*
 * Description:	Callback for when a change occurs in the material	*
 *		dialog.							*
 ************************************************************************/

void MaterialDialogChanged (Widget w, XtPointer client_data, XtPointer call_data)
{
    Element		element;
    MaterialDialogInfo *info;
    ElementDialog	element_d;


    info = (MaterialDialogInfo *) call_data;
    element_d = (ElementDialog) client_data;

    if (info -> deleted == True) {
	deleted_material = info -> material;
	TreeSetIterator (problem.element_tree, CheckMaterialReferences);
	info -> proceed = TreeIterate (problem.element_tree) ? False : True;
    }

    if (info -> proceed == True) {
	changeflag = True;
	ElementDialogUpdate(element_d, NULL, &problem.material_set, NULL, NULL);
	element = ElementDialogActive (element_d);
	if (element != NULL && info -> material == element -> material)
	    ElementDialogDisplay (element_d, element);

        ColorsDialogUpdateMaterialList (colors_d, &problem.material_set, info -> deleted);
    }
}


/************************************************************************
 * Function:	NodeDialogChanged					*
 *									*
 * Description:	Callback for when a change occurs in the node dialog.	*
 ************************************************************************/

void NodeDialogChanged (Widget w, XtPointer client_data, XtPointer call_data)
{
    Drawn	    drawn;
    NodeDialogInfo *info;
    ElementDialog   element_d;


    info = (NodeDialogInfo *) call_data;
    element_d = (ElementDialog) client_data;

    if (info -> deleted == True) {
	drawn = (Drawn) info -> node -> aux;
	if (drawn -> ref_count) {
	    error ("Node %u is still referenced by %u element%s.",
		   info -> node -> number, drawn -> ref_count,
		   drawn -> ref_count > 1 ? "s" : "");
	    info -> proceed = False;
	    return;
	}

	if (drawn -> figure != NULL) {
	    DW_SetAutoRedraw (drawing, False);
	    DW_RemoveFigure (drawing, drawn -> figure);
	    if (drawn -> label != NULL)
		DW_RemoveFigure (drawing, drawn -> label);
	    DW_SetAutoRedraw (drawing, True);
	}

    } else if (info -> moved == True)
	DoWalkNode (info -> node);

    ElementDialogUpdate (element_d, NULL, NULL, NULL, problem.node_tree);
    changeflag = True;
}
