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
 * File:	apply.c							*
 *									*
 * Description:	This files contains the functions concerning the	*
 *		application of objects to other objects.		*
 ************************************************************************/

# include <stdio.h>
# include <X11/Intrinsic.h>
# include "Element.h"
# include "Node.h"
# include "Force.h"
# include "Load.h"
# include "Constraint.h"
# include "Material.h"
# include "problem.h"
# include "globals.h"
# include "Drawing.h"
# include "text_entry.h"
# include "error.h"
# include "vfe.h"
# include "procedures.h"

extern LoadDialog	load_d;
extern ConstraintDialog	constraint_d;
extern ForceDialog 	force_d;
extern MaterialDialog	material_d;
extern ElementDialog	element_d;
extern NodeDialog	node_d;

	/*
	 * applying forces
	 */

static void
ApplyForceGroup(Figure *figures, unsigned nfigures)
{
    unsigned		i;
    Figure		fig;
    FigureAttributes	attr;
    Node		node;
    Drawn		drawn;
    Node		displayed;


    displayed = NodeDialogActive (node_d);

    for (i = 0; i < nfigures; i ++) {
	fig = figures [i];
	DW_GetAttributes (drawing, fig, &attr);

	if (attr.user_data == NULL || attr.type == TextFigure)
	    continue;

	node = (Node) attr.user_data;
	drawn = (Drawn) node -> aux;
	if (drawn -> type != DrawnNode)
	    continue;

	node -> force = ForceDialogActive (force_d);
	changeflag = True;

	if (node == displayed)
	    NodeDialogDisplay (node_d, node);
    }

    XtFree ((char *) figures);
}


static void
DoApplyForce(Node node)
{
    static char message [40];


    node -> force = ForceDialogActive (force_d);

    NodeDialogDisplay (node_d, node);

    sprintf (message, "Applied to node %d.  Select node:", node -> number);
    ChangeStatusLine (message, True);
    changeflag = True;
}

void ApplyForceCB (Widget w, XtPointer client_data, XtPointer call_data)
{
    DrawingReport   *report;
    FigureAttributes attributes;
    Figure           figure;
    Node             node;
    Drawn            drawn;


    report = (DrawingReport *) call_data;

    if (report -> event -> type != ButtonPress)
	return;

    if (report -> event -> xbutton.button == 3)
         QuitEditCB (w, client_data, call_data);

    if (report -> event -> xbutton.button == 2)
	SelectGroup (call_data, ApplyForceGroup);

    if (report -> event -> xbutton.button != 1)
	return;

    figure = DW_FindFigure (w, report -> unsnapped.x, report -> unsnapped.y);

    if (figure == NULL)
	return;

    DW_GetAttributes (w, figure, &attributes);
    if (attributes.user_data == NULL)
	return;

    node = (Node) attributes.user_data;
    drawn = (Drawn) node -> aux;
    if (drawn -> type != DrawnNode)
	return;

    DoApplyForce (node);
}


void ApplyForceAP (Widget w, XEvent *event, String *params, Cardinal *num)
{
    char       *status;
    struct node dummy;
    Item        found;


    if ((status = GetTextNumber (&dummy.number)) != NULL) {
	if (!strcmp (status, "w"))
	    SelectGroup (NULL, ApplyForceGroup);
	return;
    }

    found = TreeSearch (problem.node_tree, (Item) &dummy);
    if (found == NULL) {
	error ("Node %d does not exist.", dummy.number);
	return;
    }

    DoApplyForce ((Node) found);
}


void EditApplyForce (void)
{
    if (ForceDialogActive (force_d) == NULL) {
	error ("No active force defined.");
	return;
    }

    SetEditMode ( );
    ChangeStatusLine ("Select node:", True);

    XtRemoveAllCallbacks (drawing, XtNbuttonCallback);
    XtAddCallback (drawing, XtNbuttonCallback, ApplyForceCB, NULL);

    XtOverrideTranslations (entry,
	XtParseTranslationTable ("<Key>Return: ApplyForceAP()"));
}

	/*
	 * applying constraints
	 */

static void
ApplyConstraintGroup(Figure *figures, unsigned nfigures)
{
    unsigned		i;
    Figure		fig;
    FigureAttributes	attr;
    Node		node;
    Drawn		drawn;
    Node		displayed;


    displayed = NodeDialogActive (node_d);

    for (i = 0; i < nfigures; i ++) {
	fig = figures [i];
	DW_GetAttributes (drawing, fig, &attr);

	if (attr.user_data == NULL || attr.type == TextFigure)
	    continue;

	node = (Node) attr.user_data;
	drawn = (Drawn) node -> aux;
	if (drawn -> type != DrawnNode)
	    continue;

	node -> constraint = ConstraintDialogActive (constraint_d);
	changeflag = True;

	if (node == displayed)
	    NodeDialogDisplay (node_d, node);
    }

    XtFree ((char *) figures);
}


static void
DoApplyConstraint(Node node)
{
    static char message [40];


    node -> constraint = ConstraintDialogActive (constraint_d);

    NodeDialogDisplay (node_d, node);

    sprintf (message, "Applied to node %d.  Select node:", node -> number);
    ChangeStatusLine (message, True);
    changeflag = True;
}


void ApplyConstraintCB (Widget w, XtPointer client_data, XtPointer call_data)
{
    DrawingReport   *report;
    FigureAttributes attributes;
    Figure           figure;
    Drawn	     drawn;
    Node             node;


    report = (DrawingReport *) call_data;

    if (report -> event -> type != ButtonPress)
	return;

    if (report -> event -> xbutton.button == 3)
         QuitEditCB (w, client_data, call_data );

    if (report -> event -> xbutton.button == 2)
	SelectGroup (call_data, ApplyConstraintGroup);

    if (report -> event -> xbutton.button != 1)
	return;

    figure = DW_FindFigure (w, report -> unsnapped.x, report -> unsnapped.y);

    if (figure == NULL)
	return;

    DW_GetAttributes (w, figure, &attributes);
    if (attributes.user_data == NULL)
	return;

    node = (Node) attributes.user_data;
    drawn = (Drawn) node -> aux;
    if (drawn -> type != DrawnNode)
	return;

    DoApplyConstraint (node);
}


void ApplyConstraintAP (Widget w, XEvent *event, String *params, Cardinal *num)
{
    char       *status;
    struct node dummy;
    Item        found;


    if ((status = GetTextNumber (&dummy.number)) != NULL) {
	if (!strcmp (status, "w"))
	    SelectGroup (NULL, ApplyConstraintGroup);
	return;
    }

    found = TreeSearch (problem.node_tree, (Item) &dummy);
    if (found == NULL) {
	error ("Node %d does not exist.", dummy.number);
	return;
    }

    DoApplyConstraint ((Node) found);
}


void EditApplyConstraint (void)
{

    if (ConstraintDialogActive (constraint_d) == NULL) {
	error ("No active constraint defined.");
	return;
    }

    SetEditMode ( );
    ChangeStatusLine ("Select node:", True);

    XtRemoveAllCallbacks (drawing, XtNbuttonCallback);
    XtAddCallback (drawing, XtNbuttonCallback, ApplyConstraintCB, NULL);

    XtOverrideTranslations (entry,
	XtParseTranslationTable ("<Key>Return: ApplyConstraintAP()"));
}

	/*
	 * applying loads
	 */

static void
ApplyLoadGroup(Figure *figures, unsigned nfigures)
{
    unsigned		i;
    Figure		fig;
    FigureAttributes	attr;
    Element		element;
    Drawn		drawn;
    Element		displayed;


    displayed = ElementDialogActive (element_d);

    for (i = 0; i < nfigures; i ++) {
	fig = figures [i];
	DW_GetAttributes (drawing, fig, &attr);

	if (attr.user_data == NULL || attr.type == TextFigure)
	    continue;

	element = (Element) attr.user_data;
	drawn = (Drawn) element -> aux;
	if (drawn -> type != DrawnElement)
	    continue;

	if (element -> numdistributed == 3)
	    return;

	element->distributed [++ element->numdistributed] =
						LoadDialogActive (load_d);

	changeflag = True;

	if (element == displayed)
	    ElementDialogDisplay (element_d, element);
    }

    XtFree ((char *) figures);
}




static void
DoApplyLoad(Element element)
{
    static char message [80];


    if (element -> numdistributed == 3) {
	error ("Maximum number of loads applied.");
	return;
    }

    element->distributed[++ element->numdistributed] = LoadDialogActive(load_d);

    ElementDialogDisplay (element_d, element);

    sprintf (message, "Applied to element %d.  Select element:", element -> number);
    ChangeStatusLine (message, True);
    changeflag = True;
}


void ApplyLoadCB (Widget w, XtPointer client_data, XtPointer call_data)
{
    DrawingReport   *report;
    FigureAttributes attributes;
    Figure           figure;
    Drawn            drawn;
    Element          element;


    report = (DrawingReport *) call_data;

    if (report -> event -> type != ButtonPress)
	return;

    if (report -> event -> xbutton.button == 3)
         QuitEditCB (w, client_data, call_data);

    if (report -> event -> xbutton.button == 2)
	SelectGroup (call_data, ApplyLoadGroup);

    if (report -> event -> xbutton.button != 1)
	return;

    figure = DW_FindFigure (w, report -> unsnapped.x, report -> unsnapped.y);

    if (figure == NULL)
	return;

    DW_GetAttributes (w, figure, &attributes);
    if (attributes.user_data == NULL)
	return;

    element = (Element) attributes.user_data;
    drawn = (Drawn) element -> aux;
    if (drawn -> type != DrawnElement)
	return;

    DoApplyLoad (element);
}


void ApplyLoadAP (Widget w, XEvent *event, String *params, Cardinal *num)
{
    char          *status;
    struct element dummy;
    Item           found;


    if ((status = GetTextNumber (&dummy.number)) != NULL) {
	if (!strcmp (status, "w"))
	    SelectGroup (NULL, ApplyLoadGroup);
	return;
    }

    found = TreeSearch (problem.element_tree, (Item) &dummy);
    if (found == NULL) {
	error ("Element %d does not exist.", dummy.number);
	return;
    }

    DoApplyLoad ((Element) found);
}


void EditApplyLoad (void)
{
    if (LoadDialogActive (load_d) == NULL) {
	error ("No active load defined.");
	return;
    }

    SetEditMode ( );
    ChangeStatusLine ("Select element:", True);

    XtRemoveAllCallbacks (drawing, XtNbuttonCallback);
    XtAddCallback (drawing, XtNbuttonCallback, ApplyLoadCB, NULL);

    XtOverrideTranslations (entry,
	XtParseTranslationTable ("<Key>Return: ApplyLoadAP()"));
}

	/*
	 * applying materials
	 */

static void
ApplyMaterialGroup(Figure *figures, unsigned nfigures)
{
    unsigned		i;
    Figure		fig;
    FigureAttributes	attr;
    Element		element;
    Drawn		drawn;
    Element		displayed;


    displayed = ElementDialogActive (element_d);

    for (i = 0; i < nfigures; i ++) {
	fig = figures [i];
	DW_GetAttributes (drawing, fig, &attr);

	if (attr.user_data == NULL || attr.type == TextFigure)
	    continue;

	element = (Element) attr.user_data;
	drawn = (Drawn) element -> aux;
	if (drawn -> type != DrawnElement)
	    continue;

	element -> material = MaterialDialogActive (material_d);
	changeflag = True;

	if (element == displayed)
	    ElementDialogDisplay (element_d, element);
    }

    XtFree ((char *) figures);
}


static void
DoApplyMaterial(Element element)
{
    static char message [80];


    element -> material = MaterialDialogActive (material_d);

    ElementDialogDisplay (element_d, element);

    sprintf (message, "Applied to element %d.  Select element:", element -> number);
    ChangeStatusLine (message, True);
    changeflag = True;
}


void ApplyMaterialCB (Widget w, XtPointer client_data, XtPointer call_data)
{
    DrawingReport   *report;
    FigureAttributes attributes;
    Figure           figure;
    Drawn            drawn;
    Element          element;


    report = (DrawingReport *) call_data;

    if (report -> event -> type != ButtonPress)
	return;

    if (report -> event -> xbutton.button == 3)
         QuitEditCB (w, client_data, call_data);

    if (report -> event -> xbutton.button == 2)
	SelectGroup (call_data, ApplyMaterialGroup);

    if (report -> event -> xbutton.button != 1)
	return;

    figure = DW_FindFigure (w, report -> unsnapped.x, report -> unsnapped.y);

    if (figure == NULL)
	return;

    DW_GetAttributes (w, figure, &attributes);
    if (attributes.user_data == NULL)
	return;

    element = (Element) attributes.user_data;
    drawn = (Drawn) element -> aux;
    if (drawn -> type != DrawnElement)
	return;

    DoApplyMaterial (element);
}


void ApplyMaterialAP (Widget w, XEvent *event, String *params, Cardinal *num)
{
    char          *status;
    struct element dummy;
    Item           found;


    if ((status = GetTextNumber (&dummy.number)) != NULL) {
	if (!strcmp (status, "w"))
	    SelectGroup (NULL, ApplyMaterialGroup);
	return;
    }

    found = TreeSearch (problem.element_tree, (Item) &dummy);
    if (found == NULL) {
	error ("Element %d does not exist.", dummy.number);
	return;
    }

    DoApplyMaterial ((Element) found);
}


void EditApplyMaterial (void)
{
    if (MaterialDialogActive (material_d) == NULL) {
	error ("No active material defined.");
	return;
    }

    SetEditMode ( );
    ChangeStatusLine ("Select element:", True);

    XtRemoveAllCallbacks (drawing, XtNbuttonCallback);
    XtAddCallback (drawing, XtNbuttonCallback, ApplyMaterialCB, NULL);

    XtOverrideTranslations (entry,
	XtParseTranslationTable ("<Key>Return: ApplyMaterialAP()"));
}
