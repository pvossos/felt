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

# include <stdio.h>
# include <string.h>
# include <stdlib.h>
# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <X11/Xaw/Simple.h>
# include "fe.h"
# include "Constraint.h"
# include "Canvas.h"
# include "Drawing.h"
# include "Element.h"
# include "Material.h"
# include "ElementList.h"
# include "procedures.h"
# include "problem.h"
# include "globals.h"
# include "vfe.h"
# include "text_entry.h"
# include "objects.h"
# include "error.h"


extern ElementDialog element_d;
extern MaterialDialog material_d;
extern ElementList element_l;

void DeleteElementGroup (figures, nfigures)
    Figure  *figures;
    unsigned nfigures;
{
    unsigned         i;
    unsigned         j;
    Figure           fig;
    Drawn            drawn;
    Element          element;
    Boolean          firsttime;
    Boolean	     newinfo;
    FigureAttributes attr;


    firsttime = True;
    newinfo = False;

    for (i = 0; i < nfigures; i ++) {
	fig = figures [i];
	DW_GetAttributes (drawing, fig, &attr);

	if (attr.user_data == NULL || attr.type == TextFigure)
	    continue;

	element = (Element) attr.user_data;
	drawn = (Drawn) element -> aux;
	if (drawn -> type != DrawnElement)
	    continue;

	if (firsttime == True) {
	    firsttime = False;
	    DW_SetAutoRedraw (drawing, False);
	}

	for (j = 1; j <= element -> definition -> numnodes; j ++)
	    if (element -> node [j] != NULL)
		((Drawn) element -> node [j] -> aux) -> ref_count --;

	newinfo = True;
	DW_RemoveFigure (drawing, drawn -> figure);
	DW_RemoveFigure (drawing, drawn -> label);
        
	(void) TreeDelete (problem.element_tree, element);
	DestroyElement (element);
    }


    if (newinfo == True) {
	ElementDialogDisplay (element_d, NULL);
	ElementDialogUpdate (element_d, problem.element_tree, NULL, NULL, NULL);
    }

    if (firsttime == False) {
	DW_SetAutoRedraw (drawing, True);
	changeflag = True;
    }

    XtFree ((char *) figures);
}


void DoDeleteElt (element)
    Element element;
{
    unsigned i;
    Element newelement;
    static char message [80];
    Drawn drawn = (Drawn) element -> aux;


    if (drawn -> figure != NULL) {
	DW_SetAutoRedraw (drawing, False);
	DW_RemoveFigure  (drawing, drawn -> figure);
	DW_RemoveFigure  (drawing, drawn -> label);
	DW_SetAutoRedraw (drawing, True);
    }

    for (i = 1; i <= element -> definition -> numnodes; i ++)
	if (element -> node [i] != NULL)
	    ((Drawn) (element -> node [i] -> aux)) -> ref_count --;

    sprintf (message, "Element %d deleted.  Select element:", element -> number);

    if (element == ElementDialogActive (element_d)) {
	newelement = (Element) TreePredecessor (problem.element_tree, element);
	if (newelement == NULL)
	    newelement = (Element) TreeSuccessor (problem.element_tree, element);
	ElementDialogDisplay (element_d, newelement);
    }

    (void) TreeDelete (problem.element_tree, element);
    ElementDialogUpdate (element_d, problem.element_tree, NULL, NULL, NULL);

    DestroyElement (element);
    ChangeStatusLine (message, True);
    changeflag = True;
}


void DeleteEltCB (w, client_data, call_data)
    Widget    w;
    XtPointer client_data;
    XtPointer call_data;
{
    DrawingReport   *report;
    FigureAttributes attributes;
    Figure           figure;
    Element          element;
    Drawn            drawn;


    report = (DrawingReport *) call_data;

    if (report -> event -> type != ButtonPress)
	return;

    if (report -> event -> xbutton.button == 3)
	QuitEdit ( );

    if (report -> event -> xbutton.button == 2) {
	SelectGroup (call_data, DeleteElementGroup);
	return;
    }

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

    DoDeleteElt (element);
}


void DeleteEltAP ( )
{
    char          *status;
    struct element dummy;
    Item           found;


    if ((status = GetTextNumber (&dummy.number)) != NULL) {
	if (!strcmp (status, "w"))
	    SelectGroup (NULL, DeleteElementGroup);
	return;
    }

    found = TreeSearch (problem.element_tree, (Item) &dummy);
    if (found == NULL) {
	error ("Element %d does not exist.", dummy.number);
	return;
    }

    DoDeleteElt (found);
}


void EditDeleteElement ( )
{
    Arg		arglist [1];

    SetEditMode ( );
    ChangeStatusLine ("Select element:", True);

    XtSetArg (arglist [0], XtNcursorName, "dotbox");
    XtSetValues (drawing, arglist, 1);

    XtRemoveAllCallbacks (drawing, XtNbuttonCallback);
    XtAddCallback (drawing, XtNbuttonCallback, DeleteEltCB, NULL);

    XtOverrideTranslations (entry,
	XtParseTranslationTable ("<Key>Return: DeleteEltAP()"));
}


void EditElementCB (w, client_data, call_data)
    Widget    w;
    XtPointer client_data;
    XtPointer call_data;
{
    DrawingReport   *report;
    FigureAttributes attributes;
    Figure           figure;
    Element          element;
    Drawn            drawn;


    report = (DrawingReport *) call_data;

    if (report -> event -> type != ButtonPress)
	return;

    if (report -> event -> xbutton.button == 3)
	QuitEdit ( );

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

    ElementDialogDisplay (element_d, element);
    ElementDialogPopup (element_d);
}


void EditElementAP ( )
{
    char          *status;
    struct element dummy;
    Item           found;


    if ((status = GetTextNumber (&dummy.number)) != NULL)
	return;

    found = TreeSearch (problem.element_tree, (Item) &dummy);
    if (found == NULL) {
	error ("Element %d does not exist.", dummy.number);
	return;
    }

    ElementDialogDisplay (element_d, found);
    ElementDialogPopup (element_d);
}


void EditElementNumber ( )
{
    SetEditMode ( );
    ChangeStatusLine ("Select element:", True);

    XtRemoveAllCallbacks (drawing, XtNbuttonCallback);
    XtAddCallback (drawing, XtNbuttonCallback, EditElementCB, NULL);

    XtOverrideTranslations (entry,
	XtParseTranslationTable ("<Key>Return: EditElementAP()"));
}


static Node     nodes [20];
static unsigned current_node;
static unsigned num_nodes = 0;


static char *ordinals [ ] = {"", "first", "second", "third", "fourth", "fifth",
			"sixth", "seventh", "eighth", "ninth", "tenth",
			"eleventh", "twelfth", "thirteenth", "fourteenth"};


void AbortAddElement ( )
{
    if (num_nodes == 0)
	SetNormalMode ( );
    else {
	current_node = 1;
	ChangeStatusLine ("Aborted.  Select first node:", True);
    }
}



void DoAddElement (node)
    Node node;
{
    static char      message [80];
    unsigned         i;
    unsigned         max;
    Element          element;


    nodes [current_node] = node;

    if (current_node != num_nodes) {
	sprintf (message, "Selected node%s", current_node == 1 ? "" : "s");
	for (i = 1; i <= current_node; i ++)
	    sprintf (message + strlen (message), "%s %d", i == 1 ? "" : ",",
				nodes [i] != NULL ? nodes [i] -> number : 0);
	sprintf (message + strlen (message), ".  Select %s node:",
						ordinals [++ current_node]);
	ChangeStatusLine (message, True);
	changeflag = True;
	return;
    }


    element = (Element) TreeMaximum (problem.element_tree);
    max = element != NULL ? element -> number : 0;

    element = CreateElement (max + 1, ElementListDefinition (element_l));
    element -> material = MaterialDialogActive (material_d);
    for (i = 1; i <= num_nodes; i ++)
	element -> node [i] = nodes [i];

    DrawElement (element);

    current_node = 1;
    sprintf (message, "Added element %d.  Select first node:", max + 1);
    ChangeStatusLine (message, True);

    ElementDialogUpdate (element_d, problem.element_tree, NULL, NULL, NULL);
    ElementDialogDisplay (element_d, element);
}


void AddElementAP ( )
{
    char       *status;
    struct node dummy;
    Item        found;


    if ((status = GetTextNumber (&dummy.number)) != NULL)
	return;

    if (dummy.number == 0) {
	DoAddElement (NULL);
	return;
    }

    found = TreeSearch (problem.node_tree, (Item) &dummy);
    if (found == NULL) {
	error ("Node %d does not exist.", dummy.number);
	return;
    }

    DoAddElement (found);
}


void AddElementCB (w, client_data, call_data)
    Widget    w;
    XtPointer client_data;
    XtPointer call_data;
{
    Node             node;
    Drawn            drawn;
    Figure           figure;
    DrawingReport   *report;
    FigureAttributes attributes;


    report = (DrawingReport *) call_data;
    if (report -> event -> type != ButtonPress)
	return;

    if (report -> event -> xbutton.button == 3)
	QuitEdit ( );

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

    DoAddElement (node);
}


void EditAddElement ( )
{
    Definition		active_definition;


    if (MaterialDialogActive (material_d) == NULL) {
	error ("No active material defined.");
	return;
    }

    active_definition = ElementListDefinition (element_l);

    if (active_definition == NULL) {
	error ("No active element type defined.");
	return;
    }

    SetEditMode ( );
    ChangeStatusLine ("Select first node:", True);

    current_node = 1;
    num_nodes = active_definition -> numnodes;

    XtRemoveAllCallbacks (drawing, XtNbuttonCallback);
    XtAddCallback (drawing, XtNbuttonCallback, AddElementCB, NULL);

    AssignQuitAbort (QuitEdit, "QuitEdit", AbortAddElement, "AbortAddElement");

    XtOverrideTranslations (entry,
	XtParseTranslationTable ("<Key>Return: AddElementAP()"));
}


void ComputeCenter (element, x, y)
    Element element;
    float  *x;
    float  *y; 
{
    unsigned	i;
    unsigned	n;
    float	cx;
    float	cy;


    cx = 0;
    cy = 0;
    n = element -> definition -> shapenodes;

    for (i = 1; i <= n; i ++)
	if (element -> node [i] != NULL) {
	    cx += element -> node [i] -> x;
	    cy += element -> node [i] -> y;
	}

    cx /= n;
    cy /= n;

    *x = cx;
    *y = cy;
}


int DrawElement (element)
    Element element;
{
    Point            points [100];
    Figure           fig;
    FigureAttributes attr;
    Drawn            drawn;
    unsigned         num;
    unsigned         j;
    Item	     found;
    float	     x;
    float	     y;
    Figure	     label;
    char	     buffer [10];

    found = TreeInsert (problem.element_tree, element);
    if (found != (Item) element)
	return 1;

    if (DW_SetFont (drawing, canvas -> label_font) == False)
        (void) DW_SetFont (drawing, "fixed");

    if (element -> numdistributed && element -> distributed[1] -> color) {
        if (DW_SetForeground (drawing,element->distributed[1]->color) == False)
	    (void) DW_SetForeground (drawing, "black");
    }
    else if (element -> material -> color) {
        if (DW_SetForeground (drawing, element -> material -> color) == False)
            (void) DW_SetForeground (drawing, "black");
    }
    else {
	if (DW_SetForeground (drawing, canvas -> element_color) == False)
	    (void) DW_SetForeground (drawing, "black");
    }

    num = element -> definition -> shapenodes;

    if (num > 2) {
	for (j = 1; j <= num; j ++) {
	    if (element -> node [j] == NULL || element -> node [j] -> z != 0)
		break;

	    points [j - 1].x = element -> node [j] -> x;
	    points [j - 1].y = element -> node [j] -> y;
	}

	if (j > num) {
	    points [num].x = element -> node [1] -> x;
	    points [num].y = element -> node [1] -> y;
	    fig = DW_DrawPolygon (drawing, True, points, num + 1);
	} else
	    fig = NULL;

    } else {
	if (element -> node [1] != NULL && element -> node [2] != NULL)
	    if (element -> node [1] -> z == 0 && element -> node [2] -> z == 0)
		fig = DW_DrawLine (drawing, element -> node [1] -> x,
			element -> node [1] -> y, element -> node [2] -> x,
			element -> node [2] -> y);
	    else
		fig = NULL;
	else
	    fig = NULL;
    }

    if (fig != NULL && canvas -> element_numbers == True) {
	sprintf (buffer, "%d", element -> number);
	ComputeCenter (element, &x, &y);
	label = DW_DrawText (drawing, True, x, y, buffer);
    } else
	label = NULL;

    drawn = (Drawn) XtNew (struct drawn);
    drawn -> type = DrawnElement;
    drawn -> figure = fig;
    drawn -> label = label;
    element -> aux = (char *) drawn;

    attr.user_data = (char *) element;

    if (fig != NULL)
	DW_SetAttributes (drawing, fig, DW_FigureUserData, &attr);
    if (label != NULL)
	DW_SetAttributes (drawing, label, DW_FigureUserData, &attr);

    for (j = 1; j <= element -> definition -> numnodes; j ++)
	if (element -> node [j] != NULL) {
	    if (element -> node [j] -> aux == NULL) {
		drawn = (Drawn) XtNew (struct drawn);
		drawn -> type = DrawnNode;
		drawn -> figure = NULL;
		drawn -> label = NULL;
		drawn -> ref_count = 0;
		element -> node [j] -> aux = (char *) drawn;
	    }

	    drawn = (Drawn) element -> node [j] -> aux;
	    drawn -> ref_count ++;
	    DW_RaiseFigure (drawing, drawn -> figure);
	    DW_RaiseFigure (drawing, drawn -> label);
	}

    return 0;
}


void MoveElement (element, old_nodes)
    Element element;
    Node   *old_nodes;
{
    unsigned		i;
    unsigned		numnodes;
    unsigned		shapenodes;
    Drawn		drawn;
    Figure		fig;
    Figure		label;
    FigureAttributes	attributes;


    drawn = (Drawn) element -> aux;
    if (drawn == NULL) {
	DrawElement (element);
	drawn = (Drawn) element -> aux;
    }

    fig = drawn -> figure;
    label = drawn -> label;
    DW_GetAttributes (drawing, fig, &attributes);

    numnodes = element -> definition -> numnodes;
    shapenodes = element -> definition -> shapenodes;

    for (i = 1; i <= numnodes; i ++) {
	if (old_nodes [i] != NULL) {
	    drawn = (Drawn) old_nodes [i] -> aux;
	    drawn -> ref_count --;
	}

	if (element -> node [i] != NULL) {
	    drawn = (Drawn) element -> node [i] -> aux;
	    drawn -> ref_count ++;
	}

	if (i <= shapenodes && fig != NULL) {
	    attributes.points [i - 1].x = element -> node [i] -> x;
	    attributes.points [i - 1].y = element -> node [i] -> y;
	    if (i == 1) {
		attributes.points [shapenodes].x = element -> node [i] -> x;
		attributes.points [shapenodes].y = element -> node [i] -> y;
	    }
	}
    }

    DW_SetAttributes (drawing, fig, DW_FigurePoints, &attributes);

    if (label != NULL) {
	DW_GetAttributes (drawing, label, &attributes);
	ComputeCenter (element, &attributes.x, &attributes.y);
	DW_SetAttributes (drawing, label, DW_FigureLocation, &attributes);
    }
}
