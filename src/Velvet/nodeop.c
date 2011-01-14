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
# include "procedures.h"
# include "problem.h"
# include "globals.h"
# include "vfe.h"
# include "text_entry.h"
# include "objects.h"
# include "error.h"
# include "Node.h"

extern ConstraintDialog	constraint_d;
extern NodeDialog node_d;

void EditAddNode (void)
{

   if (ConstraintDialogActive (constraint_d) == NULL) {
	error ("No active constraint defined.");
	return;
   }

   SetEditMode ( );
   ChangeStatusLine ("Nodal coordinates:", True);

   XtRemoveAllCallbacks (drawing, XtNbuttonCallback);
   XtAddCallback (drawing, XtNbuttonCallback, AddNodeCB, NULL);

   XtOverrideTranslations (entry, 
           XtParseTranslationTable ("<Key>Return: AddNodeAP()"));
}

void AddNodeCB (Widget w, XtPointer client_data, XtPointer call_data)
{
   DrawingReport 	*report;

   report = (DrawingReport *) call_data;

   if (report -> event -> type != ButtonPress)
	return;

   if (report -> event -> xbutton.button == 1)
      DoAddNode (report -> snapped.x, report -> snapped.y, 0.0);

   if (report -> event -> xbutton.button == 3)
        QuitEditCB (w, client_data, call_data);
}

void AddNodeAP (Widget w, XEvent *event, String *params, Cardinal *num)
{
   char *status;
   float x,y,z = 0;

   status = GetTextCoordinates (&x, &y, NULL);
   if (status == NULL)
      DoAddNode (x, y, z);
}

void DoAddNode (float x, float y, float z)
{
   char     message [40];
   Node     node;
   unsigned max;

   node = (Node) TreeMaximum (problem.node_tree);
   max = node != NULL ? node -> number : 0;

   node = CreateNode (max + 1);
   node -> constraint = ConstraintDialogActive (constraint_d);
   node -> x = x;
   node -> y = y;
   node -> z = z;

   DrawNode (node);

   sprintf (message, "Added node %d.  Nodal coordinates:", node -> number);
   ChangeStatusLine (message, True);


   /* The node dialog needs to know that the new node is in
      the tree before it can display it. */

   NodeDialogUpdate (node_d, problem.node_tree, NULL, NULL);
   NodeDialogDisplay (node_d, node);

   changeflag = True;
}


static Node             moved_node;
static Figure           moved_figure;
static Figure           ghost_figure;
static FigureAttributes attr;


static int MoveNode (Item item)
{
    unsigned i;
    unsigned numnodes;
    Element  element = (Element) item;
    Drawn    drawn;


    numnodes = element -> definition -> shapenodes;

    for (i = 1; i <= numnodes; i ++)
	if (element -> node [i] == moved_node) {
	    DW_GetAttributes (drawing, ((Drawn) element -> aux) -> figure,
			      &attr);
	    attr.points [i - 1].x = moved_node -> x;
	    attr.points [i - 1].y = moved_node -> y;
	    if (numnodes > 2 && i == 1) {
		attr.points [numnodes].x = moved_node -> x;
		attr.points [numnodes].y = moved_node -> y;
	    }
	    drawn = (Drawn) element -> aux;

	    DW_SetAttributes (drawing, drawn -> figure, DW_FigurePoints, &attr);

	    if (drawn -> label != NULL) {
		ComputeCenter (element, &attr.x, &attr.y);
		DW_SetAttributes (drawing,drawn->label,DW_FigureLocation,&attr);
	    }
	}

    return 0;
}


void DoWalkNode (Node node)
{
    Drawn drawn;


    moved_node = node;
    attr.x = node -> x;
    attr.y = node -> y;
    drawn = (Drawn) node -> aux;

    if (drawn == NULL) {
	DrawNode (node);
	drawn = (Drawn) node -> aux;
    }

    DW_SetAutoRedraw (drawing, False);
    DW_SetAttributes (drawing, drawn -> figure, DW_FigureLocation, &attr);
    DW_SetAttributes (drawing, drawn -> label, DW_FigureLocation, &attr);
    (void) TreeSetIterator (problem.element_tree, MoveNode);
    (void) TreeIterate (problem.element_tree);
    DW_SetAutoRedraw (drawing, True);

    NodeDialogDisplay (node_d, node);
    changeflag = True;
}


static void
DeleteNodeGroup(Figure *figures, unsigned nfigures)
{
    unsigned         i;
    Figure           fig;
    Drawn            drawn;
    Node             node;
    unsigned         numleft;
    Boolean	     firsttime;
    Boolean	     newinfo;


    numleft = 0;
    firsttime = True;
    newinfo = False;

    for (i = 0; i < nfigures; i ++) {
	fig = figures [i];
	DW_GetAttributes (drawing, fig, &attr);

	if (attr.user_data == NULL || attr.type == TextFigure)
	    continue;

	node = (Node) attr.user_data;
	drawn = (Drawn) node -> aux;
	if (drawn -> type != DrawnNode)
	    continue;

	if (drawn -> ref_count) {
	    numleft ++;
	    continue;
	}

	if (firsttime == True) {
	    firsttime = False;
	    DW_SetAutoRedraw (drawing, False);
	}

	newinfo = True;
	DW_RemoveFigure (drawing, drawn -> figure);
	DW_RemoveFigure (drawing, drawn -> label);
	(void) TreeDelete (problem.node_tree, node);
	DestroyNode (node);
    }


    if (newinfo) {
        NodeDialogDisplay (node_d, NULL);
	NodeDialogUpdate (node_d, problem.node_tree, NULL, NULL);
    }


    if (firsttime == False) {
	DW_SetAutoRedraw (drawing, True);
	changeflag = True;
    }

    if (numleft)
	error ("Warning: elements still reference %d node%s.", numleft,
						numleft > 1 ? "s" : "");

    XtFree ((char *) figures);
}


static void
DoDeleteNode(Node node)
{
    static char message [80];
    Drawn drawn = (Drawn) node -> aux;
    Node newnode;


    if (drawn -> ref_count) {
	error ("Node %d is still referenced by %d element%s.", node -> number,
		drawn -> ref_count, drawn -> ref_count > 1 ? "s" : "");
	return;
    }

    if (drawn -> figure != NULL) {
	DW_SetAutoRedraw (drawing, False);
	DW_RemoveFigure (drawing, drawn -> figure);
	if (drawn -> label != NULL)
	    DW_RemoveFigure (drawing, drawn -> label);
	DW_SetAutoRedraw (drawing, True);
    }

    sprintf (message, "Node %d deleted.  Select node:", node -> number);

    if (node == NodeDialogActive (node_d)) {
	newnode = (Node) TreePredecessor (problem.node_tree, node);
	if (newnode == NULL)
	    newnode = (Node) TreeSuccessor (problem.node_tree, node);

	NodeDialogDisplay (node_d, newnode);
    }

    (void) TreeDelete (problem.node_tree, node);
    NodeDialogUpdate (node_d, problem.node_tree, NULL, NULL);

    DestroyNode (node);
    ChangeStatusLine (message, True);
    changeflag = True;
}


void DeleteNodeCB (Widget w, XtPointer client_data, XtPointer call_data)
{
    DrawingReport   *report;
    FigureAttributes attributes;
    Figure           figure;
    Drawn            drawn;
    Node             node;


    report = (DrawingReport *) call_data;

    if (report -> event -> type != ButtonPress)
	return;

    if (report -> event -> xbutton.button == 2) {
	SelectGroup (call_data, DeleteNodeGroup);
	return;
    }

    if (report -> event -> xbutton.button == 3)
         QuitEditCB (w, client_data, call_data);

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

    DoDeleteNode (node);
}


void DeleteNodeAP (Widget w, XEvent *event, String *params, Cardinal *num)
{
    char       *status;
    struct node dummy;
    Item        found;


    if ((status = GetTextNumber (&dummy.number)) != NULL) {
	if (!strcmp (status, "w"))
	    SelectGroup (NULL, DeleteNodeGroup);
	return;
    }

    found = TreeSearch (problem.node_tree, (Item) &dummy);
    if (found == NULL) {
	error ("Node %d does not exist.", dummy.number);
	return;
    }

    DoDeleteNode ((Node) found);
}


void EditDeleteNode (void)
{
    Arg		arglist [1];

    SetEditMode ( );
    ChangeStatusLine ("Select node:", True);

    XtRemoveAllCallbacks (drawing, XtNbuttonCallback);
    XtAddCallback (drawing, XtNbuttonCallback, DeleteNodeCB, NULL);

    XtSetArg (arglist [0], XtNcursorName, "dotbox");
    XtSetValues (drawing, arglist, 1);

    XtOverrideTranslations (entry,
	XtParseTranslationTable ("<Key>Return: DeleteNodeAP()"));
}


void EditNodeCB (Widget w, XtPointer client_data, XtPointer call_data)
{
    DrawingReport   *report;
    FigureAttributes attributes;
    Figure           figure;
    Drawn            drawn;
    Node             node;


    report = (DrawingReport *) call_data;

    if (report -> event -> type != ButtonPress)
	return;

    if (report -> event -> xbutton.button == 3)
         QuitEditCB (w, client_data, call_data);

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

    NodeDialogDisplay (node_d, node);
    NodeDialogPopup (node_d);
}


void EditNodeAP (Widget w, XEvent *event, String *params, Cardinal *num)
{
    char       *status;
    struct node dummy;
    Item        found;


    if ((status = GetTextNumber (&dummy.number)) != NULL)
	return;

    found = TreeSearch (problem.node_tree, (Item) &dummy);
    if (found == NULL) {
	error ("Node %d does not exist.", dummy.number);
	return;
    }

    NodeDialogDisplay (node_d, (Node) found);
    NodeDialogPopup (node_d);
}


void EditNodeNumber (void)
{
    SetEditMode ( );
    ChangeStatusLine ("Select node:", True);

    XtRemoveAllCallbacks (drawing, XtNbuttonCallback);
    XtAddCallback (drawing, XtNbuttonCallback, EditNodeCB, NULL);

    XtOverrideTranslations (entry,
	XtParseTranslationTable ("<Key>Return: EditNodeAP()"));
}


int DrawNode (Node node)
{
    Figure		fig;
    Figure		label;
    char		buffer [10];
    FigureAttributes 	attr;
    Item		found;
    Drawn               drawn;


    found = TreeInsert (problem.node_tree, (Item) node);
    if (found != (Item) node)
	return 1;

    if (DW_SetFont (drawing, canvas -> label_font) == False)
        (void) DW_SetFont (drawing, "fixed");

    if (node -> force && node -> force -> color) {
        if (DW_SetForeground (drawing, node -> force -> color) == False)
            (void) DW_SetForeground (drawing, "black");
    }
    else if (node -> constraint -> color) {
        if (DW_SetForeground (drawing, node -> constraint -> color) == False)
            (void) DW_SetForeground (drawing, "black");
    }
    else {
        if (DW_SetForeground (drawing, canvas -> node_color) == False)
            (void) DW_SetForeground (drawing, "black");
    }

    if (node -> z == 0) {

	fig = DW_FillArc (drawing, False, node -> x, node -> y,
                        6.0, 6.0, 0.0, 360.0);

	if (canvas -> node_numbers == True) {
	    sprintf (buffer, " %d", node -> number);
	    label = DW_DrawText (drawing, True, node -> x, node -> y, buffer);
	} else
	    label = NULL;

    } else
	fig = label = NULL;

    if (node -> aux == NULL) {
	drawn = (Drawn) XtNew (struct drawn);
	node -> aux = (char *) drawn;
	drawn -> type = DrawnNode;
	drawn -> ref_count = 0;
    } else
	drawn = (Drawn) node -> aux;

    drawn -> figure = fig;
    drawn -> label = label;


    attr.user_data = (char *) node;

    if (fig != NULL)
	DW_SetAttributes (drawing, fig, DW_FigureUserData, &attr);
    if (label != NULL)
        DW_SetAttributes (drawing, label, DW_FigureUserData, &attr);

    return 0;
}

void QuitMoveNodeCB (Widget w, XtPointer closure, XtPointer data)
{
     QuitMoveNode(w, NULL, NULL, NULL);
}

void QuitMoveNode (Widget w, XEvent *event, String *params, Cardinal *num)
{
    DW_RemoveFigure (drawing, ghost_figure);
    DW_SetInteractive (drawing, False);
    SetNormalMode ( );
}


void WalkNodeCB (Widget w, XtPointer client_data, XtPointer call_data)
{
    DrawingReport *report;
    Node           node;


    report = (DrawingReport *) call_data;
    node = (Node) client_data;

    if (report -> event -> type == ButtonPress) {
	if (report -> event -> xbutton.button == 3)
         QuitMoveNodeCB (w, client_data, call_data);

	if (report -> event -> xbutton.button > 2)
	    return;

	node -> x = report -> snapped.x;
	node -> y = report -> snapped.y;
	DW_RemoveFigure (drawing, ghost_figure);
	DW_SetInteractive (drawing, False);
	DoWalkNode (node);
	SetNormalMode ( );

    } else if (report -> event -> type == MotionNotify) {
	attr.x = report -> snapped.x;
	attr.y = report -> snapped.y;
	DW_SetAttributes (drawing, ghost_figure, DW_FigureLocation, &attr);
    }
}


void WalkNodeAP (Widget widget, XEvent *event, String *params, Cardinal *num)
{
    char *status;
    float x, y;

    status = GetTextCoordinates (&x, &y, NULL);
    if (status != NULL)
	return;

   moved_node -> x = x;
   moved_node -> y = y;
   QuitMoveNode (widget, event, params, num);
   DoWalkNode (moved_node);
}


void DoMoveNode (Node node, Boolean motion)
{
    static char buffer [80];


    SetEditMode ( );
    sprintf (buffer, "Nodal coordinates for node %d:", node -> number);
    ChangeStatusLine (buffer, True);

    AssignQuitAbort (QuitMoveNodeCB, "QuitMoveNode", QuitMoveNodeCB,"QuitMoveNode");

    XtRemoveAllCallbacks (drawing, XtNbuttonCallback);
    XtAddCallback (drawing, XtNbuttonCallback, WalkNodeCB, node);

    if (motion == True) {
	XtRemoveAllCallbacks (drawing, XtNmotionCallback);
	XtAddCallback (drawing, XtNmotionCallback, WalkNodeCB, node);
	DW_SetInteractive (drawing, True);
	ghost_figure = DW_FillArc (drawing, False, node -> x, node -> y,
					6.0, 6.0, 0.0, 360.0);
    } else {
	moved_figure = NULL;
	ghost_figure = NULL;
    }

    moved_node = node;
    XtOverrideTranslations (entry,
	XtParseTranslationTable ("<Key>Return: WalkNodeAP()"));
}


void MoveNodeCB (Widget w, XtPointer client_data, XtPointer call_data)
{
    DrawingReport   *report;
    FigureAttributes attributes;
    Figure	     figure;
    Node	     node;
    Drawn	     drawn;


    report = (DrawingReport *) call_data;

    if (report -> event -> type != ButtonPress)
	return;

    if (report -> event -> xbutton.button == 3)
         QuitEditCB (w, client_data, call_data);

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

    moved_figure = figure;
    DoMoveNode (node, False);
}


void MoveNodeAP (Widget w, XEvent *event, String *params, Cardinal *num)
{
    char       *status;
    struct node dummy;
    Item        found;


    if ((status = GetTextNumber (&dummy.number)) != NULL)
	return;

    found = TreeSearch (problem.node_tree, &dummy);
    if (found == NULL) {
	error ("Node %d does not exist.", dummy.number);
	return;
    }

    DoMoveNode ((Node) found, False);
}


void MoveNodeNumber (void)
{
    SetEditMode ( );
    ChangeStatusLine ("Select node: ", True);

    XtRemoveAllCallbacks (drawing, XtNbuttonCallback);
    XtAddCallback (drawing, XtNbuttonCallback, MoveNodeCB, NULL);

    XtOverrideTranslations (entry,
	XtParseTranslationTable ("<Key>Return: MoveNodeAP()"));
}

static double mass_value = 0.0;

static void
AssignMassGroup(Figure *figures, unsigned nfigures)
{
    unsigned         i;
    Figure           fig;
    Drawn            drawn;
    Node             node;


    for (i = 0; i < nfigures; i ++) {
	fig = figures [i];
	DW_GetAttributes (drawing, fig, &attr);

	if (attr.user_data == NULL || attr.type == TextFigure)
	    continue;

	node = (Node) attr.user_data;
	drawn = (Drawn) node -> aux;
	if (drawn -> type != DrawnNode)
	    continue;

        node -> m = mass_value;
    }


    changeflag = True;

    return;
}


void DoAssignMass (Node node)
{
    char	message [80];

    node -> m = mass_value;

    if (node == NodeDialogActive (node_d))
       NodeDialogDisplay (node_d, node);

    sprintf (message, "Mass assigned to node %d.  Select node:", node -> number);

    ChangeStatusLine (message, True);
    changeflag = True;
}


void AssignMassCB (Widget w, XtPointer client_data, XtPointer call_data)
{
    DrawingReport   *report;
    FigureAttributes attributes;
    Figure           figure;
    Drawn            drawn;
    Node             node;


    report = (DrawingReport *) call_data;

    if (report -> event -> type != ButtonPress)
	return;

    if (report -> event -> xbutton.button == 2) {
	SelectGroup (call_data, AssignMassGroup);
	return;
    }

    if (report -> event -> xbutton.button == 3)
         QuitEditCB (w, client_data, call_data);

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

    DoAssignMass (node);
}


void AssignMassAP (Widget w, XEvent *event, String *params, Cardinal *num)
{
    char       *status;
    struct node dummy;
    Item        found;


    if ((status = GetTextNumber (&dummy.number)) != NULL) {
	if (!strcmp (status, "w"))
	    SelectGroup (NULL, AssignMassGroup);
	return;
    }

    found = TreeSearch (problem.node_tree, (Item) &dummy);
    if (found == NULL) {
	error ("Node %d does not exist.", dummy.number);
	return;
    }

    DoAssignMass ((Node) found);
}

void SetMassAP (Widget w, XEvent *event, String *params, Cardinal *num)
{
    Arg		args [1];
    char	*status;
    float	m;

    status = GetTextCoordinates (&m, NULL, NULL);
    if (status == NULL)
       mass_value = m;
    else
       return; 

    ChangeStatusLine ("Select node:", True);

    XtSetArg (args [0], XtNcursorName, "dotbox");
    XtSetValues (drawing, args, 1);

    XtAddCallback (drawing, XtNbuttonCallback, AssignMassCB, NULL);
    XtOverrideTranslations (entry,
	XtParseTranslationTable ("<Key>Return: AssignMassAP()"));
}

void EditNodalMass (void)
{
    SetEditMode ( );
    ChangeStatusLine ("Enter mass value:", True);

    XtRemoveAllCallbacks (drawing, XtNbuttonCallback);

    XtOverrideTranslations (entry,
	XtParseTranslationTable ("<Key>Return: SetMassAP()"));
}
