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
 * File:	node.c							*
 *									*
 * Description:	This file contains the public and private function and	*
 *		type definitions for the node dialog box.		*
 ************************************************************************/

# include <algorithm>
# include <stdio.h>
# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <X11/Shell.h>
# include <X11/Xaw/AsciiText.h>
# include <X11/Xaw/Command.h>
# include <X11/Xaw/Label.h>
# include <X11/Xaw/MenuButton.h>
# include <X11/Xaw/Repeater.h>
# include <X11/Xaw/SimpleMenu.h>
# include <X11/Xaw/SmeBSB.h>
# include "Layout.h"
# include "Node.h"
# include "TabGroup.h"
# include "util.h"
# include "post.h"
# include "setaux.hpp"

# ifndef X_NOT_STDC_ENV
# include <stdlib.h>
# endif


struct node_dialog {
    Widget         shell;	/* topLevelShell  <specified>		  */
    Widget         layout;	/*	Layout  layout			  */
    Widget         number;	/*	     Label  number		  */
    Widget	   mass;	/* 	     AsciiText  mass	          */
    Widget         up;		/*	     Repeater  up		  */
    Widget         down;	/*	     Repeater  down		  */
    Widget         f_name;	/*	     AsciiText  forceName	  */
    Widget         f_button;	/*	     MenuButton  forceButton	  */
    Widget         f_menu;	/*	     SimpleMenu  forceMenu	  */
    Widget         c_name;	/*	     AsciiText  constraintName	  */
    Widget         c_button;	/*	     MenuButton  constraintButton */
    Widget         c_menu;	/*	     SimpleMenu  constraintMenu	  */
    Widget         loc_x;	/*	     AsciiText  locationX	  */
    Widget         loc_y;	/*	     AsciiText  locationY	  */
    Widget         loc_z;	/*	     AsciiText  locationZ	  */
    Widget         trans_x;	/*	     AsciiText  translationX	  */
    Widget         trans_y;	/*	     AsciiText  translationY	  */
    Widget         trans_z;	/*	     AsciiText  translationZ	  */
    Widget         rot_x;	/*	     AsciiText  rotationX	  */
    Widget         rot_y;	/*	     AsciiText  rotationY	  */
    Widget         rot_z;	/*	     AsciiText  rotationZ	  */
    Widget         help;	/*	     MenuButton  help		  */
    Widget         accept;	/*	     Command  accept		  */
    Widget         dismiss;	/*	     Command  dismiss		  */
    Widget         nuke;	/*	     Command  delete		  */
    Widget         nu;		/*	     Command  new		  */
    Widget         copy;	/*	     Command  copy		  */
    XtCallbackProc callback;
    XtPointer	   closure;
    unsigned	   new_copy;
    Boolean	   new_forces;
    Boolean	   new_constraints;
    Node           active;
    Problem::NodeSet *nodes;
    Problem::ForceSet *forces;
    Problem::ConstraintSet *constraints;
};

static String labels [ ] = {
    "Number:", "Mass:", 
    "X:", "Y:", "Z:", "Location", "Translation", "Rotation"
};

static String names [ ] = {
    "numberLabel", "massLabel", 
    "x", "y", "z", "location", "translation", "rotation"
};


/* Bitmaps */

#define up_width 12
#define up_height 12
static char up_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x60, 0x00, 0xf0, 0x00, 0xf8, 0x01,
   0xfc, 0x03, 0xfe, 0x07, 0xfe, 0x07, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};

#define down_width 12
#define down_height 12
static char down_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xfe, 0x07, 0xfe, 0x07, 0xfc, 0x03,
   0xf8, 0x01, 0xf0, 0x00, 0x60, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};

static Pixmap up_bitmap;
static Pixmap down_bitmap;


/* Resources */

static Pixel highlight;

static String layout_string = "\
vertical { \
    4 \
    horizontal { \
	4 \
	up \
	4 \
	down \
	4 \
	numberLabel \
	4 \
	number \
        (width forceButton + width forceName + width constraintButton + 16 - \
         16 - width up - width down - width numberLabel - width number - \
         width massLabel - 4) \
        massLabel \
        4 \
        mass \
	4 <+inf -100%> \
    } \
    4 \
    separator1 <+inf -100% *> \
    4 \
    horizontal { \
	4 \
	forceButton \
	4 \
	forceName <+inf -100% *> \
	4 \
	constraintButton \
	4 \
	constraintName <+inf -100% *> \
	4 \
    } \
    4 \
    separator2 <+inf -100% *> \
    4 \
    horizontal { \
	4 \
	vertical { \
	    height location \
	    4 \
	    ((height locationX - height x) / 2) \
	    x \
	    ((height locationX - height x) / 2) \
	    4 \
	    ((height locationY - height y) / 2) \
	    y \
	    ((height locationY - height y) / 2) \
	    4 \
	    ((height locationZ - height z) / 2) \
	    z \
	    ((height locationZ - height z) / 2) \
	} \
	4 \
	vertical { \
	    location <+inf -100% *> \
	    4 \
	    locationX <+inf -100% *> \
	    4 \
	    locationY <+inf -100% *> \
	    4 \
	    locationZ <+inf -100% *> \
	} \
	4 \
	vertical { \
	    translation <+inf -100% *> \
	    4 \
	    translationX <+inf -100% *> \
	    4 \
	    translationY <+inf -100% *> \
	    4 \
	    translationZ <+inf -100% *> \
	} \
	4 \
	vertical { \
	    rotation <+inf -100% *> \
	    4 \
	    rotationX <+inf -100% *> \
	    4 \
	    rotationY <+inf -100% *> \
	    4 \
	    rotationZ <+inf -100% *> \
	} \
	4 \
    } \
    4 \
    separator3 <+inf -100% *> \
    4 \
    horizontal { \
	4 \
	help \
	4 <+inf -100%> \
	accept \
	4 <+inf -100%> \
	dismiss \
	4 <+inf -100%> \
	delete \
	4 <+inf -100%> \
	new \
	4 <+inf -100%> \
	copy \
	4 \
    } \
    4 \
}";

static Arg color_args [ ] = {
    {XtNborderColor, (XtArgVal) &highlight},
};

static Arg shell_args [ ] = {
    {XtNtitle,    (XtArgVal) NULL},
    {XtNiconName, (XtArgVal) NULL},
};

static Arg layout_args [ ] = {
    {XtNlayout, (XtArgVal) NULL},
};

static Arg edit_text_args [ ] = {
    {XtNeditType,    (XtArgVal) XawtextEdit},
    {XtNborderWidth, (XtArgVal) 0},
    {XtNpieceSize,   (XtArgVal) 32},
    {XtNcursorName,  (XtArgVal) "left_ptr"},
};

static Arg read_text_args [ ] = {
    {XtNeditType,     (XtArgVal) XawtextRead},
    {XtNpieceSize,    (XtArgVal) 32},
    {XtNcursorName,   (XtArgVal) "left_ptr"},
    {XtNdisplayCaret, (XtArgVal) False},
};

static Arg button_args [ ] = {
    {XtNlabel,       (XtArgVal) ""},
    {XtNmenuName,    (XtArgVal) ""},
    {XtNborderWidth, (XtArgVal) 0},
};

static Arg number_args [ ] = {
    {XtNlabel,       (XtArgVal) ""},
    {XtNresize,      (XtArgVal) True},
    {XtNborderWidth, (XtArgVal) 0},
};

static Arg label_args [ ] = {
    {XtNlabel,       (XtArgVal) ""},
    {XtNborderWidth, (XtArgVal) 0},
};

static Arg core_args [ ] = {
    {XtNwidth,  (XtArgVal) 3},
    {XtNheight, (XtArgVal) 3},
};

static Arg repeater_args [ ] = {
    {XtNbitmap, (XtArgVal) NULL},
};


/* Translation tables */

static String text_table =
"<Key>Return: NodeDialogAction(accept)\n\
 <Key>Escape: NodeDialogAction(dismiss)\n\
 Ctrl<Key>d:  NodeDialogAction(delete)\n\
 Ctrl<Key>c:  NodeDialogAction(copy)\n\
 Ctrl<Key>n:  NodeDialogAction(new)\n\
 Ctrl<Key>h:  NodeDialogAction(help)\n\
 <Btn1Down>:  SetFocus() select-start()";

static XtTranslations text_translations;


static String command_table =
"<Key>Return:  NodeDialogAction(accept)\n\
 <Key>Escape:  NodeDialogAction(dismiss)\n\
 Ctrl<Key>d:   NodeDialogAction(delete)\n\
 Ctrl<Key>c:   NodeDialogAction(copy)\n\
 Ctrl<Key>n:   NodeDialogAction(new)\n\
 Ctrl<Key>h:   NodeDialogAction(help)\n\
 <Key>space:   AutoRepeat(off) set()\n\
 <KeyUp>space: AutoRepeat(saved) notify() unset()";

static XtTranslations command_translations;


static String button_table =
"<Key>Return: NodeDialogAction(accept)\n\
 <Key>Escape: NodeDialogAction(dismiss)\n\
 Ctrl<Key>d:  NodeDialogAction(delete)\n\
 Ctrl<Key>c:  NodeDialogAction(copy)\n\
 Ctrl<Key>n:  NodeDialogAction(new)\n\
 Ctrl<Key>h:  NodeDialogAction(help)\n\
 <BtnDown>:   PostMenu()\n\
 <Key>space:  PostMenu()";

static XtTranslations button_translations;


static String repeater_table =
"<Key>Return:  NodeDialogAction(accept)\n\
 <Key>Escape:  NodeDialogAction(dismiss)\n\
 Ctrl<Key>d:   NodeDialogAction(delete)\n\
 Ctrl<Key>c:   NodeDialogAction(copy)\n\
 Ctrl<Key>n:   NodeDialogAction(new)\n\
 Ctrl<Key>h:   NodeDialogAction(help)\n\
 <Key>space:   AutoRepeat(off) set()\n\
 <KeyUp>space: AutoRepeat(saved) notify() unset()";

static XtTranslations repeater_translations;


static String help_table =
"<Key>Return: NodeDialogAction(accept)\n\
 <Key>Escape: NodeDialogAction(dismiss)\n\
 Ctrl<Key>d:  NodeDialogAction(delete)\n\
 Ctrl<Key>c:  NodeDialogAction(copy)\n\
 Ctrl<Key>n:  NodeDialogAction(new)\n\
 Ctrl<Key>h:  NodeDialogAction(help)\n\
 <Key>space:  PostMenu()";

static XtTranslations help_translations;


/* Help message. */

static String help_message = "\
The node form is used to define, edit and delete nodes. Use the arrows to \
change the current node.  The menus and fields above define properties of \
and objects assigned to the current node. Use the 'Accept' button to register \
your changes. 'Delete' erases the current node.  'New' empties all fields \
creating a new node.  'Copy' creates a new node without changing any fields.";


/* Menu creation variables */

static Cardinal		child_number;
static WidgetList	children;
static Dimension	max_width;
static XtWidgetGeometry	preferred;


/************************************************************************
 * Function:	SetForceEntry						*
 *									*
 * Description:	Sets the label of the next menu entry to the name of	*
 *		the specified force.					*
 ************************************************************************/

static int SetForceEntry (Force item)
{
    SetLabelString (children [child_number],item -> name.c_str());

    XtQueryGeometry (children [child_number ++], NULL, &preferred);
    if (preferred.width > max_width)
	max_width = preferred.width;

    return 0;
}


/************************************************************************
 * Function:	UpdateForceMenu						*
 *									*
 * Description:	Updates the force menu on popup if necessary to the	*
 *		list of current forces.					*
 ************************************************************************/

static void UpdateForceMenu (Widget w, XtPointer client_data, XtPointer call_data)
{
    Arg        args [2];
    char       buffer [32];
    Cardinal   overflow;
    Cardinal   num_forces;
    Cardinal   i;
    Cardinal   num_children;
    NodeDialog noded;


    noded = (NodeDialog) client_data;

    if (noded -> new_forces == False)
	return;

    XtSetArg (args [0], XtNchildren,    &children);
    XtSetArg (args [1], XtNnumChildren, &num_children);
    XtGetValues (w, args, 2);

    num_forces = noded->forces->size() + 1;

    for (i = num_children; i < num_forces; i ++) {
	sprintf (buffer, "force%d", i);
	XtCreateManagedWidget (XtNewString (buffer), smeBSBObjectClass,
			       noded -> f_menu, NULL, 0);
    }

    if (num_children > num_forces) {
	overflow = num_children - num_forces;
	XtUnmanageChildren (children + num_forces, overflow);
    }

    XtSetArg (args [0], XtNchildren,    &children);
    XtSetArg (args [1], XtNnumChildren, &num_children);
    XtGetValues (w, args, 2);

    XtQueryGeometry (children [0], NULL, &preferred);
    max_width = preferred.width;

    child_number = 1;
    std::for_each(noded->forces->begin(), noded->forces->end(), SetForceEntry);
    noded -> new_forces = False;

    XtSetArg (args [0], XtNwidth, max_width);
    XtSetValues (w, args, 1);
}


/************************************************************************
 * Function:	UpdateForceName						*
 *									*
 * Description:	Updates the force name on popdown if necessary.		*
 ************************************************************************/

static void UpdateForceName (Widget w, XtPointer client_data, XtPointer call_data)
{
    NodeDialog noded;


    if (!(w = XawSimpleMenuGetActiveEntry (w)))
	return;

    noded = (NodeDialog) client_data;

    if (!strcmp (XtName (w), "- none -"))
	SetTextString (noded -> f_name, "");
    else
	SetTextString (noded -> f_name, GetLabelString (w));
}


/************************************************************************
 * Function:	SetConstraintEntry					*
 *									*
 * Description:	Sets the label of the next menu entry to the name of	*
 *		the specified constraint.				*
 ************************************************************************/

static int SetConstraintEntry (Constraint item)
{
    SetLabelString (children [child_number], item -> name.c_str());

    XtQueryGeometry (children [child_number ++], NULL, &preferred);
    if (preferred.width > max_width)
	max_width = preferred.width;

    return 0;
}


/************************************************************************
 * Function:	UpdateConstraintMenu					*
 *									*
 * Description:	Updates the constraint menu on popup if necessary to	*
 *		the list of current constraints.			*
 ************************************************************************/

static void UpdateConstraintMenu (Widget w, XtPointer client_data, XtPointer call_data)
{
    Arg        args [2];
    char       buffer [32];
    Cardinal   overflow;
    Cardinal   num_constraints;
    Cardinal   i;
    Cardinal   num_children;
    NodeDialog noded;


    noded = (NodeDialog) client_data;

    if (noded -> new_constraints == False)
	return;

    XtSetArg (args [0], XtNchildren,    &children);
    XtSetArg (args [1], XtNnumChildren, &num_children);
    XtGetValues (w, args, 2);

    num_constraints = noded->constraints->size();

    if (num_constraints <= 0) {
	num_constraints ++;
	SetLabelString (children [0], "- no constraints defined -");
	XtQueryGeometry (children [0], NULL, &preferred);
	max_width = preferred.width;
    } else
	max_width = 0;

    for (i = num_children; i < num_constraints; i ++) {
	sprintf (buffer, "constraint%d", i);
	XtCreateManagedWidget (XtNewString (buffer), smeBSBObjectClass,
			       noded -> c_menu, NULL, 0);
    }

    if (num_children > num_constraints) {
	overflow = num_children - num_constraints;
	XtUnmanageChildren (children + num_constraints, overflow);
    }

    XtSetArg (args [0], XtNchildren,    &children);
    XtSetArg (args [1], XtNnumChildren, &num_children);
    XtGetValues (w, args, 2);

    child_number = 0;
    std::for_each(noded->constraints->begin(), noded->constraints->end(), SetConstraintEntry);
    noded -> new_constraints = False;

    XtSetArg (args [0], XtNwidth, max_width);
    XtSetValues (w, args, 1);
}


/************************************************************************
 * Function:	UpdateConstraintName					*
 *									*
 * Description:	Updates the constraint name on popdown if necessary.	*
 ************************************************************************/

static void UpdateConstraintName (Widget w, XtPointer client_data, XtPointer call_data)
{
    String     label;
    NodeDialog noded;


    if (!(w = XawSimpleMenuGetActiveEntry (w)))
	return;

    noded = (NodeDialog) client_data;
    label = GetLabelString (w);

    if (!strcmp (label, "- no constraints defined -"))
	SetTextString (noded -> c_name, "");
    else
	SetTextString (noded -> c_name, label);
}


/************************************************************************
 * Function:	SetNumber						*
 *									*
 * Description: Sets the label of the number widget.			*
 ************************************************************************/

static void SetNumber (NodeDialog noded, unsigned int number)
{
    char buffer [10];


    sprintf (buffer, "%u", number);
    SetLabelString (noded -> number, buffer);
}


/************************************************************************
 * Function:	Action							*
 *									*
 * Description:	An action procedure which emulates the pressing of the	*
 *		specified button.					*
 ************************************************************************/

static void Action (Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    if (XtClass (w) == topLevelShellWidgetClass)
	w = XtNameToWidget (w, "layout.dismiss");
    else
	w = XtNameToWidget (XtParent (w), params [0]);

    if (!strcmp (XtName (w), "help"))
	XtCallActionProc (w, "PostMenu", event, NULL, 0);
    else
	XtCallCallbacks (w, XtNcallback, NULL);
}


/************************************************************************
 * Function:	Up							*
 *									*
 * Description:	Displays the next highest numbered node.		*
 ************************************************************************/

static void Up (Widget w, XtPointer client_data, XtPointer call_data)
{
    NodeDialog noded = (NodeDialog) client_data;

    if (noded -> active == NULL)
	return;

    Node node = SetSuccessor(*noded->nodes, noded->active);

    if (node)
        NodeDialogDisplay (noded, node);
}


/************************************************************************
 * Function:	Down							*
 *									*
 * Description:	Displays the next lowest numbered node.			*
 ************************************************************************/

static void Down (Widget w, XtPointer client_data, XtPointer call_data)
{
    NodeDialog noded = (NodeDialog) client_data;

    if (noded -> active == NULL)
	return;

    Node node = SetPredecessor(*noded->nodes, noded->active);
    
    if (node)
        NodeDialogDisplay (noded, node);
}


/************************************************************************
 * Function:	Accept							*
 *									*
 * Description:	Accepts changes made to the currently displayed force.	*
 *		If the constraint is empty or the constraint or force	*
 *		does not exist then an error is reported.  Otherwise, a	*
 *		new node is created if a new/copy operation is in	*
 *		effect.  The node is then redisplayed to correct any	*
 *		invalid entries.					*
 ************************************************************************/

static void Accept (Widget w, XtPointer client_data, XtPointer call_data)
{
    double	      x;
    double	      y;
    Constraint	      constraint;
    force_t      f_dummy;
    Node	      active;
    NodeDialog	      noded;
    NodeDialogInfo    info;


    noded = (NodeDialog) client_data;


    /* Retrieve the constraint. */

    const char *name = GetTextString (noded -> c_name);
    constraint = SetSearch(*(noded->constraints), name);
    if (!constraint) {
        XBell (XtDisplay (noded -> shell), 0);
        SetTextString (noded -> c_name, "");
        SetFocus (noded -> c_name);
        return;
    }


    /* Create a new node as needed. */

    if (noded -> new_copy) {
        noded -> active.reset(new node_t(noded -> new_copy));
        noded->nodes->insert(noded->active);
    }

    active = noded -> active;
    active -> constraint = constraint;

    /* Retrieve the lumped mass. */

    active -> m = exptod (GetTextString (noded -> mass), NULL);


    /* Retrieve the force. */
    {
        f_dummy.name = GetTextString (noded -> f_name);
        active->force = SetSearch(*noded->forces, f_dummy.name);
    }

    /* Retrieve the values from the text entries. */

    x = exptod (GetTextString (noded -> loc_x), NULL);
    y = exptod (GetTextString (noded -> loc_y), NULL);

    info.moved = x != active -> x || y != active -> y;
    active -> x = x;
    active -> y = y;

    if (noded -> callback != NULL) {
	info.dialog  = noded;
	info.node    = active;
	info.deleted = False;
	info.proceed = True;
	noded -> callback (noded -> shell, noded -> closure, &info);
    }

    NodeDialogUpdate (noded, noded -> nodes, NULL, NULL);
}


/************************************************************************
 * Function:	Dismiss							*
 *									*
 * Description:	Pops down the dialog box.				*
 ************************************************************************/

static void Dismiss (Widget w, XtPointer client_data, XtPointer call_data)
{
    NodeDialog noded;


    noded = (NodeDialog) client_data;
    XtPopdown (noded -> shell);
}


/************************************************************************
 * Function:	Delete							*
 *									*
 * Description:	Deletes the active node if a new/copy operation is not	*
 *		in effect.  The dialog is then updated.			*
 ************************************************************************/

static void Delete (Widget w, XtPointer client_data, XtPointer call_data)
{
    Node	   node;
    NodeDialog	   noded;
    NodeDialogInfo info;


    noded = (NodeDialog) client_data;

    if (!noded -> new_copy) {
	if (noded -> callback) {
	    w = noded -> shell;
	    info.dialog  = noded;
	    info.node    = noded -> active;
	    info.deleted = True;
	    info.proceed = True;
	    noded -> callback (w, noded -> closure, &info);
	    if (info.proceed == False)
		return;
	}

    node = SetPredecessor(*noded->nodes, noded->active);
    if (!node)
        node = SetSuccessor(*noded->nodes, noded->active);
    
    noded->nodes->erase(noded->active);
	noded -> active = node;
    }

    NodeDialogUpdate (noded, noded -> nodes, NULL, NULL);
}


/************************************************************************
 * Function:	Copy							*
 *									*
 * Description:	Selects a new node number, clears the displacement	*
 *		fields, and sets the flag indicating that a new/copy	*
 *		operation is in effect.					*
 ************************************************************************/

static void Copy (Widget w, XtPointer client_data, XtPointer call_data)
{
    Node       node;
    NodeDialog noded;


    noded = (NodeDialog) client_data;

    node = SetMaximum(*(noded->nodes));
    noded -> new_copy = node != NULL ? node -> number + 1 : 1;

    SetNumber (noded, noded -> new_copy);

    SetTextString (noded -> trans_x, "");
    SetTextString (noded -> trans_y, "");
    SetTextString (noded -> trans_z, "");
    SetTextString (noded -> rot_x, "");
    SetTextString (noded -> rot_y, "");
    SetTextString (noded -> rot_z, "");
}


/************************************************************************
 * Function:	New							*
 *									*
 * Description:	Selects a new node number, clears all entries in the	*
 *		node dialog, and sets the flag indicating that a new/	*
 *		copy operation is in effect.				*
 ************************************************************************/

static void New (Widget w, XtPointer client_data, XtPointer call_data)
{
    NodeDialog noded;


    noded = (NodeDialog) client_data;

    Copy (NULL, client_data, NULL);
    SetTextString (noded -> mass, "");
    SetTextString (noded -> f_name, "");
    SetTextString (noded -> c_name, "");
    SetTextString (noded -> loc_x, "");
    SetTextString (noded -> loc_y, "");
    SetTextString (noded -> loc_z, "");
}


/************************************************************************
 * Function:	NodeDialogCreate					*
 *									*
 * Description:	Creates a new node dialog.  You would never want to	*
 *		have more than one of these but the interface is kept	*
 *		consistent with those of the other dialogs.		*
 ************************************************************************/

NodeDialog NodeDialogCreate (Widget parent, String name, String title, XtCallbackProc callback, XtPointer closure)
{
    Cardinal		i;
    Arg			args [1];
    Widget		group [16];
    Window		window;
    NodeDialog		noded;
    Dimension		width;
    Position		x;
    static XtAppContext	app_context = NULL;
    static XtActionsRec	actions [ ] = {{"NodeDialogAction", Action}};


    /* Perform one time initialization. */

    if (app_context == NULL) {
	app_context = XtWidgetToApplicationContext (parent);
	XtAppAddActions (app_context, actions, XtNumber (actions));
	AddAutoRepeatAction (app_context);

	layout_args [0].value = StringToLayout (parent, layout_string);

	text_translations = XtParseTranslationTable (text_table);
	button_translations = XtParseTranslationTable (button_table);
	command_translations = XtParseTranslationTable (command_table);
	repeater_translations = XtParseTranslationTable (repeater_table);
	help_translations = XtParseTranslationTable (help_table);

	window = RootWindowOfScreen (XtScreen (parent));

	up_bitmap = XCreateBitmapFromData (XtDisplay (parent), window,
		    up_bits, up_width, up_height);

	down_bitmap = XCreateBitmapFromData (XtDisplay (parent), window,
		      down_bits, down_width, down_height);
    }


    /* Create the node dialog and its widgets. */

    XtSetArg (shell_args [0], XtNtitle, title);
    XtSetArg (shell_args [1], XtNiconName, title);

    noded = new struct node_dialog;
    noded -> callback = callback;
    noded -> closure = closure;
    noded -> active.reset();
    noded -> new_forces = True;
    noded -> new_constraints = True;

    noded -> shell    = XtCreatePopupShell (name,
			topLevelShellWidgetClass, parent,
			shell_args, XtNumber (shell_args));

    noded -> layout   = XtCreateManagedWidget ("layout",
			layoutWidgetClass, noded -> shell,
			layout_args, XtNumber (layout_args));

    noded -> number   = XtCreateManagedWidget ("number",
			labelWidgetClass, noded -> layout,
			number_args, XtNumber (number_args));

    repeater_args [0].value = (XtArgVal) up_bitmap;

    noded -> up       = XtCreateManagedWidget ("up",
			repeaterWidgetClass, noded -> layout,
			repeater_args, XtNumber (repeater_args));

    repeater_args [0].value = (XtArgVal) down_bitmap;

    noded -> down     = XtCreateManagedWidget ("down",
			repeaterWidgetClass, noded -> layout,
			repeater_args, XtNumber (repeater_args));

    noded -> f_name   = XtCreateManagedWidget ("forceName",
			asciiTextWidgetClass, noded -> layout,
			edit_text_args, XtNumber (edit_text_args));

    XtSetArg (button_args [0], XtNlabel,    "Force:");
    XtSetArg (button_args [1], XtNmenuName, "forceMenu");

    noded -> f_button = XtCreateManagedWidget ("forceButton",
			menuButtonWidgetClass, noded -> layout,
			button_args, XtNumber (button_args));

    noded -> f_menu   = XtCreatePopupShell ("forceMenu",
			simpleMenuWidgetClass, noded -> layout,
			NULL, 0);

    noded -> c_name   = XtCreateManagedWidget ("constraintName",
			asciiTextWidgetClass, noded -> layout,
			edit_text_args, XtNumber (edit_text_args));

    XtSetArg (button_args [0], XtNlabel,    "Constraint:");
    XtSetArg (button_args [1], XtNmenuName, "constraintMenu");

    noded -> c_button = XtCreateManagedWidget ("constraintButton",
			menuButtonWidgetClass, noded -> layout,
			button_args, XtNumber (button_args));

    noded -> c_menu   = XtCreatePopupShell ("constraintMenu",
			simpleMenuWidgetClass, noded -> layout,
			NULL, 0);

    noded -> mass     = XtCreateManagedWidget ("mass",
			asciiTextWidgetClass, noded -> layout,
			edit_text_args, XtNumber (edit_text_args));

    noded -> loc_x    = XtCreateManagedWidget ("locationX",
			asciiTextWidgetClass, noded -> layout,
			edit_text_args, XtNumber (edit_text_args));

    noded -> loc_y    = XtCreateManagedWidget ("locationY",
			asciiTextWidgetClass, noded -> layout,
			edit_text_args, XtNumber (edit_text_args));

    noded -> loc_z    = XtCreateManagedWidget ("locationZ",
			asciiTextWidgetClass, noded -> layout,
			edit_text_args, XtNumber (edit_text_args));

    noded -> trans_x  = XtCreateManagedWidget ("translationX",
			asciiTextWidgetClass, noded -> layout,
			read_text_args, XtNumber (read_text_args));

    noded -> trans_y  = XtCreateManagedWidget ("translationY",
			asciiTextWidgetClass, noded -> layout,
			read_text_args, XtNumber (read_text_args));

    noded -> trans_z  = XtCreateManagedWidget ("translationZ",
			asciiTextWidgetClass, noded -> layout,
			read_text_args, XtNumber (read_text_args));

    noded -> rot_x    = XtCreateManagedWidget ("rotationX",
			asciiTextWidgetClass, noded -> layout,
			read_text_args, XtNumber (read_text_args));

    noded -> rot_y    = XtCreateManagedWidget ("rotationY",
			asciiTextWidgetClass, noded -> layout,
			read_text_args, XtNumber (read_text_args));

    noded -> rot_z    = XtCreateManagedWidget ("rotationZ",
			asciiTextWidgetClass, noded -> layout,
			read_text_args, XtNumber (read_text_args));

    noded -> accept   = XtCreateManagedWidget ("accept",
			commandWidgetClass, noded -> layout,
			NULL, 0);

    noded -> dismiss  = XtCreateManagedWidget ("dismiss",
			commandWidgetClass, noded -> layout,
			NULL, 0);

    noded -> nuke   = XtCreateManagedWidget ("delete",
			commandWidgetClass, noded -> layout,
			NULL, 0);

    noded -> nu      = XtCreateManagedWidget ("new",
			commandWidgetClass, noded -> layout,
			NULL, 0);

    noded -> copy     = XtCreateManagedWidget ("copy",
			commandWidgetClass, noded -> layout,
			NULL, 0);

    noded -> help     = CreateHelpButton (noded -> layout, "help");

    for (i = 0; i < XtNumber (labels); i ++) {
	label_args [0].value = (XtArgVal) labels [i];
	XtCreateManagedWidget (names [i], labelWidgetClass,
		noded -> layout, label_args, XtNumber (label_args));
    }


    XtCreateManagedWidget ("separator1", coreWidgetClass,
			noded -> layout, core_args, XtNumber (core_args));

    XtCreateManagedWidget ("separator2", coreWidgetClass,
			noded -> layout, core_args, XtNumber (core_args));

    XtCreateManagedWidget ("separator3", coreWidgetClass,
			noded -> layout, core_args, XtNumber (core_args));

    XtCreateManagedWidget ("- none -", smeBSBObjectClass,
			noded -> f_menu, NULL, 0);

    XtCreateManagedWidget ("- none -", smeBSBObjectClass,
			noded -> c_menu, NULL, 0);


    AddPostMenuActions (noded -> f_menu);
    AddPostMenuActions (noded -> c_menu);


    /* Create a tab group for the node dialog. */

    i = 0;
    group [i++] = noded -> up;
    group [i++] = noded -> down;
    group [i++] = noded -> mass;
    group [i++] = noded -> f_button;
    group [i++] = noded -> f_name;
    group [i++] = noded -> c_button;
    group [i++] = noded -> c_name;
    group [i++] = noded -> loc_x;
    group [i++] = noded -> loc_y;
    group [i++] = noded -> loc_z;
    group [i++] = noded -> help;
    group [i++] = noded -> accept;
    group [i++] = noded -> dismiss;
    group [i++] = noded -> nuke;
    group [i++] = noded -> nu;
    group [i++] = noded -> copy;

    XtGetValues (noded -> layout, color_args, XtNumber (color_args));
    CreateTabGroup (noded -> shell, group, XtNumber (group), highlight, True);
    XtRealizeWidget (noded -> shell);
    SetFocus (noded -> up);

    XtSetArg (args [0], XtNwidth, &width);
    XtGetValues (noded -> layout, args, 1);
    XtSetArg (args [0], XtNx, &x);
    XtGetValues (noded -> help, args, 1);
    UpdateHelpMessage (noded -> help, help_message, width - 2 * x);


    /* Add the translations to each widget. */

    AddDeleteWindowProtocol (noded -> shell, "NodeDialogAction()");

    XtOverrideTranslations (noded -> f_name,   text_translations);
    XtOverrideTranslations (noded -> c_name,   text_translations);
    XtOverrideTranslations (noded -> mass,     text_translations);
    XtOverrideTranslations (noded -> loc_x,    text_translations);
    XtOverrideTranslations (noded -> loc_y,    text_translations);
    XtOverrideTranslations (noded -> loc_z,    text_translations);
    XtOverrideTranslations (noded -> f_button, button_translations);
    XtOverrideTranslations (noded -> c_button, button_translations);
    XtOverrideTranslations (noded -> accept,   command_translations);
    XtOverrideTranslations (noded -> dismiss,  command_translations);
    XtOverrideTranslations (noded -> nu,      command_translations);
    XtOverrideTranslations (noded -> copy,     command_translations);
    XtOverrideTranslations (noded -> up,       repeater_translations);
    XtOverrideTranslations (noded -> down,     repeater_translations);
    XtOverrideTranslations (noded -> help,     help_translations);


    /* Add the necessary callbacks. */

    XtAddCallback (noded -> up,      XtNcallback, Up,      (XtPointer) noded);
    XtAddCallback (noded -> down,    XtNcallback, Down,    (XtPointer) noded);
    XtAddCallback (noded -> accept,  XtNcallback, Accept,  (XtPointer) noded);
    XtAddCallback (noded -> dismiss, XtNcallback, Dismiss, (XtPointer) noded);
    XtAddCallback (noded -> nuke,  XtNcallback, Delete,  (XtPointer) noded);
    XtAddCallback (noded -> nu,     XtNcallback, New,     (XtPointer) noded);
    XtAddCallback (noded -> copy,    XtNcallback, Copy,    (XtPointer) noded);

    XtAddCallback (noded -> f_menu, XtNpopupCallback,
		   UpdateForceMenu, (XtPointer) noded);

    XtAddCallback (noded -> f_menu, XtNpopdownCallback,
		   UpdateForceName, (XtPointer) noded);

    XtAddCallback (noded -> c_menu, XtNpopupCallback,
		   UpdateConstraintMenu, (XtPointer) noded);

    XtAddCallback (noded -> c_menu, XtNpopdownCallback,
		   UpdateConstraintName, (XtPointer) noded);

    return noded;
}


/************************************************************************
 * Function:	NodeDialogPopup						*
 *									*
 * Description:	Pops up the specified node dialog.			*
 ************************************************************************/

void NodeDialogPopup (NodeDialog noded)
{
    XtPopup (noded -> shell, XtGrabNone);
}


/************************************************************************
 * Function:	NodeDialogUpdate					*
 *									*
 * Description:	Updates the specified node dialog with the specified	*
 *		trees.							*
 ************************************************************************/

void NodeDialogUpdate (NodeDialog noded, Problem::NodeSet *nodes,
                       Problem::ForceSet *forces, Problem::ConstraintSet *constraints)
{
    /* Remember to update the menus if necessary. */

    if (forces != NULL) {
        noded -> forces = forces;
        noded -> new_forces = True;
    }

    if (constraints != NULL) {
        noded -> constraints = constraints;
        noded -> new_constraints = True;
    }


    /* Determine a new active node if necessary. */

    if (nodes == NULL && noded -> active == NULL)
        noded->active = SetMinimum(*(noded->nodes));

    if (nodes != NULL && (noded -> active == NULL || noded -> nodes != nodes))
        noded->active = SetMinimum(*nodes);

    if (nodes != NULL) {
	noded -> nodes = nodes;
	if (noded -> active != NULL)
	    NodeDialogDisplay (noded, noded -> active);
	else
	    New (NULL, (XtPointer) noded, NULL);
    }
}


/************************************************************************
 * Function:	NodeDialogActive					*
 *									*
 * Description:	Returns the currently displayed (active) node.		*
 ************************************************************************/

Node NodeDialogActive (NodeDialog noded)
{
    return noded -> active;
}


/************************************************************************
 * Function:	NodeDialogDisplay					*
 *									*
 * Description:	Displays the specified node.				*
 ************************************************************************/

void NodeDialogDisplay (NodeDialog noded, Node node)
{
    char buffer [32];
    Node active;


    /* Check if the node exists. */

    noded -> active = node;

    if (noded -> active == NULL)
	return;

    active = noded -> active;
    noded -> new_copy = 0;


    /* Update all of the text entries. */

    SetNumber (noded, active -> number);

    sprintf (buffer, "%g", active -> m);
    SetTextString (noded -> mass, buffer);

    sprintf (buffer, "%g", active -> x);
    SetTextString (noded -> loc_x, buffer);

    sprintf (buffer, "%g", active -> y);
    SetTextString (noded -> loc_y, buffer);

    sprintf (buffer, "%g", active -> z);
    SetTextString (noded -> loc_z, buffer);

    sprintf (buffer, "%g", active -> dx [Tx]);
    SetTextString (noded -> trans_x, buffer);

    sprintf (buffer, "%g", active -> dx [Ty]);
    SetTextString (noded -> trans_y, buffer);

    sprintf (buffer, "%g", active -> dx [Tz]);
    SetTextString (noded -> trans_z, buffer);

    sprintf (buffer, "%g", active -> dx [Rx]);
    SetTextString (noded -> rot_x, buffer);

    sprintf (buffer, "%g", active -> dx [Ry]);
    SetTextString (noded -> rot_y, buffer);

    sprintf (buffer, "%g", active -> dx [Rz]);
    SetTextString (noded -> rot_z, buffer);

    if (active -> force != NULL)
        SetTextString (noded -> f_name, active -> force -> name.c_str());
    else
        SetTextString (noded -> f_name, "");

    if (active -> constraint != NULL)
        SetTextString (noded -> c_name, active -> constraint -> name.c_str());
    else
        SetTextString (noded -> c_name, "");
}
