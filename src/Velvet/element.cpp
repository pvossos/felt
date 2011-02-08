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
 * File:	element.c						*
 *									*
 * Description:	This file contains the public and private function and	*
 *		definitions for the element dialog box.			*
 ************************************************************************/

# include <algorithm>
# include <stdio.h>
# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <X11/Shell.h>
# include <X11/Xaw/AsciiText.h>
# include <X11/Xaw/Command.h>
# include <X11/Xaw/MenuButton.h>
# include <X11/Xaw/Repeater.h>
# include <X11/Xaw/SimpleMenu.h>
# include <X11/Xaw/SmeBSB.h>
# include <X11/Xaw/Viewport.h>
# include "Layout.h"
# include "Element.h"
# include "TabGroup.h"
# include "util.h"
# include "objects.h"
# include "post.h"
# include "problem.h"
# include "error.h"
# include "definition.h"

# ifndef X_NOT_STDC_ENV
# include <stdlib.h>
# else
extern int atoi ( );
# endif

struct element_dialog {
    Widget	   shell;	/* topLevelShell  <specified>		*/
    Widget	   layout;	/*	Layout  layout			*/
    Widget	   number;	/*	     Label  number		*/
    Widget	   type;	/*	     Label  type		*/
    Widget	   up;		/*	     Repeater  up		*/
    Widget	   down;	/*	     Repeater  down		*/
    Widget	   left;	/*	     Command  left		*/
    Widget	   right;	/*	     Command  right		*/
    Widget	   node [6];	/*	     AsciiText  node[1-6]	*/
    Widget	   m_name;	/*	     AsciiText  materialName	*/
    Widget	   m_button;	/*	     MenuButton  materialButton	*/
    Widget	   m_menu;	/*	     SimpleMenu  materialMenu	*/
    Widget	   l_name [3];	/*	     AsciiText  load[1-3]Name	*/
    Widget	   l_button [3];/*	     MenuButton  load[1-3Button	*/
    Widget	   l_menu;	/*	     SimpleMenu  loadMenu	*/
    Widget	   viewport;	/*	     Viewport  viewport		*/
    Widget	   stresses;	/*		  Label  stresses	*/
    Widget	   help;	/*	     MenuButton  help		*/
    Widget	   accept;	/*	     Command  accept		*/
    Widget	   dismiss;	/*	     Command  dismiss		*/
    Widget	   nuke;	/*	     Command  delete		*/
    Widget	   nu;		/*	     Command  new		*/
    Widget	   copy;	/*	     Command  copy		*/
    XtCallbackProc callback;
    XtPointer	   closure;
    unsigned	   offset;
    unsigned	   new_copy;
    Boolean	   new_materials;
    Boolean	   new_loads;
    Definition	   definition;
    Element	   active;
    unsigned	   node_values [32];
    Problem::ElementSet	 *elements;
    Problem::MaterialSet *materials;
    Tree           loads;
    Tree           nodes;
};

static String labels [ ] = {
    "Number:", "Type:", "Nodes:", "Stresses"
};

static String names [ ] = {
    "numberLabel", "typeLabel", "nodes", "stressLabel"
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

#define left_width 12
#define left_height 12
static char left_bits[] = {
   0x00, 0x00, 0x80, 0x01, 0xc0, 0x01, 0xe0, 0x01, 0xf0, 0x01, 0xf8, 0x01,
   0xf8, 0x01, 0xf0, 0x01, 0xe0, 0x01, 0xc0, 0x01, 0x80, 0x01, 0x00, 0x00};

#define right_width 12
#define right_height 12
static char right_bits[] = {
   0x00, 0x00, 0x18, 0x00, 0x38, 0x00, 0x78, 0x00, 0xf8, 0x00, 0xf8, 0x01,
   0xf8, 0x01, 0xf8, 0x00, 0x78, 0x00, 0x38, 0x00, 0x18, 0x00, 0x00, 0x00};

static Pixmap up_bitmap;
static Pixmap down_bitmap;
static Pixmap left_bitmap;
static Pixmap right_bitmap;


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
	40 \
	typeLabel \
	4 \
	type \
	4 <+inf -100%> \
    } \
    4 \
    separator1 <+inf -100% *> \
    4 \
    horizontal { \
	4 \
	left \
	4 \
	right \
	4 \
	nodes \
	4 \
	node1 \
	node2 \
	node3 \
	node4 \
	node5 \
	node6 \
	4 <+inf -100%> \
    } \
    4 \
    separator2 <+inf -100% *> \
    4 \
    vertical { \
	horizontal { \
	    4 \
	    materialButton 4 materialName <+inf -100% *> 4 \
	    load2Button 4 load2Name <+inf -100% *> 4 \
	} \
	4 \
	horizontal { \
	    4 \
	    load1Button 4 load1Name <+inf -100% *> 4 \
	    load3Button 4 load3Name <+inf -100% *> 4 \
	} \
    } \
    4 \
    separator3 <+inf -100% *> \
    4 \
    horizontal { \
	4 \
	stressLabel \
	4 <+inf -100%> \
    } \
    4 \
    horizontal { \
	4 \
	viewport <+inf -100% * +inf -100%> \
	4 \
    } \
    4 \
    separator4 <+inf -100% *> \
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

static Arg node_args [ ] = {
    {XtNeditType,    (XtArgVal) XawtextEdit},
    {XtNborderWidth, (XtArgVal) 0},
    {XtNpieceSize,   (XtArgVal) 32},
    {XtNcursorName,  (XtArgVal) "left_ptr"},
    {XtNwidth,       (XtArgVal) 40},
};

static Arg stress_args [ ] = {
    {XtNresize,      (XtArgVal) True},
    {XtNjustify,     (XtArgVal) XtJustifyLeft},
    {XtNborderWidth, (XtArgVal) 0},
};

static Arg viewport_args [ ] = {
    {XtNallowVert,   (XtArgVal) True},
    {XtNallowHoriz,  (XtArgVal) True},
    {XtNuseBottom,   (XtArgVal) True},
    {XtNuseRight,    (XtArgVal) True},
    {XtNforceBars,   (XtArgVal) False},
    {XtNborderWidth, (XtArgVal) 0},
};

static Arg button_args [ ] = {
    {XtNlabel,       (XtArgVal) ""},
    {XtNmenuName,    (XtArgVal) ""},
    {XtNborderWidth, (XtArgVal) 0},
    {XtNresize,      (XtArgVal) False},
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

static Arg bitmap_args [ ] = {
    {XtNbitmap, (XtArgVal) NULL},
};


/* Translation tables */

static String text_table =
"<Key>Return: ElementDialogAction(accept)\n\
 <Key>Escape: ElementDialogAction(dismiss)\n\
 Ctrl<Key>d:  ElementDialogAction(delete)\n\
 Ctrl<Key>c:  ElementDialogAction(copy)\n\
 Ctrl<Key>n:  ElementDialogAction(new)\n\
 Ctrl<Key>h:  ElementDialogAction(help)\n\
 <Btn1Down>:  SetFocus() select-start()";

static XtTranslations text_translations;


static String command_table =
"<Key>Return:  ElementDialogAction(accept)\n\
 <Key>Escape:  ElementDialogAction(dismiss)\n\
 Ctrl<Key>d:   ElementDialogAction(delete)\n\
 Ctrl<Key>c:   ElementDialogAction(copy)\n\
 Ctrl<Key>n:   ElementDialogAction(new)\n\
 Ctrl<Key>h:   ElementDialogAction(help)\n\
 <Key>space:   AutoRepeat(off) set()\n\
 <KeyUp>space: AutoRepeat(saved) notify() unset()";

static XtTranslations command_translations;


static String button_table =
"<Key>Return: ElementDialogAction(accept)\n\
 <Key>Escape: ElementDialogAction(dismiss)\n\
 Ctrl<Key>d:  ElementDialogAction(delete)\n\
 Ctrl<Key>c:  ElementDialogAction(copy)\n\
 Ctrl<Key>n:  ElementDialogAction(new)\n\
 Ctrl<Key>h:  ElementDialogAction(help)\n\
 <BtnDown>:   ElementDialogMenu() PostMenu()\n\
 <Key>space:  ElementDialogMenu() PostMenu()";

static XtTranslations button_translations;


static String repeater_table =
"<Key>Return:  ElementDialogAction(accept)\n\
 <Key>Escape:  ElementDialogAction(dismiss)\n\
 Ctrl<Key>d:   ElementDialogAction(delete)\n\
 Ctrl<Key>c:   ElementDialogAction(copy)\n\
 Ctrl<Key>n:   ElementDialogAction(new)\n\
 Ctrl<Key>h:   ElementDialogAction(help)\n\
 <Key>space:   AutoRepeat(off) set()\n\
 <KeyUp>space: AutoRepeat(saved) notify() unset()";

static XtTranslations repeater_translations;


static String viewport_table =
"<Key>Return: ElementDialogAction(accept)\n\
 <Key>Escape: ElementDialogAction(dismiss)\n\
 Ctrl<Key>d:  ElementDialogAction(delete)\n\
 Ctrl<Key>c:  ElementDialogAction(copy)\n\
 Ctrl<Key>n:  ElementDialogAction(new)\n\
 Ctrl<Key>h:  ElementDialogAction(help)\n\
 <Btn1Down>:  SetFocus()";

static XtTranslations viewport_translations;


static String help_table =
"<Key>Return: ElementDialogAction(accept)\n\
 <Key>Escape: ElementDialogAction(dismiss)\n\
 Ctrl<Key>d:  ElementDialogAction(delete)\n\
 Ctrl<Key>c:  ElementDialogAction(copy)\n\
 Ctrl<Key>n:  ElementDialogAction(new)\n\
 Ctrl<Key>h:  ElementDialogAction(help)\n\
 <Key>space:  PostMenu()";

static XtTranslations help_translations;


/* Help message. */

static String help_message = "\
The element form is used to define, edit and delete elements.  Elements can \
also be selected by using the drawing area.  Use the arrows to change \
the current element.  The menus and fields above define the behavior of the \
current element.  Use the 'Accept' button to register your changes.  'Delete' \
erases the current element.  'New' empties all fields creating a new element.  \
'Copy' creates a new element without changing any fields.";


/* Menu creation variables */

static Cardinal		child_number;
static WidgetList	children;
static Dimension	max_width;
static XtWidgetGeometry	preferred;
static Widget		menu_button;


/************************************************************************
 * Function:	SetLoadEntry						*
 *									*
 * Description:	Sets the label of the next menu entry to the name of	*
 *		the specified load.					*
 ************************************************************************/

static int SetLoadEntry (Item item)
{
    SetLabelString (children [child_number], ((Distributed) item) -> name.c_str());

    XtQueryGeometry (children [child_number ++], NULL, &preferred);
    if (preferred.width > max_width)
	max_width = preferred.width;

    return 0;
}


/************************************************************************
 * Function:	UpdateLoadMenu						*
 *									*
 * Description:	Updates the load menu on popup if necessary to the list	*
 *		of current loads.					*
 ************************************************************************/

static void UpdateLoadMenu (Widget w, XtPointer client_data, XtPointer call_data)
{
    Arg		  args [2];
    char	  buffer [32];
    Cardinal	  overflow;
    Cardinal	  num_loads;
    Cardinal	  i;
    Cardinal	  num_children;
    ElementDialog eltd;


    eltd = (ElementDialog) client_data;

    if (eltd -> new_loads == False)
	return;

    XtSetArg (args [0], XtNchildren,    &children);
    XtSetArg (args [1], XtNnumChildren, &num_children);
    XtGetValues (w, args, 2);

    num_loads = TreeSize (eltd -> loads) + 1;

    for (i = num_children; i < num_loads; i ++) {
	sprintf (buffer, "load%d", i);
	XtCreateManagedWidget (XtNewString (buffer), smeBSBObjectClass,
			       eltd -> l_menu, NULL, 0);
    }

    if (num_children > num_loads) {
	overflow = num_children - num_loads;
	XtUnmanageChildren (children + num_loads, overflow);
    }

    XtSetArg (args [0], XtNchildren,    &children);
    XtSetArg (args [1], XtNnumChildren, &num_children);
    XtGetValues (w, args, 2);

    XtQueryGeometry (children [0], NULL, &preferred);
    max_width = preferred.width;

    child_number = 1;
    TreeSetIterator (eltd -> loads, SetLoadEntry);
    TreeIterate (eltd -> loads);
    eltd -> new_loads = False;

    XtSetArg (args [0], XtNwidth, max_width);
    XtSetValues (w, args, 1);
}


/************************************************************************
 * Function:	UpdateLoadName						*
 *									*
 * Description:	Updates the load name on popdown if necessary.		*
 ************************************************************************/

static void UpdateLoadName (Widget w, XtPointer client_data, XtPointer call_data)
{
    Cardinal	  i;
    Widget	  name;
    ElementDialog eltd;


    if (!(w = XawSimpleMenuGetActiveEntry (w)))
	return;

    name = NULL;
    eltd = (ElementDialog) client_data;

    for (i = 0; i < 3; i ++)
	if (menu_button == eltd -> l_button [i]) {
	    name = eltd -> l_name [i];
	    break;
	}

    if (!strcmp (XtName (w), "- none -"))
	SetTextString (name, "");
    else
	SetTextString (name, GetLabelString (w));
}


/************************************************************************
 * Function:	SetMaterialEntry					*
 *									*
 * Description:	Sets the label of the next menu entry to the name of	*
 *		the specified material.					*
 ************************************************************************/

static int SetMaterialEntry (Material item)
{
    SetLabelString (children [child_number], ((Material) item) -> name.c_str());

    XtQueryGeometry (children [child_number ++], NULL, &preferred);
    if (preferred.width > max_width)
	max_width = preferred.width;

    return 0;
}


/************************************************************************
 * Function:	UpdateMaterialMenu					*
 *									*
 * Description:	Updates the material menu on popup if necessary to the	*
 *		list of current materials.				*
 ************************************************************************/

static void UpdateMaterialMenu (Widget w, XtPointer client_data, XtPointer call_data)
{
    Arg		  args [2];
    char	  buffer [32];
    Cardinal	  overflow;
    Cardinal	  num_materials;
    Cardinal	  i;
    Cardinal	  num_children;
    ElementDialog eltd;


    eltd = (ElementDialog) client_data;

    if (eltd -> new_materials == False)
	return;

    XtSetArg (args [0], XtNchildren,    &children);
    XtSetArg (args [1], XtNnumChildren, &num_children);
    XtGetValues (w, args, 2);

    num_materials = eltd->materials->size();

    if (num_materials <= 0) {
	num_materials ++;
	SetLabelString (children [0], "- no materials defined -");
	XtQueryGeometry (children [0], NULL, &preferred);
	max_width = preferred.width;
    } else
	max_width = 0;

    for (i = num_children; i < num_materials; i ++) {
	sprintf (buffer, "material%d", i);
	XtCreateManagedWidget (XtNewString (buffer), smeBSBObjectClass,
			       eltd -> m_menu, NULL, 0);
    }

    if (num_children > num_materials) {
	overflow = num_children - num_materials;
	XtUnmanageChildren (children + num_materials, overflow);
    }

    XtSetArg (args [0], XtNchildren,    &children);
    XtSetArg (args [1], XtNnumChildren, &num_children);
    XtGetValues (w, args, 2);

    child_number = 0;
    std::for_each(eltd->materials->begin(), eltd->materials->end(), SetMaterialEntry);

    XtSetArg (args [0], XtNwidth, max_width);
    XtSetValues (w, args, 1);
}


/************************************************************************
 * Function:	UpdateMaterialName					*
 *									*
 * Description:	Updates the material name on popdown if necessary.	*
 ************************************************************************/

static void UpdateMaterialName (Widget w, XtPointer client_data, XtPointer call_data)
{
    String	  label;
    ElementDialog eltd;


    if (!(w = XawSimpleMenuGetActiveEntry (w)))
	return;

    eltd = (ElementDialog) client_data;
    label = GetLabelString (w);

    if (!strcmp (label, "- no materials defined -"))
	SetTextString (eltd -> m_name, "");
    else
	SetTextString (eltd -> m_name, label);
}


/************************************************************************
 * Function:	SetNumber						*
 *									*
 * Description:	Sets the label of the number widget.			*
 ************************************************************************/

static void SetNumber (ElementDialog eltd, unsigned int number)
{
    char buffer [10];


    sprintf (buffer, "%u", number);
    SetLabelString (eltd -> number, buffer);
}


/************************************************************************
 * Function:	SetType							*
 *									*
 * Description:	Sets the label of the type widget.			*
 ************************************************************************/

static void SetType (ElementDialog eltd, Definition defn)
{
    Cardinal i;
    unsigned numnodes;


    SetLabelString (eltd -> type, defn -> name);

    eltd -> offset = 0;
    eltd -> definition = defn;
    numnodes = eltd -> definition -> numnodes;

    if (numnodes >= 6)
	numnodes = 6;

    for (i = 0; i < numnodes; i ++)
	if (!XtIsSensitive (eltd -> node [i]))
	    XtSetSensitive (eltd -> node [i], True);

    for (i = numnodes; i < 6; i ++) {
	SetTextString (eltd -> node [i], "");
	if (HasFocus (eltd -> node [i]))
	    SetFocus (eltd -> node [numnodes - 1]);
	if (XtIsSensitive (eltd -> node [i]))
	    XtSetSensitive (eltd -> node [i], False);
    }
}


/************************************************************************
 * Function:	DisplayNodes						*
 *									*
 * Description:	Displays the node array.				*
 ************************************************************************/

static void DisplayNodes (ElementDialog eltd)
{
    Cardinal i;
    Cardinal j;
    unsigned numnodes;
    char     buffer [32];


    numnodes = eltd -> definition -> numnodes;

    if (numnodes > 6)
	numnodes = 6;

    for (i = 0, j = eltd -> offset; i < numnodes; i ++, j ++)
	if (eltd -> node_values [j]) {
	    sprintf (buffer, "%u", eltd -> node_values [j]);
	    SetTextString (eltd -> node [i], buffer);
	} else
	    SetTextString (eltd -> node [i], "");
}


/************************************************************************
 * Function:	RetrieveNodes						*
 *									*
 * Description:	Retrieves the displayed nodes.				*
 ************************************************************************/

static void RetrieveNodes (ElementDialog eltd)
{
    Cardinal i;
    Cardinal j;
    unsigned numnodes;

    numnodes = eltd -> definition -> numnodes;

    if (numnodes > 6)
	numnodes = 6;

    for (i = 0, j = eltd -> offset; i < numnodes; i ++, j ++)
	eltd -> node_values [j] = atoi (GetTextString (eltd -> node [i]));
}


/************************************************************************
 * Function:	Menu							*
 *									*
 * Description:	Records the menu button which activated the menu.	*
 ************************************************************************/

static void Menu (Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    menu_button = w;
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
 * Description:	Displays the next highest numbered element.		*
 ************************************************************************/

static void Up (Widget w, XtPointer client_data, XtPointer call_data)
{
    Element	  element;
    ElementDialog eltd;


    eltd = (ElementDialog) client_data;

    if (eltd -> active == NULL)
	return;

    Problem::ElementSet::iterator it = problem.element_set.upper_bound(eltd->active);
    element = it != problem.element_set.end() ? *it : NULL;

    if (element != NULL)
	ElementDialogDisplay (eltd, element);
}


/************************************************************************
 * Function:	Down							*
 *									*
 * Description:	Displays the next lowest numbered element.		*
 ************************************************************************/

static void Down (Widget w, XtPointer client_data, XtPointer call_data)
{
    Element	  element;
    ElementDialog eltd;


    eltd = (ElementDialog) client_data;

    if (eltd -> active == NULL)
	return;

    Problem::ElementSet::iterator it = problem.element_set.lower_bound(eltd->active);
    element = it != problem.element_set.begin() ? *(--it) : NULL;

    if (element != NULL)
	ElementDialogDisplay (eltd, element);
}


/************************************************************************
 * Function:	Left							*
 *									*
 * Description:	Shifts the list of displayed nodes to the right.	*
 ************************************************************************/

static void Left (Widget w, XtPointer client_data, XtPointer call_data)
{
    unsigned	  numnodes;
    ElementDialog eltd;


    eltd = (ElementDialog) client_data;

    numnodes = eltd -> definition -> numnodes;

    if (numnodes > 6 && eltd -> offset > 0) {
	RetrieveNodes (eltd);
	eltd -> offset --;
	DisplayNodes (eltd);
    }
}


/************************************************************************
 * Function:	Right							*
 *									*
 * Description:	Shifts the list of displayed nodes to the left.		*
 ************************************************************************/

static void Right (Widget w, XtPointer client_data, XtPointer call_data)
{
    unsigned	  numnodes;
    ElementDialog eltd;


    eltd = (ElementDialog) client_data;

    numnodes = eltd -> definition -> numnodes;

    if (numnodes > 6 && eltd -> offset < numnodes - 6) {
	RetrieveNodes (eltd);
	eltd -> offset ++;
	DisplayNodes (eltd);
    }
}


/************************************************************************
 * Function:	Accept							*
 *									*
 * Description:	Accepts changes made to the currently displayed		*
 *		element.  If the material is empty or the material or	*
 *		or a load does not exist then an error is reported.	*
 *		Otherwise, a new element is created if a new/copy	*
 *		operation is in effect.  The element is then		*
 *		redisplayed to correct any invalid entries.		*
 ************************************************************************/

static void Accept (Widget w, XtPointer client_data, XtPointer call_data)
{
    Cardinal	       i;
    unsigned	       numnodes;
    unsigned	       shapenodes;
    Material	       material;
    Distributed	       load;
    struct element     original;
    struct distributed l_dummy;
    struct material    m_dummy;
    struct node	       n_dummy;
    Element	       active;
    ElementDialog      eltd;
    ElementDialogInfo  info;
    Node	       node;
    Node	       nodes [32];
    Node	       original_nodes [32];


    eltd = (ElementDialog) client_data;


    /* Copy the original element if necessary. */

    if (eltd -> active != NULL) {
        original = *eltd -> active;
        original.node = eltd -> active -> node;
    }
    
    /* Retrieve the material. */

    m_dummy.name = GetTextString (eltd -> m_name);
    Problem::MaterialSet::iterator it = eltd->materials->find(&m_dummy);
    material = it != eltd->materials->end() ? *it : NULL;
    if (material == NULL) {
	XBell (XtDisplay (eltd -> shell), 0);
	SetTextString (eltd -> m_name, "");
	SetFocus (eltd -> m_name);
	return;
    }


    /* Retrieve the nodes. */

    numnodes = eltd -> definition -> numnodes;
    shapenodes = eltd -> definition -> shapenodes;

    RetrieveNodes (eltd);

    for (i = 0; i < numnodes; i ++) {
	n_dummy.number = eltd -> node_values [i];
	if (n_dummy.number == 0) {
	    if (i < shapenodes) {
		if (i < eltd -> offset) {
		    eltd -> offset = i;
		    DisplayNodes (eltd);
		} else if (i >= eltd -> offset + 6) {
		    eltd -> offset = i - 5;
		    DisplayNodes (eltd);
		}

		XBell (XtDisplay (eltd -> shell), 0);
		SetTextString (eltd -> node [i], "");
		SetFocus (eltd -> node [i]);
		return;
	    }
	    nodes [i] = NULL;

	} else {
	    node = (Node) TreeSearch (eltd -> nodes, &n_dummy);
	    if (node == NULL) {
		if (i < eltd -> offset) {
		    eltd -> offset = i;
		    DisplayNodes (eltd);
		} else if (i >= eltd -> offset + 6) {
		    eltd -> offset = i - 5;
		    DisplayNodes (eltd);
		}

		XBell (XtDisplay (eltd -> shell), 0);
		SetTextString (eltd -> node [i], "");
		SetFocus (eltd -> node [i]);
		return;
	    }
	    nodes [i] = node;
	}
    }


    /* Create a new element as needed. */

    if (eltd -> new_copy) {
        eltd -> active = CreateElement (eltd -> new_copy, eltd -> definition);
        problem.element_set.insert(eltd->active);
    }

    active = eltd -> active;
    active -> material = material;


    /* Retrieve the loads. */

    active -> numdistributed = 0;
    for (i = 0; i < 3; i ++) {
	l_dummy.name = GetTextString (eltd -> l_name [i]);
	load = (Distributed) TreeSearch (eltd -> loads, &l_dummy);
	if (load != NULL)
	    active -> distributed [++ active -> numdistributed] = load;
    }

    for (i = active -> numdistributed + 1; i <= 3; i ++)
	active -> distributed [i] = NULL;


    /* Copy the nodes. */

    for (i = 0; i < numnodes; i ++)
	active -> node [i + 1] = nodes [i];

    if (eltd -> callback != NULL) {
	info.dialog   = eltd;
	info.element  = active;
	info.deleted  = False;
	info.proceed  = True;
	info.original = &original;
	eltd -> callback (eltd -> shell, eltd -> closure, &info);
    }

    ElementDialogUpdate (eltd, eltd -> elements, NULL, NULL, NULL);
}


/************************************************************************
 * Function:	Dismiss							*
 *									*
 * Description:	Pops down the dialog box.				*
 ************************************************************************/

static void Dismiss (Widget w, XtPointer client_data, XtPointer call_data)
{
    ElementDialog eltd;


    eltd = (ElementDialog) client_data;
    XtPopdown (eltd -> shell);
}


/************************************************************************
 * Function:	Delete							*
 *									*
 * Description:	Deletes the active element if a new/copy operation is	*
 *		not in effect.  The dialog is then updated.		*
 ************************************************************************/

static void Delete (Widget w, XtPointer client_data, XtPointer call_data)
{
    Element	      active;
    Element	      element;
    ElementDialog     eltd;
    ElementDialogInfo info;


    eltd = (ElementDialog) client_data;

    if (!eltd -> new_copy) {
	if (eltd -> callback) {
	    w = eltd -> shell;
	    info.dialog  = eltd;
	    info.element = eltd -> active;
	    info.deleted = True;
	    info.proceed = True;
	    eltd -> callback (w, eltd -> closure, &info);
	    if (info.proceed == False)
		return;
	}

	active = eltd -> active;

    Problem::ElementSet::iterator it = eltd->elements->lower_bound(active);
    element = it != eltd->elements->begin() ? *(--it) : NULL;
    if (!element) {
        it = eltd->elements->upper_bound(active);
        element = it != eltd->elements->end() ? *it : NULL;
    }
    

    eltd->elements->erase(active);
	DestroyElement (active);
	eltd -> active = element;
    }

    ElementDialogUpdate (eltd, eltd -> elements, NULL, NULL, NULL);
}



/************************************************************************
 * Function:	Copy							*
 *									*
 * Description:	Selects a new element number, clears the stresses,	*
 *		and sets the flag indicating that a new/copy operation	*
 *		is in effect.						*
 ************************************************************************/

static void Copy (Widget w, XtPointer client_data, XtPointer call_data)
{
    Element	  element;
    ElementDialog eltd;


    eltd = (ElementDialog) client_data;

    element = eltd->elements->empty() ? NULL : *(eltd->elements->rbegin());
    eltd -> new_copy = element != NULL ? element -> number + 1 : 1;

    SetNumber (eltd, eltd -> new_copy);
    SetLabelString (eltd -> stresses, "");
}


/************************************************************************
 * Function:	New							*
 *									*
 * Description:	Selects a new element number, clears all entries in the	*
 *		element dialog, and sets the flag indicating that a	*
 *		new/copy operation is in effect.			*
 ************************************************************************/

static void New (Widget w, XtPointer client_data, XtPointer call_data)
{
    Cardinal	  i;
    unsigned	  numnodes;
    ElementDialog eltd;


    eltd = (ElementDialog) client_data;

    Copy (NULL, client_data, NULL);
    SetTextString (eltd -> m_name, "");
    SetTextString (eltd -> l_name [0], "");
    SetTextString (eltd -> l_name [1], "");
    SetTextString (eltd -> l_name [2], "");

    numnodes = eltd -> definition -> numnodes;

    if (numnodes > 6)
	numnodes = 6;

    for (i = 0; i < numnodes; i ++)
	SetTextString (eltd -> node [i], "");
}


/************************************************************************
 * Function:	ElementDialogCreate					*
 *									*
 * Description:	Creates a new element dialog.  You would never want to	*
 *		have more than one of these but the interface is kept	*
 *		consistent with those of the other dialogs.		*
 ************************************************************************/

ElementDialog ElementDialogCreate (Widget parent, String name, String title, XtCallbackProc callback, XtPointer closure)
{
    Cardinal		i;
    Arg			args [1];
    char		buffer [32];
    Widget		group [25];
    Window		window;
    ElementDialog	eltd;
    Dimension		width;
    Position		x;
    static XtAppContext	app_context = NULL;
    static XtActionsRec	actions [ ] = {{"ElementDialogAction", Action},
				       {"ElementDialogMenu", Menu}};


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
	viewport_translations = XtParseTranslationTable (viewport_table);
	help_translations = XtParseTranslationTable (help_table);

	window = RootWindowOfScreen (XtScreen (parent));

	up_bitmap = XCreateBitmapFromData (XtDisplay (parent), window,
		    up_bits, up_width, up_height);

	down_bitmap = XCreateBitmapFromData (XtDisplay (parent), window,
		      down_bits, down_width, down_height);

	left_bitmap = XCreateBitmapFromData (XtDisplay (parent), window,
		      left_bits, left_width, left_height);

	right_bitmap = XCreateBitmapFromData (XtDisplay (parent), window,
		       right_bits, right_width, right_height);
    }


    /* Create the element dialog and its widgets. */

    XtSetArg (shell_args [0], XtNtitle, title);
    XtSetArg (shell_args [1], XtNiconName, title);

    eltd = XtNew (struct element_dialog);
    eltd -> callback = callback;
    eltd -> closure = closure;
    eltd -> active = NULL;
    eltd -> new_loads = True;
    eltd -> new_materials = True;

    eltd -> shell     = XtCreatePopupShell (name,
			topLevelShellWidgetClass, parent,
			shell_args, XtNumber (shell_args));

    eltd -> layout    = XtCreateManagedWidget ("layout",
			layoutWidgetClass, eltd -> shell,
			layout_args, XtNumber (layout_args));

    eltd -> number    = XtCreateManagedWidget ("number",
			labelWidgetClass, eltd -> layout,
			number_args, XtNumber (number_args));

    eltd -> type      = XtCreateManagedWidget ("type",
			labelWidgetClass, eltd -> layout,
			number_args, XtNumber (number_args));

    bitmap_args [0].value = (XtArgVal) up_bitmap;

    eltd -> up        = XtCreateManagedWidget ("up",
			repeaterWidgetClass, eltd -> layout,
			bitmap_args, XtNumber (bitmap_args));

    bitmap_args [0].value = (XtArgVal) down_bitmap;

    eltd -> down      = XtCreateManagedWidget ("down",
			repeaterWidgetClass, eltd -> layout,
			bitmap_args, XtNumber (bitmap_args));

    bitmap_args [0].value = (XtArgVal) left_bitmap;

    eltd -> left      = XtCreateManagedWidget ("left",
			commandWidgetClass, eltd -> layout,
			bitmap_args, XtNumber (bitmap_args));

    bitmap_args [0].value = (XtArgVal) right_bitmap;

    eltd -> right     = XtCreateManagedWidget ("right",
			commandWidgetClass, eltd -> layout,
			bitmap_args, XtNumber (bitmap_args));

    eltd -> m_menu    = XtCreatePopupShell ("materialMenu",
			simpleMenuWidgetClass, eltd -> layout,
			NULL, 0);


    XtSetArg (button_args [0], XtNlabel,    "Material:");
    XtSetArg (button_args [1], XtNmenuName, "materialMenu");

    eltd -> m_button  = XtCreateManagedWidget ("materialButton",
			menuButtonWidgetClass, eltd -> layout,
			button_args, XtNumber (button_args));

    eltd -> m_name    = XtCreateManagedWidget ("materialName",
			asciiTextWidgetClass, eltd -> layout,
			edit_text_args, XtNumber (edit_text_args));


    XtSetArg (button_args [0], XtNlabel,    "Material:");
    XtSetArg (button_args [1], XtNmenuName, "loadMenu");

    eltd -> l_button [0] = XtCreateManagedWidget ("load1Button",
			   menuButtonWidgetClass, eltd -> layout,
			   button_args, XtNumber (button_args));

    eltd -> l_name [0]   = XtCreateManagedWidget ("load1Name",
			   asciiTextWidgetClass, eltd -> layout,
			   edit_text_args, XtNumber (edit_text_args));


    XtSetArg (button_args [0], XtNlabel, "Load 2:");

    eltd -> l_button [1] = XtCreateManagedWidget ("load2Button",
			   menuButtonWidgetClass, eltd -> layout,
			   button_args, XtNumber (button_args));

    eltd -> l_name [1]   = XtCreateManagedWidget ("load2Name",
			   asciiTextWidgetClass, eltd -> layout,
			   edit_text_args, XtNumber (edit_text_args));


    XtSetArg (button_args [0], XtNlabel, "Load 3:");

    eltd -> l_button [2] = XtCreateManagedWidget ("load3Button",
			   menuButtonWidgetClass, eltd -> layout,
			   button_args, XtNumber (button_args));

    eltd -> l_name [2]   = XtCreateManagedWidget ("load3Name",
			   asciiTextWidgetClass, eltd -> layout,
			   edit_text_args, XtNumber (edit_text_args));


    eltd -> l_menu    = XtCreatePopupShell ("loadMenu",
			simpleMenuWidgetClass, eltd -> layout,
			NULL, 0);

    eltd -> viewport  = XtCreateManagedWidget ("viewport",
			viewportWidgetClass, eltd -> layout,
			viewport_args, XtNumber (viewport_args));

    eltd -> stresses  = XtCreateManagedWidget ("stresses",
			labelWidgetClass, eltd -> viewport,
			stress_args, XtNumber (stress_args));

    eltd -> accept    = XtCreateManagedWidget ("accept",
			commandWidgetClass, eltd -> layout,
			NULL, 0);

    eltd -> dismiss   = XtCreateManagedWidget ("dismiss",
			commandWidgetClass, eltd -> layout,
			NULL, 0);

    eltd -> nuke    = XtCreateManagedWidget ("delete",
			commandWidgetClass, eltd -> layout,
			NULL, 0);

    eltd -> nu       = XtCreateManagedWidget ("new",
			commandWidgetClass, eltd -> layout,
			NULL, 0);

    eltd -> copy      = XtCreateManagedWidget ("copy",
			commandWidgetClass, eltd -> layout,
			NULL, 0);

    eltd -> help      = CreateHelpButton (eltd -> layout, "help");


    for (i = 0; i < 6; i ++) {
	sprintf (buffer, "node%d", i + 1);
	eltd -> node [i] = XtCreateManagedWidget (XtNewString (buffer),
			       asciiTextWidgetClass, eltd -> layout,
			       node_args, XtNumber (node_args));
    }


    for (i = 0; i < XtNumber (labels); i ++) {
	label_args [0].value = (XtArgVal) labels [i];
	XtCreateManagedWidget (names [i], labelWidgetClass,
			eltd -> layout, label_args, XtNumber (label_args));
    }


    XtCreateManagedWidget ("separator1", coreWidgetClass,
			eltd -> layout, core_args, XtNumber (core_args));

    XtCreateManagedWidget ("separator2", coreWidgetClass,
			eltd -> layout, core_args, XtNumber (core_args));

    XtCreateManagedWidget ("separator3", coreWidgetClass,
			eltd -> layout, core_args, XtNumber (core_args));

    XtCreateManagedWidget ("separator4", coreWidgetClass,
			eltd -> layout, core_args, XtNumber (core_args));

    XtCreateManagedWidget ("- none -", smeBSBObjectClass,
			eltd -> m_menu, NULL, 0);

    XtCreateManagedWidget ("- none -", smeBSBObjectClass,
			eltd -> l_menu, NULL, 0);


    AddPostMenuActions (eltd -> m_menu);
    AddPostMenuActions (eltd -> l_menu);


    /* Create a tab group for the element dialog. */

    group [0]  = eltd -> up;
    group [1]  = eltd -> down;
    group [2]  = eltd -> left;
    group [3]  = eltd -> right;
    group [4]  = eltd -> node [0];
    group [5]  = eltd -> node [1];
    group [6]  = eltd -> node [2];
    group [7]  = eltd -> node [3];
    group [8]  = eltd -> node [4];
    group [9]  = eltd -> node [5];
    group [10] = eltd -> m_button;
    group [11] = eltd -> m_name;
    group [12] = eltd -> l_button [0];
    group [13] = eltd -> l_name [0];
    group [14] = eltd -> l_button [1];
    group [15] = eltd -> l_name [1];
    group [16] = eltd -> l_button [2];
    group [17] = eltd -> l_name [2];
    group [18] = eltd -> viewport;
    group [19] = eltd -> help;
    group [20] = eltd -> accept;
    group [21] = eltd -> dismiss;
    group [22] = eltd -> nuke;
    group [23] = eltd -> nu;
    group [24] = eltd -> copy;


    XtGetValues (eltd -> layout, color_args, XtNumber (color_args));
    CreateTabGroup (eltd -> shell, group, XtNumber(group), highlight, True);
    XtRealizeWidget (eltd -> shell);
    SetFocus (eltd -> up);
    SetType (eltd, (Definition) *(problem.definition_set.begin()));

    button_args [0].value = (XtArgVal) "Load 1:";
    XtSetValues (eltd -> l_button [0], button_args, 1);

    XtSetArg (args [0], XtNwidth, &width);
    XtGetValues (eltd -> layout, args, 1);
    XtSetArg (args [0], XtNx, &x);
    XtGetValues (eltd -> help, args, 1);
    UpdateHelpMessage (eltd -> help, help_message, width - 2 * x);


    /* Add the translations to each widget. */

    AddDeleteWindowProtocol (eltd -> shell, "ElementDialogAction()");

    XtOverrideTranslations (eltd -> up,		  repeater_translations);
    XtOverrideTranslations (eltd -> down,	  repeater_translations);
    XtOverrideTranslations (eltd -> left,	  repeater_translations);
    XtOverrideTranslations (eltd -> right,	  repeater_translations);
    XtOverrideTranslations (eltd -> m_name,	  text_translations);
    XtOverrideTranslations (eltd -> l_name [0],	  text_translations);
    XtOverrideTranslations (eltd -> l_name [1],	  text_translations);
    XtOverrideTranslations (eltd -> l_name [2],	  text_translations);
    XtOverrideTranslations (eltd -> m_button,	  button_translations);
    XtOverrideTranslations (eltd -> l_button [0], button_translations);
    XtOverrideTranslations (eltd -> l_button [1], button_translations);
    XtOverrideTranslations (eltd -> l_button [2], button_translations);
    XtOverrideTranslations (eltd -> accept,	  command_translations);
    XtOverrideTranslations (eltd -> dismiss,	  command_translations);
    XtOverrideTranslations (eltd -> nuke,	  command_translations);
    XtOverrideTranslations (eltd -> nu,	  command_translations);
    XtOverrideTranslations (eltd -> copy,	  command_translations);
    XtOverrideTranslations (eltd -> viewport,	  viewport_translations);
    XtOverrideTranslations (eltd -> help,	  help_translations);

    for (i = 0; i < 6; i ++)
	XtOverrideTranslations (eltd -> node [i], text_translations);


    /* Add the necessary callbacks. */

    XtAddCallback (eltd -> up,      XtNcallback, Up,      (XtPointer) eltd);
    XtAddCallback (eltd -> down,    XtNcallback, Down,    (XtPointer) eltd);
    XtAddCallback (eltd -> left,    XtNcallback, Left,    (XtPointer) eltd);
    XtAddCallback (eltd -> right,   XtNcallback, Right,   (XtPointer) eltd);
    XtAddCallback (eltd -> accept,  XtNcallback, Accept,  (XtPointer) eltd);
    XtAddCallback (eltd -> dismiss, XtNcallback, Dismiss, (XtPointer) eltd);
    XtAddCallback (eltd -> nuke,  XtNcallback, Delete,  (XtPointer) eltd);
    XtAddCallback (eltd -> nu,     XtNcallback, New,     (XtPointer) eltd);
    XtAddCallback (eltd -> copy,    XtNcallback, Copy,    (XtPointer) eltd);

    XtAddCallback (eltd -> m_menu, XtNpopupCallback,
		   UpdateMaterialMenu, (XtPointer) eltd);

    XtAddCallback (eltd -> m_menu, XtNpopdownCallback,
		   UpdateMaterialName, (XtPointer) eltd);

    XtAddCallback (eltd -> l_menu, XtNpopupCallback,
		   UpdateLoadMenu, (XtPointer) eltd);

    XtAddCallback (eltd -> l_menu, XtNpopdownCallback,
		   UpdateLoadName, (XtPointer) eltd);

    return eltd;
}


/************************************************************************
 * Function:	ElementDialogPopup					*
 *									*
 * Description:	Pops up the specified element dialog.			*
 ************************************************************************/

void ElementDialogPopup (ElementDialog eltd)
{
    XtPopup (eltd -> shell, XtGrabNone);
}


/************************************************************************
 * Function:	ElementDialogUpdate					*
 *									*
 * Description:	Updates the specified element dialog with the specified	*
 *		trees.							*
 ************************************************************************/

void ElementDialogUpdate (ElementDialog eltd, Problem::ElementSet *elements, Problem::MaterialSet *materials, Tree loads, Tree nodes)
{
    /* Remember to update the menus if necessary. */

    if (materials != NULL) {
	eltd -> materials = materials;
	eltd -> new_materials = True;
    }

    if (loads != NULL) {
	eltd -> loads = loads;
	eltd -> new_loads = True;
    }

    if (nodes != NULL)
	eltd -> nodes = nodes;


    /* Determine a new active element if necessary. */

    if (!elements && eltd -> active == NULL)
        eltd -> active = *(eltd->elements->begin());

    if (elements && (eltd -> active == NULL || eltd -> elements != elements))
        eltd -> active = *(elements->begin());

    if (elements != NULL) {
	eltd -> elements = elements;
	if (eltd -> active != NULL)
	    ElementDialogDisplay (eltd, eltd -> active);
	else
	    New (NULL, (XtPointer) eltd, NULL);
    }
}


/************************************************************************
 * Function:	ElementDialogActive					*
 *									*
 * Description:	Returns the currently displayed (active) element.	*
 ************************************************************************/

Element ElementDialogActive (ElementDialog eltd)
{
    return eltd -> active;
}


/************************************************************************
 * Function:	ElementDialogDisplay					*
 *									*
 * Description:	Displays the specified element.				*
 ************************************************************************/

void ElementDialogDisplay (ElementDialog eltd, Element element)
{
    unsigned	numnodes;
    Distributed	load;
    Cardinal	i;
    Cardinal	j;
    Cardinal	count;
    Element	active;
    char	buffer [2048];


    /* Check if the element exists. */

    eltd -> active = element;

    if (eltd -> active == NULL)
	return;

    active = eltd -> active;
    eltd -> new_copy = 0;


    /* Update all of the entries. */

    SetNumber (eltd, active -> number);
    SetType   (eltd, active -> definition);

    if (active -> material != NULL)
        SetTextString (eltd -> m_name, active -> material -> name.c_str());
    else
        SetTextString (eltd -> m_name, "");

    for (i = 1; i <= 3; i ++)
	if (active -> numdistributed >= i && (load = active -> distributed [i]))
	    SetTextString (eltd -> l_name [i - 1], load -> name.c_str());
	else
	    SetTextString (eltd -> l_name [i - 1], "");


    numnodes = active -> definition -> numnodes;

    for (i = 1; i <= numnodes; i ++)
	if (element -> node [i] != NULL)
	    eltd -> node_values [i - 1] = element -> node [i] -> number;
	else
	    eltd -> node_values [i - 1] = 0;


    if (element -> ninteg == 0 || element -> stress.empty())
	SetLabelString (eltd -> stresses, "No stresses available");
    else {
	count = 0;
	buffer [0] = 0;
	for (i = 1; i <= element -> ninteg; i ++)
	    for (j = 1; j <= element -> definition -> numstresses; j ++) {
		sprintf (buffer + strlen (buffer), "%11.5g ",
			 element -> stress [i] -> values [j]);
		if (++ count == 6) {
		    sprintf (buffer + strlen (buffer), "\n");
		    count = 0;
		}
	    }

	SetLabelString (eltd -> stresses, buffer);
    }

    XawViewportSetCoordinates (eltd -> viewport, 0, 0);

    DisplayNodes (eltd);
}
