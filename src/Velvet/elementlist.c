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
 * File:	elementlist.c						*
 *									*
 * Description: This file contains the private and public function and	*
 *		type definitions for the element list box.		*
 ************************************************************************/

# include <stdio.h>
# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <X11/Shell.h>
# include <X11/Xaw/AsciiText.h>
# include <X11/Xaw/Command.h>
# include <X11/Xaw/Label.h>
# include <X11/Xaw/List.h>
# include <X11/Xaw/Viewport.h>
# include "Layout.h"
# include "ElementList.h"
# include "TabGroup.h"
# include "util.h"
# include "problem.h"
# include "error.h"
# include "definition.h"

static String	*element_names;
static int	num_types;

struct element_list {
    Widget	   shell;		/* transientShell  <specified>	*/
    Widget	   layout;		/*	Layout  layout		*/
    Widget	   label;		/*	     Label  label	*/
    Widget	   entry;		/*	     AsciiText  entry	*/
    Widget	   viewport;		/*	     Viewport  viewport	*/
    Widget	   list;		/*		  List  list	*/
    Widget	   accept;		/*	     Command  accept	*/
    Widget	   dismiss;		/*	     Command  dismiss	*/
};


/* Resources */

static Pixel highlight;

static String dummy_list [ ] = {
    "                              ", "", "", "", "", "", "", "", "", NULL
};

static char layout_string [ ] =
"vertical { 4 \
 horizontal { 4 label <+inf -100% *> 4 } 4 \
 horizontal { 4 entry <+inf -100% *> 4 } 4 \
 horizontal { 4 viewport <+inf -100% * +inf -100%> 4 } 4 \
 horizontal { 4 accept 4 <+inf -100%> dismiss 4 } 4 }";

static Arg color_args [ ] = {
    {XtNborderColor, (XtArgVal) &highlight},
};

static Arg layout_args [ ] = {
    {XtNlayout, (XtArgVal) NULL},
};

static Arg label_args [ ] = {
    {XtNresize,      (XtArgVal) True},
    {XtNjustify,     (XtArgVal) XtJustifyLeft},
    {XtNborderWidth, (XtArgVal) 0},
};

static Arg entry_args [ ] = {
    {XtNresize,      (XtArgVal) XawtextResizeWidth},
    {XtNeditType,    (XtArgVal) XawtextEdit},
    {XtNborderWidth, (XtArgVal) 0},
};

static Arg viewport_args [ ] = {
    {XtNallowVert,   (XtArgVal) True},
    {XtNuseRight,    (XtArgVal) True},
    {XtNforceBars,   (XtArgVal) True},
    {XtNborderWidth, (XtArgVal) 0},
};

static Arg list_args [ ] = {
    {XtNdefaultColumns, (XtArgVal) 1},
    {XtNforceColumns,   (XtArgVal) 1},
    {XtNresize,         (XtArgVal) True},
    {XtNlist,           (XtArgVal) dummy_list},
};


/* Translation tables */

static String entry_table =
"<Key>Return:   ElementListAccept()\n\
 <Key>Escape:   ElementListDismiss()\n\
 <Btn1Down>:    SetFocus() select-start()";

static XtTranslations entry_translations;

static String viewport_table =
"<Key>Return:   ElementListAccept()\n\
 <Key>Escape:   ElementListDismiss()\n\
 <Btn1Down>:    SetFocus()";

static XtTranslations viewport_translations;

static String list_table =
"<Btn1Up>(2):	Notify() ElementListAccept()\n\
 <Btn2Down>:    Set()\n\
 <Btn2Up>:      Notify() ElementListAccept()";

static XtTranslations list_translations;

static String accept_table =
"<Key>Return:   AutoRepeat(off) set()\n\
 <KeyUp>Return: AutoRepeat(saved) notify() unset()\n\
 <Key>space:    AutoRepeat(off) set()\n\
 <KeyUp>space:  AutoRepeat(saved) notify() unset()\n\
 <Key>Escape:   ElementListDismiss()";

static XtTranslations accept_translations;

static String dismiss_table =
"<Key>Return:   AutoRepeat(off) set()\n\
 <KeyUp>Return: AutoRepeat(saved) notify() unset()\n\
 <Key>space:    AutoRepeat(off) set()\n\
 <KeyUp>space:  AutoRepeat(saved) notify() unset()\n\
 <Key>Escape:   ElementListDismiss()";

static XtTranslations dismiss_translations;


/************************************************************************
 * Function:	CopySelected						*
 *									*
 * Description:	A callback for the List widget which copies the current	*
 *		entry to the entry widget.				*
 ************************************************************************/

static void CopySelected (w, client_data, call_data)
    Widget    w;
    XtPointer client_data;
    XtPointer call_data;
{
    ElementList		 elementl;
    XawListReturnStruct *info;


    elementl = (ElementList) client_data;
    info = (XawListReturnStruct *) call_data;
    SetTextString (elementl -> entry, info -> string);
}


/************************************************************************
 * Function:	Accept							*
 *									*
 * Description:	A callback for the accept button which forms a name	*
 *		from the current entry.  				*
 ************************************************************************/

static void Accept (w, client_data, call_data)
    Widget    w;
    XtPointer client_data;
    XtPointer call_data;
{
    XawListReturnStruct	*item;
    ElementList		 elementl;
    Arg			 args [1];
    String		 value;
    unsigned		 i;
    int			 index;

    elementl = (ElementList) client_data;
    item = XawListShowCurrent (elementl -> list);
 
    XtSetArg (args [0], XtNstring,  &value);
    XtGetValues (elementl -> entry, args, 1);

    index = XAW_LIST_NONE;
    for (i = 0 ; i < num_types ; i++) {
       if (strcmp (element_names [i], value) == 0) {
          index = i;
          break;
       }
    }  

    if (index == XAW_LIST_NONE)
       CopySelected (NULL, (XtPointer) elementl, (XtPointer) item);
    else
       XawListHighlight (elementl -> list, index);
}

/************************************************************************
 * Function:	Dismiss							*
 *									*
 * Description:	A callback for the dismiss button which indicates that	*
 *		the user wishes to popdown the element list		*
 ************************************************************************/

static void Dismiss (w, client_data, call_data)
    Widget    w;
    XtPointer client_data;
    XtPointer call_data;
{
    ElementList	elementl;

    elementl = (ElementList) client_data;
    ElementListPopdown (elementl);
}


/************************************************************************
 * Function:	ElementListAccept					*
 *									*
 * Description:	An action procedure which emulates pressing of the 	*
 *		accept button.						* 
 ************************************************************************/

static void ElementListAccept (w, event, params, num_params)
    Widget   w;
    XEvent  *event;
    String  *params;
    Cardinal num_params;
{
    if (XtClass (w) == listWidgetClass)
	w = XtParent (w);

    w = XtNameToWidget (XtParent (w), "accept");
    XtCallCallbacks (w, XtNcallback, NULL);
}


/************************************************************************
 * Function:	ElementListDismiss					*
 *									*
 * Description:	An action procedure which emulates pressing of the	*
 *		dismiss button.						*
 ************************************************************************/

static void ElementListDismiss (w, event, params, num_params)
    Widget   w;
    XEvent  *event;
    String  *params;
    Cardinal num_params;
{
    if (event -> type == ClientMessage)
	w = XtNameToWidget (w, "layout.dismiss");
    else
	w = XtNameToWidget (XtParent (w), "dismiss");

    XtCallCallbacks (w, XtNcallback, NULL);
}

/************************************************************************
 * Function:	ElementListUpdate					*
 *									*
 * Description:	Sets the element names into the list widget		*
 ************************************************************************/
 
static void ElementListUpdate (elementl)
    ElementList	elementl;
{
    XawListChange (elementl -> list, element_names, num_types, 0, True);
    XawViewportSetCoordinates (elementl -> viewport, 0, 0);


    return;
}

/************************************************************************
 * Function:	ElementListCreate					*
 *									*
 * Description:	Creates and returns a new element list box.  		*
 ************************************************************************/

ElementList ElementListCreate (parent, name, title)
    Widget parent;
    String name;
    String title;
{
    Arg			args[1];
    Widget		group [4];
    ElementList		elementl;
    static XtAppContext	app_context = NULL;
    static XtActionsRec actions [ ] =
			{{"ElementListAccept",   ElementListAccept},
			 {"ElementListDismiss",  ElementListDismiss}};


    /* Perform one time initialization. */

    if (app_context == NULL) {
	app_context = XtWidgetToApplicationContext (parent);
	XtAppAddActions (app_context, actions, XtNumber (actions));
	AddAutoRepeatAction (app_context);

	layout_args [0].value = StringToLayout (parent, layout_string);

	entry_translations    = XtParseTranslationTable (entry_table);
	viewport_translations = XtParseTranslationTable (viewport_table);
	list_translations     = XtParseTranslationTable (list_table);
	accept_translations   = XtParseTranslationTable (accept_table);
	dismiss_translations  = XtParseTranslationTable (dismiss_table);
    }


    /* Create the element list and its widgets. */

    elementl = XtNew (struct element_list);

    elementl -> shell     = XtCreatePopupShell (name,
			 applicationShellWidgetClass, parent,
			 NULL, 0);

    elementl -> layout    = XtCreateManagedWidget ("layout",
			 layoutWidgetClass, elementl -> shell,
			 layout_args, XtNumber (layout_args));

    elementl -> label     = XtCreateManagedWidget ("label",
			 labelWidgetClass, elementl -> layout,
			 label_args, XtNumber (label_args));

    elementl -> entry     = XtCreateManagedWidget ("entry",
			 asciiTextWidgetClass, elementl -> layout,
			 entry_args, XtNumber (entry_args));

    elementl -> viewport  = XtCreateManagedWidget ("viewport",
			 viewportWidgetClass, elementl -> layout, 
			 viewport_args, XtNumber (viewport_args));

    elementl -> list      = XtCreateManagedWidget ("list",
			 listWidgetClass, elementl -> viewport,
			 list_args, XtNumber (list_args));

    elementl -> accept    = XtCreateManagedWidget ("accept",
			 commandWidgetClass, elementl -> layout,
			 NULL, 0);

    elementl -> dismiss   = XtCreateManagedWidget ("dismiss",
			 commandWidgetClass, elementl -> layout,
			 NULL, 0);


    /* Create a tab group for the element list. */

    group [0] = elementl -> entry;
    group [1] = elementl -> viewport;
    group [2] = elementl -> accept;
    group [3] = elementl -> dismiss;

    XtGetValues (elementl -> layout, color_args, XtNumber (color_args));
    CreateTabGroup(elementl -> shell, group, XtNumber (group), highlight,True);
    XtRealizeWidget (elementl -> shell);


    /* Add the translations to each widget. */

    AddDeleteWindowProtocol   (elementl -> shell, "ElementListDismiss()");
    ListAddCursorTranslations (elementl -> viewport);
    ListAddCursorAccelerators (elementl -> viewport, elementl -> entry);

    XtOverrideTranslations (elementl -> entry,    entry_translations);
    XtOverrideTranslations (elementl -> viewport, viewport_translations);
    XtOverrideTranslations (elementl -> list,     list_translations);
    XtOverrideTranslations (elementl -> accept,     accept_translations);
    XtOverrideTranslations (elementl -> dismiss,   dismiss_translations);


    /* Add the necessary callbacks. */

    XtAddCallback(elementl->list,XtNcallback,CopySelected,(XtPointer) elementl);
    XtAddCallback(elementl->dismiss,XtNcallback,Dismiss,(XtPointer) elementl);
    XtAddCallback(elementl->accept,XtNcallback,Accept,(XtPointer) elementl);

    /* set some labels */

    XtSetArg (args [0], XtNtitle, title);
    XtSetValues (elementl -> shell, args, 1);
 
    XtSetArg (args [0], XtNlabel, "Element name:");
    XtSetValues (elementl -> label, args, 1);

    ElementListSetupNames (elementl);

    ElementListUpdate (elementl);

    return elementl;
}

/************************************************************************
 * Function:	ElementListDefinition					*
 *									*
 * Description: Sets the currently displayed element to the given	*
 *		definition;
 ************************************************************************/

void ElementListSet (elementl, definition)
    ElementList	elementl;
    Definition	definition;
{
    unsigned	i;
    int		index;

    if (definition == NULL) {
       XawListUnhighlight (elementl -> list);
       SetTextString (elementl -> entry, "");
       return;
    } 

    index = XAW_LIST_NONE;
    for (i = 0; i < num_types ; i++) {
       if (LookupDefinition (element_names [i]) == definition) {
          index = i;
          break;
       }
    }

    XawListHighlight (elementl -> list, index);
    if (index == XAW_LIST_NONE)
       SetTextString (elementl -> entry, "");
    else
       SetTextString (elementl -> entry, element_names [index]);
}

/************************************************************************
 * Function:	ElementListDefinition					*
 *									*
 * Description: Returns the currently selected element definition	*
 ************************************************************************/

Definition ElementListDefinition (elementl)
    ElementList	elementl;
{
    XawListReturnStruct	*item;

    item = XawListShowCurrent (elementl -> list);

    if (item -> list_index != XAW_LIST_NONE)
       return LookupDefinition (element_names [item -> list_index]);
    else
       return NULL;
}

/************************************************************************
 * Function:	ElementListName      	 				*
 *									*
 * Description: Returns the currently selected element definition	*
 ************************************************************************/

String ElementListName (elementl)
    ElementList	elementl;
{
    XawListReturnStruct	*item;

    item = XawListShowCurrent (elementl -> list);

    if (item -> list_index != XAW_LIST_NONE)
       return element_names [item -> list_index];
    else
       return NULL;
}

static int	count;

static int SetName (item)
    Item	item;
{
    element_names [count ++] = XtNewString (((Definition) item) -> name);
    return 0;
}

/************************************************************************
 * Function:	ElementListSetupNames					*
 *									*
 * Description: Initializes the array of element type names based	*
 *		on the definition_tree					*
 ************************************************************************/

void ElementListSetupNames (elementl)
    ElementList	elementl;
{
    unsigned	i;

    if (num_types > 0) {
        for (i = 0 ; i < num_types ; i++)
           XtFree (element_names [i]);

        XtFree ((char *) element_names);
    }
     
    num_types = TreeSize (problem.definition_tree);
    element_names = (String *) XtMalloc (sizeof(String) * num_types);

    count = 0;
    TreeSetIterator (problem.definition_tree, SetName);
    TreeIterate (problem.definition_tree);

    return;
}

/************************************************************************
 * Function:	ElementListPopup					*
 *									*
 * Description: Pops up the element list with the specified title.	*
 ************************************************************************/

void ElementListPopup (elementl)
    ElementList	   elementl;
{
    SetFocus (elementl -> entry);
    XtPopup (elementl -> shell, XtGrabNone);
}


/************************************************************************
 * Function:	ElementListPopdown					*
 *									*
 * Descripion:	Pops down the specified element list			*
 ************************************************************************/

void ElementListPopdown (elementl)
    ElementList elementl;
{
    XtPopdown (elementl -> shell);
}
