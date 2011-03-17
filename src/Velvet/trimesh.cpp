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
 * File:	trimesh.c						*
 *									*
 * Description:	This file contains the private and public function and	*
 *		type definitions for the trimesh dialog box.		*
 ************************************************************************/

# include <stdio.h>
# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <X11/Shell.h>
# include <X11/Xaw/AsciiText.h>
# include <X11/Xaw/Command.h>
# include "Layout.h"
# include "Trimesh.h"
# include "TabGroup.h"
# include "util.h"
# include "code.h"

# ifndef X_NOT_STDC_ENV
# include <stdlib.h>
# else
extern int    atoi ( );
# endif

# define WaitState 0
# define OkayState 1
# define CancelState 2

struct trimesh_dialog {
    Widget  shell;		/* topLevelShell  <specified>	*/
    Widget  layout;		/*	Layout  layout		*/
    Widget  target;		/*	     AsciiText target	*/
    Widget  alpha;		/*	     AsciiText alpha	*/
    Widget  nholes;		/*	     AsciiText nholes	*/
    Widget  help;		/*	     MenuButton  help	*/
    Widget  okay;		/*	     Command  okay	*/
    Widget  cancel;		/*	     Command  cancel	*/
    TriMesh tm;			/* TriMesh			*/
    int	    status;		/* status			*/
};


static String property_labels [ ] = {
    "target:", "alpha:", "nholes:"
};

static String property_names [ ] = {
    "target_name", "alpha_name", "nholes_name"
};

static String property_help [ ] = {
   "target number of elements",
   "area constraint factor: max elt area = alpha*Atot/target",
   "number of holes in the generation region"
};


/* Resources */

static Pixel highlight;

static char layout_string [ ] =
"vertical { \
     horizontal { \
        vertical { \
            4 \
            ((height target - height target_name) / 2) \
            target_name \
            ((height target - height target_name) / 2) \
	    4 \
            ((height alpha - height alpha_name) / 2) \
            alpha_name \
            ((height alpha - height alpha_name) / 2) \
            4 \
            ((height nholes - height nholes_name) / 2) \
            nholes_name \
            ((height nholes - height nholes_name) / 2) \
            4 \
        } \
        1 \
        vertical { \
            4 \
            target \
            4 \
            alpha \
            4 \
            nholes \
            4 \
        } \
        4 \
     } \
     4 \
     separator <+inf -100% *> \
     4 \
     horizontal { \
 	4 \
 	help \
 	4 <+inf -100%> \
 	okay \
 	4 <+inf -100%> \
 	cancel \
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

static Arg core_args [ ] = {
    {XtNwidth,  (XtArgVal) 3},
    {XtNheight, (XtArgVal) 3},
};

static Arg text_args [ ] = {
    {XtNeditType,    (XtArgVal) XawtextEdit},
    {XtNborderWidth, (XtArgVal) 0},
    {XtNpieceSize,   (XtArgVal) 32},
    {XtNcursorName,  (XtArgVal) "left_ptr"},
};

# ifndef XtNshadowWidth
# define XtNshadowWidth "shadowWidth"
# endif

static Arg property_args [ ] = {
    {XtNlabel,	            (XtArgVal) ""},
    {XtNborderWidth,        (XtArgVal) 0},
    {XtNhighlightThickness, (XtArgVal) 0},
    {XtNshadowWidth,	    (XtArgVal) 0},
};

/* Translation tables */

static String text_table =
"<Key>Return: TrimeshDialogAction(okay)\n\
 <Key>Escape: TrimeshDialogAction(cancel)\n\
 Ctrl<Key>h:  TrimeshDialogAction(help)\n\
 <Btn1Down>:  SetFocus() select-start()";

static XtTranslations text_translations;

static String command_table =
"<Key>Return:   AutoRepeat(off) set()\n\
 <KeyUp>Return: AutoRepeat(saved) notify() unset()\n\
 <Key>space:    AutoRepeat(off) set()\n\
 <KeyUp>space:  AutoRepeat(saved) notify() unset()\n\
 <Key>Escape:   TrimeshDialogAction(cancel)";

static XtTranslations command_translations;


/* Help message. */

static String help_message ="\
Use this form to define the parameters for the triangular mesh which you \
want to generate.  For a more detailed description of each parameter, \
click on the label for that variable.";


/************************************************************************
 * Function:	Action							*
 *									*
 * Description:	An action procedure which emulates pressing of the	*
 *		specified button.					*
 ************************************************************************/

static void Action (Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    w = XtNameToWidget (XtParent (w), params [0]);

    XtCallCallbacks (w, XtNcallback, NULL);
}


/************************************************************************
 * Function:	Okay							*
 *									*
 * Description:								*
 ************************************************************************/

static void Okay (Widget w, XtPointer client_data, XtPointer call_data)
{
    TrimeshDialog   trimeshd;


    trimeshd = (TrimeshDialog) client_data;
    trimeshd -> status = OkayState;
}


/************************************************************************
 * Function:	Cancel							*
 *									*
 * Description:	sets the cancel flag					*
 ************************************************************************/

static void Cancel (Widget w, XtPointer client_data, XtPointer call_data)
{
    TrimeshDialog trimeshd;

    trimeshd = (TrimeshDialog) client_data;
    trimeshd -> status = CancelState;
}

/************************************************************************
 * Function:	TrimeshDialogSet					*
 *									*
 * Description:	fills the TriMesh structure based on the text fields	*
 ************************************************************************/

static void TrimeshDialogSet (TrimeshDialog trimeshd)
{
    Arg		args [1];
    String	value;
    char	buffer [80];
    int		numholes;

    XtSetArg (args [0], XtNstring, &value);

    XtGetValues (trimeshd -> target, args, 1); 
    trimeshd -> tm -> target = exptod (value, NULL);
    sprintf (buffer, (trimeshd -> tm -> target ? "%d" : "0.0"), 
             trimeshd -> tm -> target);
    SetTextString (trimeshd -> target, buffer);

    XtGetValues (trimeshd -> alpha, args, 1); 
    trimeshd -> tm -> alpha = exptod (value, NULL);
    sprintf (buffer, (trimeshd -> tm -> alpha ? "%g" : "0.0"), 
             trimeshd -> tm -> alpha);
    SetTextString (trimeshd -> alpha, buffer);

    XtGetValues (trimeshd -> nholes, args, 1); 
    numholes = atoi (value);
    trimeshd -> tm -> numcurves = numholes + 1;
    sprintf (buffer, (numholes ? "%d" : "0"), numholes);
    SetTextString (trimeshd -> nholes, buffer);
}

/************************************************************************
 * Function:	TrimeshDialogCreate					*
 *									*
 * Description:	Creates a new material dialog.  You would never want to	*
 *		have more than one of these but the interface is kept	*
 *		consistent with those of the other dialogs.		*
 ************************************************************************/

TrimeshDialog TrimeshDialogCreate (Widget parent, String name, String title)
{
    Cardinal		i;
    Arg			args [1];
    Widget		group [6];
    TrimeshDialog	trimeshd;
    Dimension		width;
    Position		x;
    Widget		property_help_widget;
    static XtAppContext	app_context = NULL;
    static XtActionsRec	actions [ ] = {{"TrimeshDialogAction", Action}};


    /* Perform one time initialization. */

    if (app_context == NULL) {
	app_context = XtWidgetToApplicationContext (parent);
	XtAppAddActions (app_context, actions, XtNumber (actions));
	AddAutoRepeatAction (app_context);

        layout_args [0].value = StringToLayout (parent, layout_string);

	text_translations = XtParseTranslationTable (text_table);
	command_translations = XtParseTranslationTable (command_table);
    }


    /* Create the material dialog and its widgets. */

    XtSetArg (shell_args [0], XtNtitle,    title);
    XtSetArg (shell_args [1], XtNiconName, title);

    trimeshd = XtNew (struct trimesh_dialog);

    trimeshd -> tm = XtNew (struct _trimesh);

    trimeshd -> shell    = XtCreatePopupShell (name,
			 topLevelShellWidgetClass, parent,
			 shell_args, XtNumber (shell_args));

    trimeshd -> layout   = XtCreateManagedWidget ("layout",
			 layoutWidgetClass, trimeshd -> shell,
			 layout_args, XtNumber (layout_args));

    trimeshd -> target  = XtCreateManagedWidget ("target",
			 asciiTextWidgetClass, trimeshd -> layout,
			 text_args, XtNumber (text_args));

    trimeshd -> alpha  = XtCreateManagedWidget ("alpha",
			 asciiTextWidgetClass, trimeshd -> layout,
			 text_args, XtNumber (text_args));

    trimeshd -> nholes = XtCreateManagedWidget ("nholes",
			 asciiTextWidgetClass, trimeshd -> layout,
			 text_args, XtNumber (text_args));

    trimeshd -> okay   = XtCreateManagedWidget ("okay",
			 commandWidgetClass, trimeshd -> layout,
			 NULL, 0);

    trimeshd -> cancel  = XtCreateManagedWidget ("cancel",
			 commandWidgetClass, trimeshd -> layout,
			 NULL, 0);

    trimeshd -> help     = CreateHelpButton (trimeshd -> layout, "help");

    XtCreateManagedWidget ("separator", coreWidgetClass,
			trimeshd -> layout, core_args, XtNumber (core_args));

    for (i = 0 ; i < XtNumber (property_labels) ; i++) {
        property_args [0].value = (XtArgVal) property_labels [i];
        property_help_widget = CreateHelpButton (trimeshd -> layout, 
                                                 property_names [i]);

        XtSetValues (property_help_widget, property_args, 
                     XtNumber (property_args));

        UpdateHelpMessage (property_help_widget, property_help [i], 200);
    }


    /* set the defaults */

    SetTextString (trimeshd -> target, "100");
    SetTextString (trimeshd -> alpha, "2.0");
    SetTextString (trimeshd -> nholes, "0");


    /* Create a tab group for the material dialog. */

    group [0]  = trimeshd -> target;
    group [1]  = trimeshd -> alpha;
    group [2]  = trimeshd -> nholes;
    group [3]  = trimeshd -> help;
    group [4]  = trimeshd -> okay;
    group [5] = trimeshd -> cancel;

    XtGetValues (trimeshd -> layout, color_args, XtNumber (color_args));
    CreateTabGroup (trimeshd -> shell, group, XtNumber (group), 
                    highlight, True);

    XtRealizeWidget (trimeshd -> shell);
    SetFocus (trimeshd -> target);

    XtSetArg (args [0], XtNwidth, &width);
    XtGetValues (trimeshd -> layout, args, 1);
    XtSetArg (args [0], XtNx, &x);
    XtGetValues (trimeshd -> help, args, 1);
    UpdateHelpMessage (trimeshd -> help, help_message, width - 2 * x);


    /* Add the translations to each widget. */

    AddDeleteWindowProtocol (trimeshd -> shell, "TrimeshDialogAction()");

    XtOverrideTranslations (trimeshd -> target,	text_translations);
    XtOverrideTranslations (trimeshd -> alpha,	text_translations);
    XtOverrideTranslations (trimeshd -> nholes,	text_translations);
    XtOverrideTranslations (trimeshd -> cancel, command_translations);
    XtOverrideTranslations (trimeshd -> okay,   command_translations);


    /* Add the necessary callbacks. */

    XtAddCallback(trimeshd->okay,  XtNcallback,Okay, (XtPointer) trimeshd);
    XtAddCallback(trimeshd->cancel,XtNcallback,Cancel,(XtPointer) trimeshd);

    return trimeshd;
}


/************************************************************************
 * Function:	TrimeshDialogPopup					*
 *									*
 * Description:	Pops up the specified material dialog.			*
 ************************************************************************/

TriMesh TrimeshDialogPopup (TrimeshDialog trimeshd)
{
    XEvent	 event;
    XtAppContext app_context;
    
    app_context = XtWidgetToApplicationContext (trimeshd -> shell);

    XtPopup (trimeshd -> shell, XtGrabExclusive);
    trimeshd -> status = WaitState;

    while (trimeshd -> status == WaitState) {
       XtAppNextEvent (app_context, &event);
       XtDispatchEvent (&event);
    }

    XtPopdown (trimeshd -> shell);

    if (trimeshd -> status == CancelState)
       return NULL;
    else {
       TrimeshDialogSet (trimeshd);
       return trimeshd -> tm;
    }
}
