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
    Widget  tolin;		/*	     AsciiText tolin	*/
    Widget  angtol;		/*	     AsciiText angtol	*/
    Widget  angspc;		/*	     AsciiText angspc	*/
    Widget  dmin;		/*	     AsciiText dmin	*/
    Widget  kappa;		/*	     AsciiText kapp	*/
    Widget  min;		/*	     AsciiText min	*/
    Widget  max;		/*	     AsciiText max	*/
    Widget  nholes;		/*	     AsciiText nholes	*/
    Widget  help;		/*	     MenuButton  help	*/
    Widget  okay;		/*	     Command  okay	*/
    Widget  cancel;		/*	     Command  cancel	*/
    TriMesh tm;			/* TriMesh			*/
    int	    status;		/* status			*/
};


static String property_labels [ ] = {
    "tolin:", "angspc:", "angtol:", "dmin:", "k:", "min:", "max:", "nholes:"
};

static String property_names [ ] = {
    "tolin_name", "angspc_name", "angtol_name", "dmin_name", "kappa_name", 
    "min_name", "max_name", "nholes_name"
};

static String property_help [ ] = {
   "relative tolerance",
   "angular spacing",
   "angular tolerance",
   "minimum distribution function variation",
   "mesh density function",
   "minimum number of triangles to generate",
   "desired number of triangles",
   "number of holes in the generation region"
};


/* Resources */

static Pixel highlight;

static char layout_string [ ] =
"vertical { \
     horizontal { \
        vertical { \
            4 \
            ((height tolin - height tolin_name) / 2) \
            tolin_name \
            ((height tolin - height tolin_name) / 2) \
	    4 \
            ((height angspc - height angspc_name) / 2) \
            angspc_name \
            ((height angspc - height angspc_name) / 2) \
            4 \
            ((height angtol - height angtol_name) / 2) \
            angtol_name \
            ((height angtol - height angtol_name) / 2) \
            4 \
            ((height dmin - height dmin_name) / 2) \
            dmin_name \
            ((height dmin - height dmin_name) / 2) \
            4 \
        } \
        1 \
        vertical { \
            4 \
            tolin \
            4 \
            angspc \
            4 \
            angtol \
            4 \
            dmin \
            4 \
        } \
        4 \
        vertical { \
            4 \
            ((height kappa - height kappa_name) / 2) \
            kappa_name \
            ((height kappa - height kappa_name) / 2) \
            4 \
            ((height min - height min_name) / 2) \
            min_name \
            ((height min - height min_name) / 2) \
            4 \
            ((height max - height max_name) / 2) \
            max_name \
            ((height max - height max_name) / 2) \
            4 \
            ((height nholes - height nholes_name) / 2) \
            nholes_name \
            ((height nholes - height nholes_name) / 2) \
            4\
        } \
        1 \
        vertical { \
            4 \
            kappa \
            4 \
            min \
            4 \
            max \
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

static void Action (w, event, params, num_params)
    Widget    w;
    XEvent   *event;
    String   *params;
    Cardinal *num_params;
{
    w = XtNameToWidget (XtParent (w), params [0]);

    XtCallCallbacks (w, XtNcallback, NULL);
}


/************************************************************************
 * Function:	Okay							*
 *									*
 * Description:								*
 ************************************************************************/

static void Okay (w, client_data, call_data)
    Widget    w;
    XtPointer client_data;
    XtPointer call_data;
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

static void Cancel (w, client_data, call_data)
    Widget    w;
    XtPointer client_data;
    XtPointer call_data;
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

static void TrimeshDialogSet (trimeshd)
    TrimeshDialog	trimeshd;
{
    Arg		args [1];
    String	value;
    char	buffer [80];
    int		numholes;

    XtSetArg (args [0], XtNstring, &value);

    XtGetValues (trimeshd -> tolin, args, 1); 
    trimeshd -> tm -> tolin = exptod (value, NULL);
    sprintf (buffer, (trimeshd -> tm -> tolin ? "%g" : "0.0"), 
             trimeshd -> tm -> tolin);
    SetTextString (trimeshd -> tolin, buffer);

    XtGetValues (trimeshd -> angspc, args, 1); 
    trimeshd -> tm -> angspc = exptod (value, NULL);
    sprintf (buffer, (trimeshd -> tm -> angspc ? "%g" : "0.0"), 
             trimeshd -> tm -> angspc);
    SetTextString (trimeshd -> angspc, buffer);

    XtGetValues (trimeshd -> angtol, args, 1); 
    trimeshd -> tm -> angtol = exptod (value, NULL);
    sprintf (buffer, (trimeshd -> tm -> angtol ? "%g" : "0.0"), 
             trimeshd -> tm -> angtol);
    SetTextString (trimeshd -> angtol, buffer);

    XtGetValues (trimeshd -> dmin, args, 1); 
    trimeshd -> tm -> dmin = exptod (value, NULL);
    sprintf (buffer, (trimeshd -> tm -> dmin ? "%g" : "0.0"), 
             trimeshd -> tm -> dmin);
    SetTextString (trimeshd -> dmin, buffer);

    XtGetValues (trimeshd -> kappa, args, 1); 
    trimeshd -> tm -> kappa = exptod (value, NULL);
    sprintf (buffer, (trimeshd -> tm -> kappa ? "%g" : "0.0"), 
             trimeshd -> tm -> kappa);
    SetTextString (trimeshd -> kappa, buffer);

    XtGetValues (trimeshd -> min, args, 1); 
    trimeshd -> tm -> min = atoi (value);
    sprintf (buffer, (trimeshd -> tm -> min ? "%d" : "0"), 
             trimeshd -> tm -> min);
    SetTextString (trimeshd -> min, buffer);

    XtGetValues (trimeshd -> max, args, 1); 
    trimeshd -> tm -> max = atoi (value);
    sprintf (buffer, (trimeshd -> tm -> max ? "%d" : "0"), 
             trimeshd -> tm -> max);
    SetTextString (trimeshd -> max, buffer);

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

TrimeshDialog TrimeshDialogCreate (parent, name, title)
    Widget parent;
    String name;
    String title;
{
    Cardinal		i;
    Arg			args [1];
    Widget		group [11];
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

    trimeshd -> tolin  = XtCreateManagedWidget ("tolin",
			 asciiTextWidgetClass, trimeshd -> layout,
			 text_args, XtNumber (text_args));

    trimeshd -> angspc  = XtCreateManagedWidget ("angspc",
			 asciiTextWidgetClass, trimeshd -> layout,
			 text_args, XtNumber (text_args));

    trimeshd -> angtol  = XtCreateManagedWidget ("angtol",
			 asciiTextWidgetClass, trimeshd -> layout,
			 text_args, XtNumber (text_args));

    trimeshd -> dmin = XtCreateManagedWidget ("dmin",
			 asciiTextWidgetClass, trimeshd -> layout,
			 text_args, XtNumber (text_args));

    trimeshd -> kappa = XtCreateManagedWidget ("kappa",
			 asciiTextWidgetClass, trimeshd -> layout,
			 text_args, XtNumber (text_args));

    trimeshd -> min = XtCreateManagedWidget ("min",
			 asciiTextWidgetClass, trimeshd -> layout,
			 text_args, XtNumber (text_args));

    trimeshd -> max = XtCreateManagedWidget ("max",
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

    SetTextString (trimeshd -> tolin, "0.0");
    SetTextString (trimeshd -> angspc, "30.0");
    SetTextString (trimeshd -> angtol, "20.0");
    SetTextString (trimeshd -> dmin, "0.5");
    SetTextString (trimeshd -> kappa, "0.25");
    SetTextString (trimeshd -> min, "50");
    SetTextString (trimeshd -> max, "100");
    SetTextString (trimeshd -> nholes, "0");


    /* Create a tab group for the material dialog. */

    group [0]  = trimeshd -> tolin;
    group [1]  = trimeshd -> angspc;
    group [2]  = trimeshd -> angtol;
    group [3]  = trimeshd -> dmin;
    group [4]  = trimeshd -> kappa;
    group [5]  = trimeshd -> min;
    group [6]  = trimeshd -> max;
    group [7]  = trimeshd -> nholes;
    group [8]  = trimeshd -> help;
    group [9]  = trimeshd -> okay;
    group [10] = trimeshd -> cancel;

    XtGetValues (trimeshd -> layout, color_args, XtNumber (color_args));
    CreateTabGroup (trimeshd -> shell, group, XtNumber (group), 
                    highlight, True);

    XtRealizeWidget (trimeshd -> shell);
    SetFocus (trimeshd -> tolin);

    XtSetArg (args [0], XtNwidth, &width);
    XtGetValues (trimeshd -> layout, args, 1);
    XtSetArg (args [0], XtNx, &x);
    XtGetValues (trimeshd -> help, args, 1);
    UpdateHelpMessage (trimeshd -> help, help_message, width - 2 * x);


    /* Add the translations to each widget. */

    AddDeleteWindowProtocol (trimeshd -> shell, "TrimeshDialogAction()");

    XtOverrideTranslations (trimeshd -> tolin,	text_translations);
    XtOverrideTranslations (trimeshd -> angspc,	text_translations);
    XtOverrideTranslations (trimeshd -> angtol,	text_translations);
    XtOverrideTranslations (trimeshd -> dmin,	text_translations);
    XtOverrideTranslations (trimeshd -> kappa,	text_translations);
    XtOverrideTranslations (trimeshd -> min,	text_translations);
    XtOverrideTranslations (trimeshd -> max,	text_translations);
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

TriMesh TrimeshDialogPopup (trimeshd)
    TrimeshDialog trimeshd;
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
