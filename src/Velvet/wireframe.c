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
 * File:	wireframe.c						*
 *									*
 * Description:	This file contains the private and public function and	*
 *		type definitions for the wireframe dialog box.		*
 ************************************************************************/

# include <stdio.h>
# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <X11/Shell.h>
# include <X11/Xaw/AsciiText.h>
# include <X11/Xaw/Command.h>
# include <X11/Xaw/Toggle.h>
# include <X11/Xaw/Label.h>
# include "Rheostat.h"
# include "Layout.h"
# include "Solution.h"
# include "Wireframe.h"
# include "TabGroup.h"
# include "util.h"
# include "problem.h"
# include "procedures.h"

struct wireframe_dialog {
    Widget  shell;		/* topLevelShell  <specified>	*/
    Widget  layout;		/*	Layout  layout		*/
    Widget  xrot;		/*	     Rheostat xrot	*/
    Widget  yrot;		/*	     Rheostat yrot	*/
    Widget  zrot;		/*	     Rheostat zrot	*/
    Widget  zscale;		/*	     AsciiText zscale	*/
    Widget  magnify;		/*	     AsciiText magnify	*/
    Widget  hlhsr;		/*	     Toggle hlhsr	*/
    Widget  plot_orig;		/*	     Toggle plot_orig	*/
    Widget  plot;		/* 	     Command plot	*/
    Widget  help;		/*	     MenuButton  help	*/
    Widget  accept;		/*	     Command  accept	*/
    Widget  dismiss;		/*	     Command  dismiss	*/
};


static String labels [ ] = {
    "x-rot","y-rot","z-rot",
    "z-scale:", "magnify:","remove hidden lines", "plot original shape"
};

static String label_names [ ] = {
    "xrot_name", "yrot_name","zrot_name",
    "zscale_name", "magnify_name", "hlhsr_name", "plot_orig_name"
};


/* Resources */

static Pixel highlight;

static char layout_string [ ] =
"vertical { \
    8 \
    horizontal { \
       4 \
       xrot \
       4 <+inf -100%> \
       yrot \
       4 <+inf -100%> \
       zrot \
       4 \
   } \
   1 \
   horizontal { \
       4 \
       ((width xrot - width xrot_name) / 2) \
       xrot_name \
       ((width xrot - width xrot_name) / 2) \
       4 \
       ((width yrot - width yrot_name) / 2) \
       yrot_name \
       ((width yrot - width yrot_name) / 2) \
       4 \
       ((width zrot - width zrot_name) / 2) \
       zrot_name \
       ((width zrot - width zrot_name) / 2) \
       4 \
   } \
   4 \
   horizontal { \
       4 \
       zscale_name \
       2  + (width magnify_name - width zscale_name) \
       zscale \
   } \
   4 \
   horizontal { \
       4 \
       magnify_name 2 magnify \
   } \
   4 \
   horizontal { \
       4 \
       hlhsr 2 hlhsr_name \
   } \
   4 \
   horizontal { \
       4 \
       plot_orig 2 plot_orig_name \
   } \
   4 \
   separator1 <+inf -100% *> \
   4 \
   horizontal { \
      4 \
      help \
      4 <+inf -100%> \
      plot \
      4 <+inf -100%> \
      accept \
      4 <+inf -100%> \
      dismiss \
      4 \
   } \
   4 \
}";

static Arg color_args [ ] = {
    {XtNborderColor, (XtArgVal) &highlight},
};

static Arg shell_args [ ] = {
    {XtNtitle, (XtArgVal) NULL},
};

static Arg layout_args [ ] = {
    {XtNlayout, (XtArgVal) NULL},
};

static Arg text_args [ ] = {
    {XtNeditType,    (XtArgVal) XawtextEdit},
    {XtNborderWidth, (XtArgVal) 0},
    {XtNpieceSize,   (XtArgVal) 32},
    {XtNcursorName,  (XtArgVal) "left_ptr"},
};

static Arg label_args [ ] = {
    {XtNlabel,	            (XtArgVal) ""},
    {XtNborderWidth,        (XtArgVal) 0},
};

static Arg core_args [ ] = {
    {XtNwidth,	(XtArgVal) 3},
    {XtNheight, (XtArgVal) 3},
};

static Arg rheostat_args [ ] = {
    {XtNminimumAngle, (XtArgVal) 0},
    {XtNmaximumAngle, (XtArgVal) 360},
    {XtNminimumValue, (XtArgVal) 0},
    {XtNmaximumValue, (XtArgVal) 360},
    {XtNradius,       (XtArgVal) 18},
    {XtNtickGravity,  (XtArgVal) True},
};

static Arg toggle_args [ ] = {
    {XtNlabel,		    (XtArgVal) " "},
};

/* Translation tables */

static String text_table =
"<Key>Return: WireframeDialogAction(accept)\n\
 <Key>Escape: WireframeDialogAction(dismiss)\n\
 Ctrl<Key>h:  WireframeDialogAction(help)\n\
 <Btn1Down>:  SetFocus() select-start()";

static XtTranslations text_translations;

static String command_table =
"<Key>Return:   AutoRepeat(off) set()\n\
 <KeyUp>Return: AutoRepeat(saved) notify() unset()\n\
 <Key>space:    AutoRepeat(off) set()\n\
 <KeyUp>space:  AutoRepeat(saved) notify() unset()\n\
 <Key>Escape:   WireframeDialogAction(dismiss)";

static XtTranslations command_translations;

static String toggle_table =
"<Key>Return:	WireframeDialogAction(accept)\n\
 <Key>Escape:	WireframeDialogAction(dismiss)\n\
 <Key>space:	toggle() notify()\n\
 Ctrl<Key>h:	WireframeDialogAction(help)";

static XtTranslations toggle_translations;

static String help_table =
"<Key>Return: WireframeDialogAction(accept)\n\
 <Key>Escape: WireframeDialogAction(dismiss)\n\
 Ctrl<Key>h:  WireframeDialogAction(help)\n\
 <Key>space:  PostMenu()";

static XtTranslations help_translations;

/* Help message. */

static String help_message ="\
Wireframe plots are simple two- or three-dimensional line drawings of the \
displaced shape of your structure.  The magnification factor specifies the \
scaling of the nodal displacements.  The three dials and the z-scale \
parameter control the viewing for three-dimensional drawing.  If the toggle \
is checked, the original shape of the structure will also be drawn with \
a dashed line.  Clicking the plot button is equivalent to pushing accept and \
then choosing plot structure from the main Postprocessing menu.\
";


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
    if (XtClass (w) == topLevelShellWidgetClass)
	w = XtNameToWidget (w, "layout.dismiss");
    else
	w = XtNameToWidget (XtParent (w), params [0]);

    XtCallCallbacks (w, XtNcallback, NULL);
}


/************************************************************************
 * Function:	Accept							*
 *									*
 * Description:								*
 ************************************************************************/

static void Accept (w, client_data, call_data)
    Widget    w;
    XtPointer client_data;
    XtPointer call_data;
{
    Arg		   args [1];
    WireframeDialog wireframed;
    String	   value;
    Boolean	   state;
    int		   angle;

    wireframed = (WireframeDialog) client_data;

    XtSetArg (args [0], XtNstring,  &value);

    XtGetValues (wireframed -> zscale, args, 1);
    solution -> zscale = exptod (value, NULL);
    XtGetValues (wireframed -> magnify, args, 1);
    solution -> magnify = exptod (value, NULL);

    XtSetArg (args [0], XtNstate, &state);

    XtGetValues (wireframed -> hlhsr, args, 1);
    solution -> hlhsr = state;
    XtGetValues (wireframed -> plot_orig, args, 1);
    solution -> plot_orig = state;

    XtSetArg (args [0], XtNvalue, &angle);
   
    XtGetValues (wireframed -> xrot, args, 1);
    solution -> xrot = (float) angle;
    XtGetValues (wireframed -> yrot, args, 1);
    solution -> yrot = (float) angle;
    XtGetValues (wireframed -> zrot, args, 1);
    solution -> zrot = (float) angle;
   
    WireframeDialogUpdate (wireframed);
}

/************************************************************************
 * Function:	Dismiss							*
 *									*
 * Description:	sets the dismiss flag					*
 ************************************************************************/

static void Dismiss (w, client_data, call_data)
    Widget    w;
    XtPointer client_data;
    XtPointer call_data;
{
    WireframeDialog wireframed;

    wireframed = (WireframeDialog) client_data;
    XtPopdown (wireframed -> shell);
}

/************************************************************************
 ************************************************************************/

static void ExternalCallback (w, client_data, call_data)
    Widget 	w;
    XtPointer	client_data;
    XtPointer	call_data;
{
    WireframeDialog	wireframed;

    wireframed = (WireframeDialog) client_data;

    WireframeDialogUpdate (wireframed);

    if (w == wireframed -> plot) 
       SetupStructure (True);

    return;
}

/************************************************************************
 * Function:	WireframeDialogUpdate					*
 *									*
 * Description:	sets the widgets based on current solution settings 	*
 ************************************************************************/

void WireframeDialogUpdate (wireframed)
    WireframeDialog	wireframed;
{
    Arg		args [1];
    char	buffer [80];

    sprintf (buffer,"%g",solution -> zscale);
    SetTextString (wireframed -> zscale, buffer);
    sprintf (buffer,"%g",solution -> magnify);
    SetTextString (wireframed -> magnify, buffer);

    XtSetArg (args [0], XtNstate, solution -> hlhsr);
    XtSetValues (wireframed -> hlhsr, args, 1);
    XtSetArg (args [0], XtNstate, solution -> plot_orig);
    XtSetValues (wireframed -> plot_orig, args, 1);

    XtSetArg (args [0], XtNvalue, (int) solution -> xrot);
    XtSetValues (wireframed -> xrot, args, 1);
    XtSetArg (args [0], XtNvalue, (int) solution -> yrot);     
    XtSetValues (wireframed -> yrot, args, 1);
    XtSetArg (args [0], XtNvalue, (int) solution -> zrot);     
    XtSetValues (wireframed -> zrot, args, 1);
}

/************************************************************************
 * Function:	WireframeDialogCreate					*
 *									*
 * Description:	Creates a new wireframe dialog.  You would never want to	*
 *		have more than one of these but the interface is kept	*
 *		consistent with those of the other dialogs.		*
 ************************************************************************/

WireframeDialog WireframeDialogCreate (parent, name, title)
    Widget parent;
    String name;
    String title;
{
    Cardinal		i;
    Arg			args [1];
    Widget		group [11];
    WireframeDialog	wireframed;
    Dimension		width;
    Position		x;
    static XtAppContext	app_context = NULL;
    static XtActionsRec	actions [ ] = {{"WireframeDialogAction", Action}};


    /* Perform one time initialization. */

    if (app_context == NULL) {
	app_context = XtWidgetToApplicationContext (parent);
	XtAppAddActions (app_context, actions, XtNumber (actions));
	AddAutoRepeatAction (app_context);

        layout_args [0].value = StringToLayout (parent, layout_string);

        toggle_translations = XtParseTranslationTable (toggle_table);
	text_translations = XtParseTranslationTable (text_table);
	command_translations = XtParseTranslationTable (command_table);
        help_translations = XtParseTranslationTable (help_table);
    }


    /* Create the material dialog and its widgets. */

    XtSetArg (shell_args [0], XtNtitle, title);

    wireframed = XtNew (struct wireframe_dialog);

    wireframed -> shell    = XtCreatePopupShell (name,
			 topLevelShellWidgetClass, parent,
			 shell_args, XtNumber (shell_args));

    wireframed -> layout   = XtCreateManagedWidget ("layout",
			 layoutWidgetClass, wireframed -> shell,
			 layout_args, XtNumber (layout_args));

    wireframed -> hlhsr = XtCreateManagedWidget ("hlhsr",
                         toggleWidgetClass, wireframed -> layout,
                         toggle_args, XtNumber (toggle_args));

    wireframed -> plot_orig = XtCreateManagedWidget ("plot_orig",
                         toggleWidgetClass, wireframed -> layout,
                         toggle_args, XtNumber (toggle_args));

    wireframed -> xrot     = XtCreateManagedWidget ("xrot",
                         rheostatWidgetClass, wireframed -> layout,
                         rheostat_args, XtNumber (rheostat_args));

    wireframed -> yrot     = XtCreateManagedWidget ("yrot",
                         rheostatWidgetClass, wireframed -> layout,
                         rheostat_args, XtNumber (rheostat_args));

    wireframed -> zrot     = XtCreateManagedWidget ("zrot",
                         rheostatWidgetClass, wireframed -> layout,
                         rheostat_args, XtNumber (rheostat_args));

    wireframed -> zscale  = XtCreateManagedWidget ("zscale",
                         asciiTextWidgetClass, wireframed -> layout,
                         text_args, XtNumber (text_args));

    wireframed -> magnify = XtCreateManagedWidget ("magnify",
                         asciiTextWidgetClass, wireframed -> layout,
                         text_args, XtNumber (text_args));

    wireframed -> accept   = XtCreateManagedWidget ("accept",
			 commandWidgetClass, wireframed -> layout,
			 NULL, 0);

    wireframed -> plot   = XtCreateManagedWidget ("plot",
			 commandWidgetClass, wireframed -> layout,
			 NULL, 0);

    wireframed -> dismiss  = XtCreateManagedWidget ("dismiss",
			 commandWidgetClass, wireframed -> layout,
			 NULL, 0);

    wireframed -> help     = CreateHelpButton (wireframed -> layout, "help");

    for (i = 0 ; i < XtNumber (labels) ; i++) {
        label_args [0].value = (XtArgVal) labels [i];
        XtCreateManagedWidget (label_names [i], labelWidgetClass,
                   wireframed -> layout, label_args, XtNumber (label_args));
    }

    XtCreateManagedWidget ("separator1", coreWidgetClass, 
               wireframed -> layout, core_args, XtNumber (core_args));


    /* Create a tab group for the solution dialog. */

    group [0]  = wireframed -> xrot;
    group [1]  = wireframed -> yrot;
    group [2]  = wireframed -> zrot;
    group [3] = wireframed -> zscale;
    group [4] = wireframed -> magnify;
    group [5] = wireframed -> hlhsr;
    group [6] = wireframed -> plot_orig;
    group [7] = wireframed -> help;
    group [8] = wireframed -> plot;
    group [9] = wireframed -> accept;
    group [10] = wireframed -> dismiss;

    XtGetValues (wireframed -> layout, color_args, XtNumber (color_args));
    CreateTabGroup (wireframed -> shell, group, XtNumber (group), 
                    highlight, True);

    XtRealizeWidget (wireframed -> shell);
    SetFocus (wireframed -> xrot);

    XtSetArg (args [0], XtNwidth, &width);
    XtGetValues (wireframed -> layout, args, 1);
    XtSetArg (args [0], XtNx, &x);
    XtGetValues (wireframed -> help, args, 1);
    UpdateHelpMessage (wireframed -> help, help_message, width - 2 * x + 160);


    /* Add the translations to each widget. */

    AddDeleteWindowProtocol   (wireframed -> shell, "WireframeDialogAction()");

    XtOverrideTranslations (wireframed -> zscale,    text_translations);
    XtOverrideTranslations (wireframed -> magnify,   text_translations);
    XtOverrideTranslations (wireframed -> hlhsr,     toggle_translations);
    XtOverrideTranslations (wireframed -> plot_orig, toggle_translations);
    XtOverrideTranslations (wireframed -> plot,      command_translations);
    XtOverrideTranslations (wireframed -> dismiss,   command_translations);
    XtOverrideTranslations (wireframed -> accept,    command_translations);
    XtOverrideTranslations (wireframed -> help,	     help_translations);


    /* Add the necessary callbacks. */

    XtAddCallback(wireframed->accept, XtNcallback,Accept, (XtPointer) wireframed);
    XtAddCallback(wireframed->dismiss,XtNcallback,Dismiss,(XtPointer) wireframed);

    XtAddCallback (wireframed -> plot, XtNcallback, 
                   Accept, (XtPointer)wireframed);

    XtAddCallback (wireframed -> plot, XtNcallback, 
                   ExternalCallback, (XtPointer) wireframed);

    XtSetSensitive (wireframed -> hlhsr, False);

    return wireframed;
}


/************************************************************************
 * Function:	WireframeDialogPopup					*
 *									*
 * Description:	Pops up the specified material dialog.			*
 ************************************************************************/

void WireframeDialogPopup (wireframed)
    WireframeDialog wireframed;
{
    XtPopup (wireframed -> shell, XtGrabNone);
}
