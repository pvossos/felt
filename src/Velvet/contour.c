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
 * File:	contour.c						*
 *									*
 * Description:	This file contains the private and public function and	*
 *		type definitions for the contour dialog box.		*
 ************************************************************************/

# include <stdio.h>
# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <X11/Shell.h>
# include <X11/Xaw/AsciiText.h>
# include <X11/Xaw/Command.h>
# include <X11/Xaw/Toggle.h>
# include <X11/Xaw/Label.h>
# include "Layout.h"
# include "Solution.h"
# include "Contour.h"
# include "TabGroup.h"
# include "util.h"
# include "problem.h"
# include "procedures.h"

# ifndef X_NOT_STDC_ENV
# include <stdlib.h>
# else
extern int    atoi ( );
# endif

Solution	solution;

struct contour_dialog {
    Widget  shell;		/* topLevelShell  <specified>	*/
    Widget  layout;		/*	Layout  layout		*/
    Widget  s_component;	/*	     AsciiText s_compon	*/
    Widget  d_component;	/*	     AsciiText d_compon	*/
    Widget  s_equalize;		/*	     Toggle s_equalize	*/
    Widget  s_plot_elt;		/*           Toggle s_plot_elt	*/
    Widget  d_equalize;		/*	     Toggle d_equalize	*/
    Widget  d_plot_elt;		/*           Toggle d_plot_elt	*/
    Widget  s_plot;		/* 	     Command s_plot	*/
    Widget  d_plot;		/* 	     Command d_plot	*/
    Widget  colors;		/*	     Toggle colors	*/
    Widget  lines;		/*	     Toggle lines	*/
    Widget  help;		/*	     MenuButton  help	*/
    Widget  accept;		/*	     Command  accept	*/
    Widget  dismiss;		/*	     Command  dismiss	*/
};


static String label_names [ ] = {
   "stresses_name", "displacements_name", "s_eq_name", "d_eq_name",
   "s_comp_name", "d_comp_name", "s_plot_name", "d_plot_name",
   "color_name", "line_name"
};

static String labels [ ] = {
   "Stresses:", "Displacements:", "equalize", "equalize",
   "component:", "component:", "overlay elements", "overlay elements",
   "Color contours", "Line contours"
};


/* Resources */

static Pixel highlight;

static char layout_string [ ] =
"vertical { \
    horizontal { \
       vertical { \
          8 \
          horizontal { \
             8 \
             ((width s_plot_name + width s_plot - width s_plot_name) / 2) \
             stresses_name \
             ((width s_plot_name + width s_plot - width s_plot_name) / 2) \
             8 \
          } \
          8 \
          horizontal { \
             8 \
             s_comp_name 2 s_component \
             8 \
          } \
          4 \
          horizontal { \
             8 \
             s_equalize 2 s_eq_name \
             8 \
          } \
          4 \
          horizontal { \
             8 \
             s_plot_elt 2 s_plot_name \
             8 \
          } \
          8 \
       } \
       separator1 <* +inf -100%> \
       vertical { \
          8 \
          horizontal { \
             8 \
             ((width d_plot_name + width d_plot - width d_plot_name) / 2) \
             displacements_name \
             ((width d_plot_name + width d_plot - width d_plot_name) / 2) \
             8 \
          } \
          8 \
          horizontal { \
             8 \
             d_comp_name 2 d_component \
             8 \
          } \
          4 \
          horizontal { \
             8 \
             d_equalize 2 d_eq_name \
             8 \
          } \
          4 \
          horizontal { \
             8 \
             d_plot_elt 2 d_plot_name \
             8 \
          } \
          8 \
       } \
    } \
    separator2 <+inf -100% *> \
    4 \
    horizontal { \
        4 \
        colors \
        2 \
        color_name \
        4 <+inf -100%> \
        lines \
        2 \
        line_name \
        4 \
    } \
    separator3 <+inf -100% *> \
    4 \
    horizontal { \
	4 \
        help \
        4 <+inf -100%> \
        s_plot \
        4 <+inf -100%> \
        d_plot \
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

static Arg component_args [ ] = {
    {XtNeditType,    (XtArgVal) XawtextEdit},
    {XtNborderWidth, (XtArgVal) 0},
    {XtNpieceSize,   (XtArgVal) 32},
    {XtNcursorName,  (XtArgVal) "left_ptr"},
    {XtNwidth,       (XtArgVal) 22},
};

static Arg label_args [ ] = {
    {XtNlabel,	            (XtArgVal) ""},
    {XtNborderWidth,        (XtArgVal) 0},
};

static Arg core_args [ ] = {
    {XtNwidth,	(XtArgVal) 3},
    {XtNheight, (XtArgVal) 3},
};

static Arg toggle_args [ ] = {
    {XtNlabel,		    (XtArgVal) " "},
};

/* Translation tables */

static String text_table =
"<Key>Return: ContourDialogAction(accept)\n\
 <Key>Escape: ContourDialogAction(dismiss)\n\
 Ctrl<Key>h:  ContourDialogAction(help)\n\
 <Btn1Down>:  SetFocus() select-start()";

static XtTranslations text_translations;

static String command_table =
"<Key>Return:   AutoRepeat(off) set()\n\
 <KeyUp>Return: AutoRepeat(saved) notify() unset()\n\
 <Key>space:    AutoRepeat(off) set()\n\
 <KeyUp>space:  AutoRepeat(saved) notify() unset()\n\
 <Key>Escape:   ContourDialogAction(dismiss)";

static XtTranslations command_translations;

static String toggle_table =
"<Key>Return:	ContourDialogAction(accept)\n\
 <Key>Escape:	ContourDialogAction(dismiss)\n\
 <Key>space:	toggle() notify()\n\
 Ctrl<Key>h:	ContourDialogAction(help)";

static XtTranslations toggle_translations;

static String help_table =
"<Key>Return: ContourDialogAction(accept)\n\
 <Key>Escape: ContourDialogAction(dismiss)\n\
 Ctrl<Key>h:  ContourDialogAction(help)\n\
 <Key>space:  PostMenu()";

static XtTranslations help_translations;

/* Help message. */

static String help_message ="\
The contouring controls allow you to define just how color contour plots \
are generated.  The component should be one of the appropriate stress or \
displacement components for this problem (i.e., for CST elements, the stress \
component can be a value from 1-6 and the displacement component can be \
from 1-2).  The equalize toggles control histogram equalization of the \
The overlay elements option allows you to specify whether or not \
the resulting image should have the element outlines drawn on top of the \
image.  The 's plot' and 'd plot' buttons are equivalent to pushing accept \
and then choosing either plot stresses or plot displacements from the main \
Postprocessing menu. \
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
 * Function:	Toggle							*
 *									*
 * Description:								*
 ************************************************************************/

static void Toggle (w, client_data, call_data)
    Widget    w;
    XtPointer client_data;
    XtPointer call_data;
{
    Arg		   args [1];
    ContourDialog  contourd;
    Boolean	   state;
 
    contourd = (ContourDialog) client_data;

    XtSetArg (args [0], XtNstate, &state);
    XtGetValues (w, args, 1);

    if (state) {
       XtSetArg (args [0], XtNstate, False);
       if (w == contourd -> colors)
          XtSetValues (contourd -> lines, args, 1);
       else 
          XtSetValues (contourd -> colors, args, 1);
    }
    else {
       XtSetArg (args [0], XtNstate, True);
       XtSetValues (w, args, 1);
    }
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
    ContourDialog contourd;
    String	   value;
    Boolean	   state;

    contourd = (ContourDialog) client_data;

    XtSetArg (args [0], XtNstring,  &value);

    XtGetValues (contourd -> s_component, args, 1);
    solution -> s_component = atoi (value);
    XtGetValues (contourd -> d_component, args, 1);
    solution -> d_component = atoi (value);

    XtSetArg (args [0], XtNstate, &state);

    XtGetValues (contourd -> s_equalize, args, 1);
    solution -> s_equalize = state;
    XtGetValues (contourd -> d_equalize, args, 1);
    solution -> d_equalize = state;

    XtGetValues (contourd -> s_plot_elt, args, 1);
    solution -> s_plot_elt = state;
    XtGetValues (contourd -> d_plot_elt, args, 1);
    solution -> d_plot_elt = state;

    XtGetValues (contourd -> colors, args, 1);
    if (state)
       solution -> contours = 'c';
    else
       solution -> contours = 'l';

    ContourDialogUpdate (contourd);
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
    ContourDialog contourd;

    contourd = (ContourDialog) client_data;
    XtPopdown (contourd -> shell);
}

/************************************************************************
 ************************************************************************/

static void ExternalCallback (w, client_data, call_data)
    Widget 	w;
    XtPointer	client_data;
    XtPointer	call_data;
{
    ContourDialog	contourd;

    contourd = (ContourDialog) client_data;

    ContourDialogUpdate (contourd);

    if (w == contourd -> s_plot) 
       SetupStresses (True);
    else if (w == contourd -> d_plot)
       SetupDisplacements (True);
}

/************************************************************************
 * Function:	ContourDialogUpdate					*
 *									*
 * Description:	sets the widgets based on current solution settings 	*
 ************************************************************************/

void ContourDialogUpdate (contourd)
    ContourDialog	contourd;
{
    Arg		args [1];
    char	buffer [80];

    sprintf (buffer,"%d",solution -> s_component);
    SetTextString (contourd -> s_component, buffer);
    sprintf (buffer,"%d",solution -> d_component);
    SetTextString (contourd -> d_component, buffer);

    XtSetArg (args [0], XtNstate, solution -> s_equalize);
    XtSetValues (contourd -> s_equalize, args, 1);
    XtSetArg (args [0], XtNstate, solution -> d_equalize);
    XtSetValues (contourd -> d_equalize, args, 1);

    XtSetArg (args [0], XtNstate, solution -> s_plot_elt);
    XtSetValues (contourd -> s_plot_elt, args, 1);
    XtSetArg (args [0], XtNstate, solution -> d_plot_elt);
    XtSetValues (contourd -> d_plot_elt, args, 1);

    XtSetArg (args [0], XtNstate, True);
    if (solution -> contours == 'c')
       XtSetValues (contourd -> colors, args, 1);
    else
       XtSetValues (contourd -> lines, args, 1);
}

/************************************************************************
 * Function:	ContourDialogCreate					*
 *									*
 * Description:	Creates a new solution dialog.  You would never want to	*
 *		have more than one of these but the interface is kept	*
 *		consistent with those of the other dialogs.		*
 ************************************************************************/

ContourDialog ContourDialogCreate (parent, name, title)
    Widget parent;
    String name;
    String title;
{
    Cardinal		i;
    Arg			args [1];
    Widget		group [13];
    ContourDialog	contourd;
    Dimension		width;
    Position		x;
    int			depth;
    static XtAppContext	app_context = NULL;
    static XtActionsRec	actions [ ] = {{"ContourDialogAction", Action}};


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

    contourd = XtNew (struct contour_dialog);

    contourd -> shell    = XtCreatePopupShell (name,
			 topLevelShellWidgetClass, parent,
			 shell_args, XtNumber (shell_args));

    contourd -> layout   = XtCreateManagedWidget ("layout",
			 layoutWidgetClass, contourd -> shell,
			 layout_args, XtNumber (layout_args));

    contourd -> s_plot_elt = XtCreateManagedWidget ("s_plot_elt",
                         toggleWidgetClass, contourd -> layout,
                         toggle_args, XtNumber (toggle_args));

    contourd -> d_plot_elt = XtCreateManagedWidget ("d_plot_elt",
                         toggleWidgetClass, contourd -> layout,
                         toggle_args, XtNumber (toggle_args));

    contourd -> s_component = XtCreateManagedWidget ("s_component",
                         asciiTextWidgetClass, contourd -> layout,
                         component_args, XtNumber (component_args));

    contourd -> d_component = XtCreateManagedWidget ("d_component",
                         asciiTextWidgetClass, contourd -> layout,
                         component_args, XtNumber (component_args));

    contourd -> s_equalize = XtCreateManagedWidget ("s_equalize",
                         toggleWidgetClass, contourd -> layout,
                         toggle_args, XtNumber (toggle_args));

    contourd -> d_equalize = XtCreateManagedWidget ("d_equalize",
                         toggleWidgetClass, contourd -> layout,
                         toggle_args, XtNumber (toggle_args));

    contourd -> lines	   = XtCreateManagedWidget ("lines",
                         toggleWidgetClass, contourd -> layout,
                         toggle_args, XtNumber (toggle_args));

    contourd -> colors	   = XtCreateManagedWidget ("colors",
                         toggleWidgetClass, contourd -> layout,
                         toggle_args, XtNumber (toggle_args));

    contourd -> accept   = XtCreateManagedWidget ("accept",
			 commandWidgetClass, contourd -> layout,
			 NULL, 0);

    XtSetArg (args [0], XtNlabel, "s plot");
    contourd -> s_plot   = XtCreateManagedWidget ("s_plot",
			 commandWidgetClass, contourd -> layout,
			 args, 1);

    XtSetArg (args [0], XtNlabel, "d plot");
    contourd -> d_plot   = XtCreateManagedWidget ("d_plot",
			 commandWidgetClass, contourd -> layout,
			 args, 1);

    contourd -> dismiss  = XtCreateManagedWidget ("dismiss",
			 commandWidgetClass, contourd -> layout,
			 NULL, 0);

    contourd -> help     = CreateHelpButton (contourd -> layout, "help");

    for (i = 0 ; i < XtNumber (labels) ; i++) {
        label_args [0].value = (XtArgVal) labels [i];
        XtCreateManagedWidget (label_names [i], labelWidgetClass,
                   contourd -> layout, label_args, XtNumber (label_args));
    }

    XtCreateManagedWidget ("separator1", coreWidgetClass, 
               contourd -> layout, core_args, XtNumber (core_args));
    XtCreateManagedWidget ("separator2", coreWidgetClass, 
               contourd -> layout, core_args, XtNumber (core_args));
    XtCreateManagedWidget ("separator3", coreWidgetClass, 
               contourd -> layout, core_args, XtNumber (core_args));

    /* Create a tab group for the solution dialog. */

    i = 0;
    group [i++]  = contourd -> s_component;
    group [i++]  = contourd -> s_equalize;
    group [i++]  = contourd -> s_plot_elt;
    group [i++]  = contourd -> d_component;
    group [i++]  = contourd -> d_equalize;
    group [i++]  = contourd -> d_plot_elt;
    group [i++]  = contourd -> colors;
    group [i++]  = contourd -> lines;
    group [i++] = contourd -> help;
    group [i++] = contourd -> s_plot;
    group [i++] = contourd -> d_plot;
    group [i++] = contourd -> accept;
    group [i++] = contourd -> dismiss;

    XtGetValues (contourd -> layout, color_args, XtNumber (color_args));
    CreateTabGroup (contourd -> shell, group, XtNumber (group), 
                    highlight, True);

    XtRealizeWidget (contourd -> shell);
    SetFocus (contourd -> s_component);

    XtSetArg (args [0], XtNwidth, &width);
    XtGetValues (contourd -> layout, args, 1);
    XtSetArg (args [0], XtNx, &x);
    XtGetValues (contourd -> help, args, 1);
    UpdateHelpMessage (contourd -> help, help_message, width - 2 * x + 80);


    /* Add the translations to each widget. */

    AddDeleteWindowProtocol   (contourd -> shell, "ContourDialogAction()");

    XtOverrideTranslations (contourd -> s_component, text_translations);
    XtOverrideTranslations (contourd -> d_component, text_translations);
    XtOverrideTranslations (contourd -> s_plot_elt,  toggle_translations);
    XtOverrideTranslations (contourd -> d_plot_elt,  toggle_translations);
    XtOverrideTranslations (contourd -> s_equalize,  toggle_translations);
    XtOverrideTranslations (contourd -> d_equalize,  toggle_translations);
    XtOverrideTranslations (contourd -> colors,  toggle_translations);
    XtOverrideTranslations (contourd -> lines,  toggle_translations);
    XtOverrideTranslations (contourd -> s_plot,       command_translations);
    XtOverrideTranslations (contourd -> d_plot,       command_translations);
    XtOverrideTranslations (contourd -> dismiss,      command_translations);
    XtOverrideTranslations (contourd -> accept,       command_translations);
    XtOverrideTranslations (contourd -> help,	      help_translations);


    /* Add the necessary callbacks. */

    XtAddCallback(contourd->accept, XtNcallback,Accept, (XtPointer) contourd);
    XtAddCallback(contourd->dismiss,XtNcallback,Dismiss,(XtPointer) contourd);

    XtAddCallback (contourd -> s_plot, XtNcallback, 
                   Accept, (XtPointer)contourd);
    XtAddCallback (contourd -> d_plot, XtNcallback, 
                   Accept, (XtPointer)contourd);

    XtAddCallback (contourd -> s_plot, XtNcallback, 
                   ExternalCallback, (XtPointer) contourd);
    XtAddCallback (contourd -> d_plot, XtNcallback, 
                   ExternalCallback, (XtPointer) contourd);

    XtAddCallback (contourd -> colors, XtNcallback, Toggle, (XtPointer) contourd);
    XtAddCallback (contourd -> lines, XtNcallback, Toggle, (XtPointer) contourd);

	/*
	 * a check to see if the current display is only 1-bit deep,
	 * if so, color contours are a non-no
	 */

    XtSetArg (args [0], XtNdepth, &depth);
    XtGetValues (parent, args, 1);
    if (depth != 8) {
        XtSetSensitive (contourd -> s_equalize, False);
        XtSetSensitive (contourd -> d_equalize, False);
        XtSetSensitive (contourd -> s_plot_elt, False);
        XtSetSensitive (contourd -> d_plot_elt, False);
/*
        XtSetSensitive (contourd -> s_component, False);
        XtSetSensitive (contourd -> d_component, False);
        XtSetSensitive (contourd -> s_plot, False);
        XtSetSensitive (contourd -> d_plot, False);
*/

        XtSetSensitive (contourd -> colors, False);
        solution -> contours = 'l';

        solution -> d_equalize = False;
        solution -> s_equalize = False;
        solution -> d_plot_elt = False;
        solution -> s_plot_elt = False;
    }
    else {
        solution -> contours = 'c';

        solution -> d_equalize = True;
        solution -> s_equalize = True;
        solution -> d_plot_elt = True;
        solution -> s_plot_elt = True;
    }
       

    return contourd;
}


/************************************************************************
 * Function:	ContourDialogPopup					*
 *									*
 * Description:	Pops up the specified material dialog.			*
 ************************************************************************/

void ContourDialogPopup (contourd)
    ContourDialog contourd;
{
    XtPopup (contourd -> shell, XtGrabNone);
}
