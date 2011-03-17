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
 * File:	solution.c						*
 *									*
 * Description:	This file contains the private and public function and	*
 *		type definitions for the solution dialog box.		*
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

struct solution_dialog {
    Widget  shell;		/* topLevelShell  <specified>	*/
    Widget  layout;		/*	Layout  layout		*/
    Widget  felt;		/* 	     Toggle felt	*/
    Widget  summary;		/*           Toggle summary	*/
    Widget  debug;		/*	     Toggle debug	*/
    Widget  matrices;		/*	     Toggle matrices	*/
    Widget  details;		/*	     Toggle details	*/
    Widget  plot;		/*           Toggle plot	*/
    Widget  eigen;		/*           Toggle eigen	*/
    Widget  ortho;		/*           Toggle ortho	*/
    Widget  transfer;		/* 	     Toggle transfer    */
    Widget  mode_shapes;	/*           Toggle mode_shapes	*/
    Widget  structure;		/*	     Toggle structure	*/
    Widget  contour_d;		/*	     Toggle contour_d	*/
    Widget  contour_s;		/*	     Toggle contour_s   */
    Widget  solve;		/* 	     Command solve	*/
    Widget  help;		/*	     MenuButton  help	*/
    Widget  accept;		/*	     Command  accept	*/
    Widget  dismiss;		/*	     Command  dismiss	*/
};


static String labels [ ] = {
    "felt tabular", "wireframe structure", 
    "displacement contours", "stress contours", 
    "line plot", "mode shapes", "material summary", 
    "debug", "print matrices", "analysis details",
    "eigen analysis", "transfer functions", "orthonormal mode shapes",
    "Graphical Output:", "Textual Output:",
    "Results:"
};

static String label_names [ ] = {
    "felt_name", "structure_name", 
    "displacements_name", "stresses_name", 
    "plot_name", "shapes_name", "summary_name", 
    "debug_name", "matrices_name", "details_name",
    "eigen_name", "transfer_name", "ortho_name",
    "graphics_name", "text_name",
    "results_name"
};


/* Resources */

static Pixel highlight;

static char layout_string [ ] =
"vertical { \
    horizontal { \
       vertical { \
          4 \
          horizontal { \
             8 \
             ((width ortho_name + width ortho - width results_name) / 2) \
             results_name \
             ((width ortho_name + width ortho - width results_name) / 2) \
             8 \
          } \
          4 \
          horizontal { \
             8 \
             eigen 2 eigen_name \
             8 \
          } \
          4 \
          horizontal { \
             8 \
             ortho 2 ortho_name \
             8 \
          } \
          4 \
          horizontal { \
             8 \
             transfer 2 transfer_name \
             8 \
          } \
          4 \
          separator3 <+inf -100% *> \
          4 \
          horizontal { \
             8 \
             ((width ortho_name + width ortho - width text_name) / 2) \
             text_name \
             ((width ortho_name + width ortho - width text_name) / 2) \
             8 \
          } \
          4 \
          horizontal { \
             8 \
             felt 2 felt_name \
             8 \
          } \
          4 \
          horizontal { \
             8 \
             summary 2 summary_name \
             8 \
          } \
          4 \
          horizontal { \
             8 \
             details 2 details_name \
             8 \
          } \
          4 \
          horizontal { \
             8 \
             debug 2 debug_name \
             8 \
          } \
          4 \
          horizontal { \
             8 \
             matrices 2 matrices_name \
             8 \
          } \
          8 \
       } \
       separator1 <* +inf -100%> \
       vertical { \
          4 \
          horizontal { \
             8 \
             ((width displacements_name + width contour_d - width graphics_name) / 2) \
             graphics_name \
             ((width displacements_name + width contour_d - width graphics_name) / 2) \
             8 \
          } \
          4 \
          horizontal { \
             8 \
             contour_s 2 stresses_name \
             8 \
          } \
          4 \
          horizontal { \
             8 \
             contour_d 2 displacements_name \
             8 \
          } \
          4 \
          horizontal { \
             8 \
             structure 2 structure_name \
             8 \
          } \
          4 \
          horizontal { \
             8 \
             plot 2 plot_name \
             8 \
          } \
          4 \
          horizontal { \
             8 \
             mode_shapes 2 shapes_name \
             8 \
          } \
          8 \
       } \
    } \
    separator2 <+inf -100% *> \
    4 \
    horizontal { \
	4 \
        help \
        4 <+inf -100%> \
        solve \
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

static String command_table =
"<Key>Return:   AutoRepeat(off) set()\n\
 <KeyUp>Return: AutoRepeat(saved) notify() unset()\n\
 <Key>space:    AutoRepeat(off) set()\n\
 <KeyUp>space:  AutoRepeat(saved) notify() unset()\n\
 <Key>Escape:   SolutionDialogAction(dismiss)";

static XtTranslations command_translations;

static String toggle_table =
"<Key>Return:	SolutionDialogAction(accept)\n\
 <Key>Escape:	SolutionDialogAction(dismiss)\n\
 <Key>space:	toggle() notify()\n\
 Ctrl<Key>h:	SolutionDialogAction(help)";

static XtTranslations toggle_translations;

static String help_table =
"<Key>Return: SolutionDialogAction(accept)\n\
 <Key>Escape: SolutionDialogAction(dismiss)\n\
 Ctrl<Key>h:  SolutionDialogAction(help)\n\
 <Key>space:  PostMenu()";

static XtTranslations help_translations;

/* Help message. */

static String help_message ="\
Use these toggles to control what kinds of output are generated whenever \
you solve the problem, and if appropriate for the current analysis type, \
what kind of solution is generated.  Solution controls are available for \
modal analysis (limiting the computations to the eigenvalue problem or \
specifying that orthonormal mode shapes should be used in constructing modal \
matrices) and spectral analysis (limiting the computations to transfer \
functions).  Text based output includes the standard FElt output \
(displacements, stresses, reactions, tabular time-displacement, modal \
information, modal matrices, tabular frequency-power spectra). The text can \
also contain a printout of the global stiffness and/or mass and damping \
matrices, debugging information (what the FElt input file for the problem \
would look like), and a material usage summary. All textual output will \
appear in a single window upon solution. Graphical output can include color \
contour plots of stresses and displacements, plots of the displaced structure, \
time-displacement plots for transient analysis problems, power spectra plots \
for spectral analysis, and mode shape plots for modal analysis problems.";


/************************************************************************
 * Function:	Action							*
 *									*
 * Description:	An action procedure which emulates pressing of the	*
 *		specified button.					*
 ************************************************************************/

static void Action (Widget w, XEvent *event, String *params, Cardinal *num_params)
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

static void Accept (Widget w, XtPointer client_data, XtPointer call_data)
{
    Arg		   args [1];
    SolutionDialog solutiond;
    Boolean	   state;

    solutiond = (SolutionDialog) client_data;

    XtSetArg (args [0], XtNstate, &state);

    XtGetValues (solutiond -> eigen, args, 1);
    solution -> eigen = state;
    XtGetValues (solutiond -> ortho, args, 1);
    solution -> orthonormal = state;
    XtGetValues (solutiond -> transfer, args, 1);
    solution -> transfer = state;
    XtGetValues (solutiond -> felt, args, 1);
    solution -> felt = state;
    XtGetValues (solutiond -> debug, args, 1);
    solution -> debug = state;
    XtGetValues (solutiond -> details, args, 1);
    solution -> details = state;
    XtGetValues (solutiond -> summary, args, 1);
    solution -> summary = state;
    XtGetValues (solutiond -> matrices, args, 1);
    solution -> matrices = state;

    XtGetValues (solutiond -> contour_s, args, 1);
    solution -> stress = state;
    XtGetValues (solutiond -> contour_d, args, 1);
    solution -> displacement = state;
    XtGetValues (solutiond -> structure, args, 1);
    solution -> structure = state;
    XtGetValues (solutiond -> plot, args, 1);
    solution -> plot = state;
    XtGetValues (solutiond -> mode_shapes, args, 1);
    solution -> mode_shapes = state;

    SolutionDialogUpdate (solutiond);
}

/************************************************************************
 * Function:	Dismiss							*
 *									*
 * Description:	sets the dismiss flag					*
 ************************************************************************/

static void Dismiss (Widget w, XtPointer client_data, XtPointer call_data)
{
    SolutionDialog solutiond;

    solutiond = (SolutionDialog) client_data;
    XtPopdown (solutiond -> shell);
}

/************************************************************************
 ************************************************************************/

static void ExternalCallback (Widget w, XtPointer client_data, XtPointer call_data)
{
    SolutionDialog	solutiond;

    solutiond = (SolutionDialog) client_data;

    SolutionDialogUpdate (solutiond);

    if (w == solutiond -> solve) 
       SetupAndSolve ( );

    return;
}

/************************************************************************
 * Function:	SolutionDialogUpdate					*
 *									*
 * Description:	sets the widgets based on current solution settings 	*
 ************************************************************************/

void SolutionDialogUpdate (SolutionDialog solutiond)
{
    Arg		args [1];

    XtSetArg (args [0], XtNstate, solution -> eigen);
    XtSetValues (solutiond -> eigen, args, 1);
    XtSetArg (args [0], XtNstate, solution -> orthonormal);
    XtSetValues (solutiond -> ortho, args, 1);
    XtSetArg (args [0], XtNstate, solution -> transfer);
    XtSetValues (solutiond -> transfer, args, 1);
    XtSetArg (args [0], XtNstate, solution -> felt);
    XtSetValues (solutiond -> felt, args, 1);
    XtSetArg (args [0], XtNstate, solution -> summary);
    XtSetValues (solutiond -> summary, args, 1);
    XtSetArg (args [0], XtNstate, solution -> debug);
    XtSetValues (solutiond -> debug, args, 1);
    XtSetArg (args [0], XtNstate, solution -> details);
    XtSetValues (solutiond -> details, args, 1);
    XtSetArg (args [0], XtNstate, solution -> matrices);
    XtSetValues (solutiond -> matrices, args, 1);

    XtSetArg (args [0], XtNstate, solution -> structure);
    XtSetValues (solutiond -> structure, args, 1);
    XtSetArg (args [0], XtNstate, solution -> stress);
    XtSetValues (solutiond -> contour_s, args, 1);
    XtSetArg (args [0], XtNstate, solution -> displacement);
    XtSetValues (solutiond -> contour_d, args, 1);
    XtSetArg (args [0], XtNstate, solution -> plot);
    XtSetValues (solutiond -> plot, args, 1);
    XtSetArg (args [0], XtNstate, solution -> mode_shapes);
    XtSetValues (solutiond -> mode_shapes, args, 1);

    return;
}

/************************************************************************
 * Function:	SolutionDialogCreate					*
 *									*
 * Description:	Creates a new solution dialog.  You would never want to	*
 *		have more than one of these but the interface is kept	*
 *		consistent with those of the other dialogs.		*
 ************************************************************************/

SolutionDialog SolutionDialogCreate (Widget parent, String name, String title)
{
    Cardinal		i;
    Arg			args [1];
    Widget		group [17];
    SolutionDialog	solutiond;
    Dimension		width;
    Position		x;
    int			depth;
    static XtAppContext	app_context = NULL;
    static XtActionsRec	actions [ ] = {{"SolutionDialogAction", Action}};


    /* Perform one time initialization. */

    if (app_context == NULL) {
	app_context = XtWidgetToApplicationContext (parent);
	XtAppAddActions (app_context, actions, XtNumber (actions));
	AddAutoRepeatAction (app_context);

        layout_args [0].value = StringToLayout (parent, layout_string);

        toggle_translations = XtParseTranslationTable (toggle_table);
	command_translations = XtParseTranslationTable (command_table);
        help_translations = XtParseTranslationTable (help_table);
    }


    /* Create the material dialog and its widgets. */

    XtSetArg (shell_args [0], XtNtitle, title);

    solutiond = XtNew (struct solution_dialog);

    solution = XtNew (struct solution);

    solutiond -> shell    = XtCreatePopupShell (name,
			 topLevelShellWidgetClass, parent,
			 shell_args, XtNumber (shell_args));

    solutiond -> layout   = XtCreateManagedWidget ("layout",
			 layoutWidgetClass, solutiond -> shell,
			 layout_args, XtNumber (layout_args));

    solutiond -> transfer = XtCreateManagedWidget ("transfer",
                         toggleWidgetClass, solutiond -> layout,
                         toggle_args, XtNumber (toggle_args));

    solutiond -> ortho    = XtCreateManagedWidget ("ortho",
                         toggleWidgetClass, solutiond -> layout,
                         toggle_args, XtNumber (toggle_args));

    solutiond -> eigen    = XtCreateManagedWidget ("eigen",
                         toggleWidgetClass, solutiond -> layout,
                         toggle_args, XtNumber (toggle_args));

    solutiond -> felt     = XtCreateManagedWidget ("felt",
                         toggleWidgetClass, solutiond -> layout,
                         toggle_args, XtNumber (toggle_args));

    solutiond -> debug   = XtCreateManagedWidget ("debug",
                         toggleWidgetClass, solutiond -> layout,
                         toggle_args, XtNumber (toggle_args));

    solutiond -> details = XtCreateManagedWidget ("details",
                         toggleWidgetClass, solutiond -> layout,
                         toggle_args, XtNumber (toggle_args));

    solutiond -> summary = XtCreateManagedWidget ("summary",
                         toggleWidgetClass, solutiond -> layout,
                         toggle_args, XtNumber (toggle_args));

    solutiond -> matrices  = XtCreateManagedWidget ("matrices",
                         toggleWidgetClass, solutiond -> layout,
                         toggle_args, XtNumber (toggle_args));

    solutiond -> plot    = XtCreateManagedWidget ("plot",
                         toggleWidgetClass, solutiond -> layout,
                         toggle_args, XtNumber (toggle_args));

    solutiond -> mode_shapes = XtCreateManagedWidget ("mode_shapes",
                         toggleWidgetClass, solutiond -> layout,
                         toggle_args, XtNumber (toggle_args));

    solutiond -> structure     = XtCreateManagedWidget ("structure",
                         toggleWidgetClass, solutiond -> layout,
                         toggle_args, XtNumber (toggle_args));

    solutiond -> contour_s = XtCreateManagedWidget ("contour_s",
                         toggleWidgetClass, solutiond -> layout,
                         toggle_args, XtNumber (toggle_args));

    solutiond -> contour_d = XtCreateManagedWidget ("contour_d",
                         toggleWidgetClass, solutiond -> layout,
                         toggle_args, XtNumber (toggle_args));

    solutiond -> accept   = XtCreateManagedWidget ("accept",
			 commandWidgetClass, solutiond -> layout,
			 NULL, 0);

    solutiond -> solve   = XtCreateManagedWidget ("solve",
			 commandWidgetClass, solutiond -> layout,
			 NULL, 0);

    solutiond -> dismiss  = XtCreateManagedWidget ("dismiss",
			 commandWidgetClass, solutiond -> layout,
			 NULL, 0);

    solutiond -> help     = CreateHelpButton (solutiond -> layout, "help");

    for (i = 0 ; i < XtNumber (labels) ; i++) {
        label_args [0].value = (XtArgVal) labels [i];
        XtCreateManagedWidget (label_names [i], labelWidgetClass,
                   solutiond -> layout, label_args, XtNumber (label_args));
    }

    XtCreateManagedWidget ("separator1", coreWidgetClass, 
               solutiond -> layout, core_args, XtNumber (core_args));
    XtCreateManagedWidget ("separator2", coreWidgetClass, 
               solutiond -> layout, core_args, XtNumber (core_args));
    XtCreateManagedWidget ("separator3", coreWidgetClass, 
               solutiond -> layout, core_args, XtNumber (core_args));


    /* Create a tab group for the solution dialog. */

    i = 0;
    group [i++]  = solutiond -> eigen;
    group [i++]  = solutiond -> ortho;
    group [i++]  = solutiond -> transfer;
    group [i++]  = solutiond -> felt;
    group [i++]  = solutiond -> summary;
    group [i++]  = solutiond -> details;
    group [i++]  = solutiond -> debug;
    group [i++]  = solutiond -> matrices;
    group [i++]  = solutiond -> contour_s;
    group [i++]  = solutiond -> contour_d;
    group [i++]  = solutiond -> structure;
    group [i++]  = solutiond -> plot;
    group [i++]  = solutiond -> mode_shapes;
    group [i++]  = solutiond -> help;
    group [i++]  = solutiond -> solve;
    group [i++] = solutiond -> accept;
    group [i++] = solutiond -> dismiss;

    XtGetValues (solutiond -> layout, color_args, XtNumber (color_args));
    CreateTabGroup (solutiond -> shell, group, XtNumber (group), 
                    highlight, True);

    XtRealizeWidget (solutiond -> shell);
    SetFocus (solutiond -> felt);

    XtSetArg (args [0], XtNwidth, &width);
    XtGetValues (solutiond -> layout, args, 1);
    XtSetArg (args [0], XtNx, &x);
    XtGetValues (solutiond -> help, args, 1);
    UpdateHelpMessage (solutiond -> help, help_message, width - 2 * x + 160);


    /* Add the translations to each widget. */

    AddDeleteWindowProtocol   (solutiond -> shell, "SolutionDialogAction()");

    XtOverrideTranslations (solutiond -> eigen,     toggle_translations);
    XtOverrideTranslations (solutiond -> ortho,     toggle_translations);
    XtOverrideTranslations (solutiond -> transfer,  toggle_translations);
    XtOverrideTranslations (solutiond -> felt,      toggle_translations);
    XtOverrideTranslations (solutiond -> debug,     toggle_translations);
    XtOverrideTranslations (solutiond -> details,   toggle_translations);
    XtOverrideTranslations (solutiond -> summary,   toggle_translations);
    XtOverrideTranslations (solutiond -> matrices,  toggle_translations);
    XtOverrideTranslations (solutiond -> mode_shapes, toggle_translations);
    XtOverrideTranslations (solutiond -> plot,      toggle_translations);
    XtOverrideTranslations (solutiond -> contour_s, toggle_translations);
    XtOverrideTranslations (solutiond -> contour_d, toggle_translations);
    XtOverrideTranslations (solutiond -> structure, toggle_translations);
    XtOverrideTranslations (solutiond -> solve,     command_translations);
    XtOverrideTranslations (solutiond -> dismiss,   command_translations);
    XtOverrideTranslations (solutiond -> accept,    command_translations);
    XtOverrideTranslations (solutiond -> help,	    help_translations);


    /* Add the necessary callbacks. */

    XtAddCallback(solutiond->accept, XtNcallback,Accept, (XtPointer) solutiond);
    XtAddCallback(solutiond->dismiss,XtNcallback,Dismiss,(XtPointer) solutiond);

    XtAddCallback (solutiond -> solve, XtNcallback, 
                   Accept, (XtPointer)solutiond);

    XtAddCallback (solutiond -> solve, XtNcallback, 
                   ExternalCallback, (XtPointer) solutiond);

	/*
	 * a check to see if the current display is only 1-bit deep,
	 * if so, contouring are a non-no
	 */

    XtSetArg (args [0], XtNdepth, &depth);
    XtGetValues (parent, args, 1);
    if (depth < 8) {
        XtSetSensitive (solutiond -> contour_s, False);
        XtSetSensitive (solutiond -> contour_d, False);

        solution -> displacement = False;
        solution -> stress = False;
    }

    return solutiond;
}


/************************************************************************
 * Function:	SolutionDialogPopup					*
 *									*
 * Description:	Pops up the specified material dialog.			*
 ************************************************************************/

void SolutionDialogPopup (SolutionDialog solutiond)
{
    XtPopup (solutiond -> shell, XtGrabNone);
}
