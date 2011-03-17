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
 * File:	grid.c							*
 *									*
 * Description:	This file contains the public and private function and	*
 *		type definitions for the grid generation control	*
 *		dialog box.						*
 ************************************************************************/

# include <stdio.h>
# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <X11/Shell.h>
# include <X11/Xaw/AsciiText.h>
# include <X11/Xaw/Command.h>
# include <X11/Xaw/Label.h>
# include <X11/Xaw/MenuButton.h>
# include <X11/Xaw/SimpleMenu.h>
# include <X11/Xaw/SmeBSB.h>
# include "Layout.h"
# include "Grid.h"
# include "TabGroup.h"
# include "util.h"
# include "post.h"
# include "mesh.h"
# include "code.h"
# include "error.h"

# ifndef X_NOT_STDC_ENV
# include <stdlib.h>
# endif

# define WaitState 0
# define OkayState 1
# define CancelState 2

static String rule_names [] = {
   "linear",
   "cosinusoidal",
   "sinusoidal",
   "logarithmic",
   "reverse-logarithmic",
   "parabolic",
   "reverse-parabolic"
};

struct grid_dialog {
    Widget         shell;	   /* topLevelShell  <specified>	*/
    Widget         layout;	   /*	Layout  layout		   	*/
    Widget         start [3];	   /*	     AsciiText  start{i}   	*/
    Widget         end [3];	   /*	     AsciiText  end{i} 		*/
    Widget   	   number [3];	   /*	     AsciiText  number{i}	*/
    Widget   	   rule [3];	   /*	     AsciiText  rule{i}		*/
    Widget         rule_button;    /*	     MenuButton  rule_button    */
    Widget         rule_menu;      /*	       SimpleMenu  rule_menu	*/
    Widget         help;	   /*	     MenuButton  help	   	*/
    Widget         okay;	   /*	     Command  okay	   	*/
    Widget         cancel;	   /*	     Command  cancel	   	*/
    Grid	   gr;
    int		   status;
};

static String labels [ ] = {
    "x", "y", "z", "start", "end", "number" 
};

static String names [ ] = {
    "xLabel", "yLabel", "zLabel", "startLabel", "endLabel", "numberLabel" 
};


/* Resources */

static Pixel highlight;

static String layout_string = 
"vertical { \
     horizontal { \
        vertical { \
           8 \
           height startLabel \
           8 \
           ((height start1 - height xLabel) / 2) \
           xLabel \
           ((height start1 - height xLabel) / 2) \
           8 \
           ((height start2 - height yLabel) / 2) \
           yLabel \
           ((height start2 - height yLabel) / 2) \
           8 \
           ((height start3 - height zLabel) / 2) \
           zLabel \
           ((height start3 - height zLabel) / 2) \
           8 \
        } \
        8 \
        vertical { \
           8 \
           horizontal { \
              ((width start1 - width startLabel) / 2) \
              startLabel \
              ((width start1 - width startLabel) / 2) \
           } \
           8 \
           start1 \
           8 \
           start2 \
           8 \
           start3 \
           8 \
        } \
        8 \
        vertical { \
           8 \
           horizontal { \
              ((width end1 - width endLabel) / 2) \
              endLabel \
              ((width end1 - width endLabel) / 2) \
           } \
           8 \
           end1 \
           8 \
           end2 \
           8 \
           end3 \
           8 \
        } \
        8 \
        vertical { \
           8 \
           horizontal { \
              ((width number1 - width numberLabel) / 2) \
              numberLabel \
              ((width number1 - width numberLabel) / 2) \
           } \
           8 \
           number1 \
           8 \
           number2 \
           8 \
           number3 \
           8 \
        } \
        8 \
        vertical { \
           8 \
           ((height numberLabel - height rule_button) / 2) \
           horizontal { \
              ((width rule1 - width rule_button) / 2) \
              rule_button \
              ((width rule1 - width rule_button) / 2) \
           } \
           ((height numberLabel - height rule_button) / 2) \
           8 \
           rule1 \
           8 \
           rule2 \
           8 \
           rule3 \
           8 \
        } \
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

static Arg text_args [ ] = {
    {XtNeditType,    (XtArgVal) XawtextEdit},
    {XtNborderWidth, (XtArgVal) 0},
    {XtNpieceSize,   (XtArgVal) 32},
    {XtNcursorName,  (XtArgVal) "left_ptr"},
};

static Arg label_args [ ] = {
    {XtNlabel,       (XtArgVal) ""},
    {XtNborderWidth, (XtArgVal) 0},
};

static Arg core_args [ ] = {
    {XtNwidth,  (XtArgVal) 3},
    {XtNheight, (XtArgVal) 3},
};

static Arg button_args [ ] = {
    {XtNlabel,       (XtArgVal) ""},
    {XtNmenuName,    (XtArgVal) ""},
    {XtNborderWidth, (XtArgVal) 0},
};


/* Translation tables */

static String text_table =
"<Key>Return: GridDialogAction(okay)\n\
 <Key>Escape: GridDialogAction(cancel)\n\
 Ctrl<Key>h:  GridDialogAction(help)\n\
 <Btn1Down>:  SetFocus() select-start()";

static XtTranslations text_translations;


static String button_table =
"<Key>Return: GridDialogAction(okay)\n\
 <Key>Escape: GridDialogAction(cancel)\n\
 Ctrl<Key>h:  GridDialogAction(help)\n\
 <BtnDown>:   PostMenu()\n\
 <Key>space:  PostMenu()";

static XtTranslations button_translations;


static String command_table =
"<Key>Return:  GridDialogAction(okay)\n\
 <Key>Escape:  GridDialogAction(cancel)\n\
 Ctrl<Key>h:   GridDialogAction(help)\n\
 <Key>space:   AutoRepeat(off) set()\n\
 <KeyUp>space: AutoRepeat(saved) notify() unset()";

static XtTranslations command_translations;


static String help_table =
"<Key>Return: GridDialogAction(okay)\n\
 <Key>Escape: GridDialogAction(cancel)\n\
 Ctrl<Key>h:  GridDialogAction(help)\n\
 <Key>space:  PostMenu()";

static XtTranslations help_translations;


/* Help message. */

static String help_message ="\
Use this form to define the parameters for grids of line, quadrilateral, \
and brick elements.  The start and end coordinates define the two opposite \
corners of the grid to generate.  The number and rule entries define \
how many elements, and which spacing rule to use in generating the elements, \
wll be generated along each of the three axes.";


/************************************************************************
 * Function:	Action							*
 *									*
 * Description:	An action procedure which emulates pressing of the	*
 *		specified button.					*
 ************************************************************************/

static void Action (Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    if (XtClass (w) == topLevelShellWidgetClass)
	w = XtNameToWidget (w, "layout.cancel");
    else
	w = XtNameToWidget (XtParent (w), params [0]);

    if (!strcmp (XtName (w), "help"))
	XtCallActionProc (w, "PostMenu", event, NULL, 0);
    else
	XtCallCallbacks (w, XtNcallback, NULL);
}


/************************************************************************
 * Function:	ChangeRule
 *	
 * Description:	 
 ************************************************************************/

static void ChangeRule (Widget w, XtPointer client_data, XtPointer call_data)
{
    GridDialog  gridd;
    Widget      current_rule;
    int	        i;


    if (!(w = XawSimpleMenuGetActiveEntry (w)))
        return;

    gridd = (GridDialog) client_data;

    current_rule = GetFocus (gridd -> start [0]);

    for (i = 0 ; i < 3 ; i++) 
       if (current_rule == gridd -> rule [i])
          break;

    if (i == 3)
       return;

    SetTextString (current_rule, GetLabelString (w));
}


/************************************************************************
 * Function:	Okay							*
 *									*
 * Description:								*
 ************************************************************************/

static void Okay (Widget w, XtPointer client_data, XtPointer call_data)
{
    GridDialog   gridd;


    gridd = (GridDialog) client_data;
    gridd -> status = OkayState;
}


/************************************************************************
 * Function:	Cancel							*
 *									*
 * Description:	sets the cancel flag					*
 ************************************************************************/

static void Cancel (Widget w, XtPointer client_data, XtPointer call_data)
{
    GridDialog gridd;

    gridd = (GridDialog) client_data;
    gridd -> status = CancelState;
}

static int MatchRuleName (String name)
{
   int	i;

   if (strcmp(name, "") == 0)
      return 0;			/* default - linear rule */

   for (i = 0 ; i < XtNumber (rule_names) ; i++) 
      if (strcmp(rule_names [i], name) == 0)
         return i;

   return -1;
}

/************************************************************************
 * Function:	GridDialogSet						*
 *									*
 * Description:	fills the Grid structure based on the text fields	*
 ************************************************************************/

static int GridDialogSet (GridDialog gridd)
{
    Arg		args [1];
    String	value;
    int		rule;

    XtSetArg (args [0], XtNstring, &value);

    XtGetValues (gridd -> start [0], args, 1); 
    gridd -> gr -> xs = exptod (value, NULL);

    XtGetValues (gridd -> start [1], args, 1); 
    gridd -> gr -> ys = exptod (value, NULL);

    XtGetValues (gridd -> start [2], args, 1); 
    gridd -> gr -> zs = exptod (value, NULL);

    XtGetValues (gridd -> end [0], args, 1); 
    gridd -> gr -> xe = exptod (value, NULL);

    XtGetValues (gridd -> end [1], args, 1); 
    gridd -> gr -> ye = exptod (value, NULL);

    XtGetValues (gridd -> end [2], args, 1); 
    gridd -> gr -> ze = exptod (value, NULL);

    XtGetValues (gridd -> number [0], args, 1); 
    gridd -> gr -> xnumber = exptod (value, NULL);

    XtGetValues (gridd -> number [1], args, 1); 
    gridd -> gr -> ynumber = exptod (value, NULL);

    XtGetValues (gridd -> number [2], args, 1); 
    gridd -> gr -> znumber = exptod (value, NULL);

    XtGetValues (gridd -> rule [0], args, 1); 
    rule = MatchRuleName (value);
    if (rule == -1) {
       error ("%s is an invalid rule name", value);
       return 1;
    }
    else
        gridd -> gr -> xrule = (Rule) rule;
  
    XtGetValues (gridd -> rule [1], args, 1); 
    rule = MatchRuleName (value);
    if (rule == -1) {
       error ("%s is an invalid rule name", value);
       return 1;
    }
    else
        gridd -> gr -> yrule = (Rule) rule;

    XtGetValues (gridd -> rule [2], args, 1); 
    rule = MatchRuleName (value);
    if (rule == -1) {
       error ("%s is an invalid rule name", value);
       return 1;
    }
    else
        gridd -> gr -> zrule = (Rule) rule;

    return 0;
}

/************************************************************************
 * Function:	GridDialogCreate					*
 *									*
 * Description:	Creates a new grid dialog.  You would never want to	*
 *		have more than one of these but the interface is kept	*
 *		consistent with those of the other dialogs.		*
 ************************************************************************/

GridDialog GridDialogCreate (Widget parent, String name, String title)
{
    Cardinal		i;
    char		buffer [128];
    Arg			args [2];
    Widget		group [15];
    GridDialog		gridd;
    Dimension		width;
    Position		x;
    static XtAppContext	app_context = NULL;
    static XtActionsRec	actions [ ] = {{"GridDialogAction", Action}};


    /* Perform one time initialization. */

    if (app_context == NULL) {
	app_context = XtWidgetToApplicationContext (parent);
	XtAppAddActions (app_context, actions, XtNumber (actions));
	AddAutoRepeatAction (app_context);

        layout_args [0].value = StringToLayout (parent, layout_string);

	text_translations     = XtParseTranslationTable (text_table);
	command_translations  = XtParseTranslationTable (command_table);
	help_translations     = XtParseTranslationTable (help_table);
	button_translations   = XtParseTranslationTable (button_table);
    }


    /* Create the grid dialog and its widgets. */

    XtSetArg (shell_args [0], XtNtitle, title);
    XtSetArg (shell_args [1], XtNiconName, title);

    gridd = XtNew (struct grid_dialog);
   
    gridd -> gr = XtNew (struct _grid);

    gridd -> shell    = XtCreatePopupShell (name,
			 topLevelShellWidgetClass, parent,
			 shell_args, XtNumber (shell_args));

    gridd -> layout   = XtCreateManagedWidget ("layout",
			 layoutWidgetClass, gridd -> shell,
			 layout_args, XtNumber (layout_args));

    for (i = 0 ; i < 3 ; i++) {
       sprintf (buffer,"start%d", i+1);
       gridd -> start [i] = XtCreateManagedWidget (XtNewString (buffer),
			 asciiTextWidgetClass, gridd -> layout,
			 text_args, XtNumber (text_args));

       sprintf (buffer,"end%d", i+1);
       gridd -> end [i] = XtCreateManagedWidget (XtNewString (buffer),
			 asciiTextWidgetClass, gridd -> layout,
			 text_args, XtNumber (text_args));

       sprintf (buffer,"number%d", i+1);
       gridd -> number [i] = XtCreateManagedWidget (XtNewString (buffer),
			 asciiTextWidgetClass, gridd -> layout,
			 text_args, XtNumber (text_args));

       sprintf (buffer,"rule%d", i+1);
       gridd -> rule [i] = XtCreateManagedWidget (XtNewString (buffer),
			 asciiTextWidgetClass, gridd -> layout,
			 text_args, XtNumber (text_args));

    }

    gridd -> rule_menu = XtCreateManagedWidget ("rule_menu",
			 simpleMenuWidgetClass, gridd -> layout,
			 NULL, 0);

    XtSetArg (button_args [0], XtNlabel,    "rule");
    XtSetArg (button_args [1], XtNmenuName, "rule_menu");

    gridd -> rule_button = XtCreateManagedWidget ("rule_button",
			 menuButtonWidgetClass, gridd -> layout,
			 button_args, XtNumber (button_args));

    for (i = 0 ; i < XtNumber (rule_names) ; i++)
       XtCreateManagedWidget (rule_names [i], smeBSBObjectClass,
                              gridd -> rule_menu, NULL, 0);

    AddPostMenuActions (gridd -> rule_menu);

    gridd -> okay   = XtCreateManagedWidget ("okay",
			 commandWidgetClass, gridd -> layout,
			 NULL, 0);

    gridd -> cancel  = XtCreateManagedWidget ("cancel",
			 commandWidgetClass, gridd -> layout,
			 NULL, 0);

    gridd -> help     = CreateHelpButton (gridd -> layout, "help");

    for (i = 0; i < XtNumber (labels); i ++) {
	label_args [0].value = (XtArgVal) labels [i];
	XtCreateManagedWidget (names [i], labelWidgetClass,
		gridd -> layout, label_args, XtNumber (label_args));
    }


    XtCreateManagedWidget ("separator", coreWidgetClass,
			gridd -> layout, core_args, XtNumber (core_args));



    /* Create a tab group for the grid dialog. */

    i = 0;
    group [i++]  = gridd -> start [0];
    group [i++]  = gridd -> start [1];
    group [i++]  = gridd -> start [2];
    group [i++]  = gridd -> end [0];
    group [i++]  = gridd -> end [1];
    group [i++]  = gridd -> end [2];
    group [i++]  = gridd -> number [0];
    group [i++]  = gridd -> number [1];
    group [i++]  = gridd -> number [2];
    group [i++]  = gridd -> rule [0];
    group [i++]  = gridd -> rule [1];
    group [i++]  = gridd -> rule [2];
    group [i++]  = gridd -> help;
    group [i++]  = gridd -> okay;
    group [i++] = gridd -> cancel;

    XtGetValues (gridd -> layout, color_args, XtNumber (color_args));
    CreateTabGroup (gridd -> shell, group, XtNumber (group), highlight, True);
    XtRealizeWidget (gridd -> shell);
    SetFocus (gridd -> start [0]);

    XtSetArg (args [0], XtNwidth, &width);
    XtGetValues (gridd -> layout, args, 1);
    XtSetArg (args [0], XtNx, &x);
    XtGetValues (gridd -> help, args, 1);
    UpdateHelpMessage (gridd -> help, help_message, width - 2 * x);


    /* Add the translations to each widget. */

    AddDeleteWindowProtocol   (gridd -> shell, "GridDialogAction()");

    for (i = 0 ; i < 3 ; i++) {
       XtOverrideTranslations (gridd -> start [i],  text_translations);
       XtOverrideTranslations (gridd -> end [i],    text_translations);
       XtOverrideTranslations (gridd -> number [i], text_translations);
       XtOverrideTranslations (gridd -> rule [i],   text_translations);
    }

    XtOverrideTranslations (gridd -> rule_button, button_translations);

    XtOverrideTranslations (gridd -> okay,    command_translations);
    XtOverrideTranslations (gridd -> cancel,   command_translations);
    XtOverrideTranslations (gridd -> help,	    help_translations);


    /* Add the necessary callbacks. */

    XtAddCallback (gridd -> okay,  XtNcallback, Okay,  (XtPointer) gridd);
    XtAddCallback (gridd -> cancel, XtNcallback, Cancel, (XtPointer) gridd);
    XtAddCallback (gridd -> rule_menu, XtNpopdownCallback, ChangeRule, (XtPointer) gridd); 

    return gridd;
}

/************************************************************************
 * Function:	GridDialogPopup						*
 *									*
 * Description:	Pops up the specified material dialog.			*
 ************************************************************************/

Grid GridDialogPopup (GridDialog gridd)
{
    XEvent	 event;
    XtAppContext app_context;
    
    app_context = XtWidgetToApplicationContext (gridd -> shell);

    XtPopup (gridd -> shell, XtGrabExclusive);
    gridd -> status = WaitState;

    while (gridd -> status == WaitState) {
       XtAppNextEvent (app_context, &event);
       XtDispatchEvent (&event);
    }

    XtPopdown (gridd -> shell);

    if (gridd -> status == CancelState)
       return NULL;
    else {
       if (!GridDialogSet (gridd))
          return gridd -> gr;
       else
          return GridDialogPopup (gridd);
    }
}
