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
 * File:	canvas.c						*
 *									*
 * Description:	This file contains the private and public function and	*
 *		type definitions for the canvas dialog box.		*
 ************************************************************************/

# include <stdio.h>
# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <X11/Shell.h>
# include <X11/Xaw/AsciiText.h>
# include <X11/Xaw/Command.h>
# include <X11/Xaw/Toggle.h>
# include <X11/Xaw/Label.h>
# include "Drawing.h"
# include "Layout.h"
# include "Canvas.h"
# include "TabGroup.h"
# include "util.h"
# include "code.h"

# ifndef X_NOT_STDC_ENV
# include <stdlib.h>
# endif

extern void ToggleEltNumberStatus ();
extern void ToggleNodeNumberStatus ();
extern void ToggleGridStatus ();
extern void ToggleSnapStatus ();
extern void SetNodeNumberFlag ();
extern void SetEltNumberFlag ();

Canvas	canvas;

static Boolean no_set = False;

struct canvas_dialog {
    Widget  shell;		/* topLevelShell  <specified>	*/
    Widget  layout;		/*	Layout  layout		*/
    Widget  xmin;		/*	     AsciiText xmin	*/
    Widget  xmax;		/*	     AsciiText xmax	*/
    Widget  ymin;		/*	     AsciiText ymin	*/
    Widget  ymax;		/*	     AsciiText ymax	*/
    Widget  snap_size;		/*	     AsciiText snap	*/
    Widget  grid_size;		/*	     AsciiText grid	*/
    Widget  snap;		/*	     Toggle snap state	*/
    Widget  grid;		/*	     Toggle grid state	*/ 
    Widget  element_numbers;	/*	     Toggle element #	*/
    Widget  node_numbers;	/*           Toggle node #      */
    Widget  element_color;	/*	     AsciiText e color  */
    Widget  node_color;		/*	     AsciiText n color  */
    Widget  tool_color;		/* 	     AsciiText t color  */
    Widget  tool_font;		/*	     AsciiText t font   */	
    Widget  label_font;		/*           AsciiText l font   */
    Widget  help;		/*	     MenuButton  help	*/
    Widget  accept;		/*	     Command  accept	*/
    Widget  dismiss;		/*	     Command  dismiss	*/
    Widget  dw;			/* drawing widget		*/
};


static String labels [ ] = {
     "min X:","max X:","min Y:","max Y:","snap:","grid:","snap","grid",
     "elt color:","node color:","tool color:","label font:","tool font:",
     "element numbers","node numbers"
};

static String label_names [ ] = {
     "xmin_name","xmax_name","ymin_name","ymax_name","snap_size_name",
     "grid_size_name","snap_name","grid_name","element_color_name",
     "node_color_name","tool_color_name","label_font_name",
     "tool_font_name","element_numbers_name","node_numbers_name"
};


/* Resources */

static Pixel highlight;

static char layout_string [ ] =
"vertical { \
     horizontal { \
        vertical { \
            4 \
            ((height xmin - height xmin_name) / 2) \
            xmin_name \
            ((height xmin - height xmin_name) / 2) \
	    4 \
            ((height xmax - height xmax_name) / 2) \
            xmax_name \
            ((height xmax - height xmax_name) / 2) \
            4 \
            ((height ymin - height ymin_name) / 2) \
            ymin_name \
            ((height ymin - height ymin_name) / 2) \
	    4 \
            ((height ymax - height ymax_name) / 2) \
            ymax_name \
            ((height ymax - height ymax_name) / 2) \
            4 \
            ((height grid_size - height grid_size_name) / 2) \
            grid_size_name \
            ((height grid_size - height grid_size_name) / 2) \
            4 \
            ((height snap_size - height snap_size_name) / 2) \
            snap_size_name \
            ((height snap_size - height snap_size_name) / 2) \
            (4 + height element_number + \
             height node_numbers - height snap_size ) \
            horizontal { \
                ((width xmin_name - width snap_size_name) / 2) \
                snap \
                1 \
                snap_name \
                (width xmin + width xmin_name + 1 - \
                 (2 + width snap_name + width snap + \
                  width grid + width grid_name + \
                  (width xmin_name - width snap_size_name) / 2)) \
                grid \
                1 \
                grid_name \
            } \
            4 \
        } \
        1 \
        vertical { \
            4 \
            xmin \
            4 \
            xmax \
            4 \
            ymin \
            4 \
            ymax \
            4 \
            grid_size \
            4 \
            snap_size \
            4 \
        } \
        18 \
        vertical { \
            4 \
            ((height node_color - height node_color_name) / 2) \
            node_color_name \
            ((height node_color - height node_color_name) / 2) \
            4 \
            ((height element_color - height element_color_name) / 2) \
            element_color_name \
            ((height element_color - height element_color_name) / 2) \
            4 \
            ((height tool_color - height tool_color_name) / 2) \
            tool_color_name \
            ((height tool_color - height tool_color_name) / 2) \
            4 \
            ((height tool_font - height tool_font_name) / 2) \
            tool_font_name \
            ((height tool_font - height tool_font_name) / 2) \
            4 \
            ((height label_font - height label_font_name) / 2) \
            label_font_name \
            ((height label_font - height label_font_name) / 2) \
            4 \
            horizontal { \
                node_numbers \
                1 \
                node_numbers_name \
            } \
            4 \
            horizontal { \
                element_numbers \
                1 \
                element_numbers_name \
            } \
            4 \
        } \
        1 \
        vertical { \
            4 \
            node_color <+inf *> \
            4 \
            element_color <+inf *> \
            4 \
            tool_color <+inf *> \
            4 \
            tool_font <+inf *> \
            4 \
            label_font <+inf *> \
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
    {XtNlabel,	            (XtArgVal) ""},
    {XtNborderWidth,        (XtArgVal) 0},
};

static Arg toggle_args [ ] = {
    {XtNlabel,		    (XtArgVal) " "},
};

static Arg core_args [ ] = {
    {XtNwidth,  (XtArgVal) 3},
    {XtNheight, (XtArgVal) 3},
};

/* Translation tables */

static String text_table =
"<Key>Return: CanvasDialogAction(accept)\n\
 <Key>Escape: CanvasDialogAction(dismiss)\n\
 Ctrl<Key>h:  CanvasDialogAction(help)\n\
 <Btn1Down>:  SetFocus() select-start()";

static XtTranslations text_translations;

static String command_table =
"<Key>Return:   AutoRepeat(off) set()\n\
 <KeyUp>Return: AutoRepeat(saved) notify() unset()\n\
 <Key>space:    AutoRepeat(off) set()\n\
 <KeyUp>space:  AutoRepeat(saved) notify() unset()\n\
 <Key>Escape:   CanvasDialogAction(dismiss)";

static XtTranslations command_translations;

static String toggle_table =
"<Key>Return:	CanvasDialogAction(accept)\n\
 <Key>Escape:   CanvasDialogAction(dismiss)\n\
 <Key>space:	toggle()\n\
 Ctrl<Key>h:	CanvasDialogAction(help)";

static XtTranslations toggle_translations;

/* Help message. */

static String help_message ="\
Use this form to configure the main velvet work space: the minimum and \
maximum coordinates, grid and snap size, fonts and color.  The toggle buttons \
near the bottom of the form control whether a given feature is on or off. \
Note that these toggles are also available on the main 'Canvas' menu.";


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
    CanvasDialog   canvasd;
    String	   value;
    Boolean	   state;

    canvasd = (CanvasDialog) client_data;

    XtSetArg (args [0], XtNstring,  &value);

    XtGetValues (canvasd -> xmin, args, 1);
    canvas -> xmin = exptod (value, NULL);
    XtGetValues (canvasd -> xmax, args, 1);
    canvas -> xmax = exptod (value, NULL);
    XtGetValues (canvasd -> ymin, args, 1);
    canvas -> ymin = exptod (value, NULL);
    XtGetValues (canvasd -> ymax, args, 1);
    canvas -> ymax = exptod (value, NULL);
    XtGetValues (canvasd -> grid_size, args, 1);
    canvas -> grid_size = exptod (value, NULL);
    XtGetValues (canvasd -> snap_size, args, 1);
    canvas -> snap_size = exptod (value, NULL);

    XtGetValues (canvasd -> element_color, args, 1);
    canvas -> element_color = XtNewString (value);
    XtGetValues (canvasd -> node_color, args, 1);
    canvas -> node_color = XtNewString (value);
    XtGetValues (canvasd -> tool_color, args, 1);
    canvas -> tool_color = XtNewString (value);
    XtGetValues (canvasd -> tool_font, args, 1);
    canvas -> tool_font = XtNewString (value);
    XtGetValues (canvasd -> label_font, args, 1);
    canvas -> label_font = XtNewString (value);    

	/*
	 * for this stuff we need to call some external functions
	 * to make sure the checkmarks get changed and because we
	 * don't want to do the element and node numbering stuff
	 * here (its not as simple as just changing something
	 * in the drawing widget
	 */

    XtSetArg (args [0], XtNstate, &state);
  
    no_set = True; 
    
    XtGetValues (canvasd -> element_numbers, args, 1);
    if (canvas -> element_numbers != state)
       ToggleEltNumberStatus ();

    XtGetValues (canvasd -> node_numbers, args, 1);
    if (canvas -> node_numbers != state)
       ToggleNodeNumberStatus ();

    XtGetValues (canvasd -> grid, args, 1);
    if (canvas -> grid != state)
       ToggleGridStatus ();

    XtGetValues (canvasd -> snap, args, 1);
    if (canvas -> snap != state)
       ToggleSnapStatus ();

    no_set = False;

    CanvasDialogSet (canvasd);
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
    CanvasDialog canvasd;

    canvasd = (CanvasDialog) client_data;
    XtPopdown (canvasd -> shell);
}


/************************************************************************
 * Function:	CanvasDialogSet						*
 *									*
 * Description:	fills the entry fields based on current canvas settings *
 ************************************************************************/

void CanvasDialogSet (canvasd)
    CanvasDialog	canvasd;
{
    Arg		args [14];
    Cardinal	count;
    char	buffer [80];

    if (no_set)
       return;
 
    sprintf (buffer,"%g",canvas -> xmin);
    SetTextString (canvasd -> xmin, buffer);
    sprintf (buffer,"%g",canvas -> xmax);
    SetTextString (canvasd -> xmax, buffer);
    sprintf (buffer,"%g",canvas -> ymin);
    SetTextString (canvasd -> ymin, buffer);
    sprintf (buffer,"%g",canvas -> ymax);
    SetTextString (canvasd -> ymax, buffer);

    sprintf (buffer,"%g",canvas -> snap_size);
    SetTextString (canvasd -> snap_size, buffer);
    sprintf (buffer,"%g",canvas -> grid_size);
    SetTextString (canvasd -> grid_size, buffer);

    SetTextString (canvasd -> element_color, canvas -> element_color);
    SetTextString (canvasd -> node_color, canvas -> node_color);
    SetTextString (canvasd -> tool_color, canvas -> tool_color);
    SetTextString (canvasd -> tool_font, canvas -> tool_font);
    SetTextString (canvasd -> label_font, canvas -> label_font);
/*
    if (GetTextString (canvasd -> element_color) != canvas -> element_color) {
	SetTextString (canvasd -> element_color, canvas -> element_color);
	canvas -> element_color = GetTextString (canvasd -> element_color);
    }

    if (GetTextString (canvasd -> node_color) != canvas -> node_color) {
	SetTextString (canvasd -> node_color, canvas -> node_color);
	canvas -> node_color = GetTextString (canvasd -> node_color);
    }

    if (GetTextString (canvasd -> tool_color) != canvas -> tool_color) {
	SetTextString (canvasd -> tool_color, canvas -> tool_color);
	canvas -> tool_color = GetTextString (canvasd -> tool_color);
    }

    if (GetTextString (canvasd -> tool_font) != canvas -> tool_font) {
	SetTextString (canvasd -> tool_font, canvas -> tool_font);
	canvas -> tool_font = GetTextString (canvasd -> tool_font);
    }

    if (GetTextString (canvasd -> label_font) != canvas -> label_font) {
	SetTextString (canvasd -> label_font, canvas -> label_font);
	canvas -> label_font = GetTextString (canvasd -> label_font);
    }
*/
    XtSetArg (args [0], XtNstate, canvas -> element_numbers);
    XtSetValues (canvasd -> element_numbers, args, 1);
    XtSetArg (args [0], XtNstate, canvas -> node_numbers);
    XtSetValues (canvasd -> node_numbers, args, 1);
    XtSetArg (args [0], XtNstate, canvas -> snap);
    XtSetValues (canvasd -> snap, args, 1);
    XtSetArg (args [0], XtNstate, canvas -> grid);
    XtSetValues (canvasd -> grid, args, 1);

    count = 0;
    XtSetArg (args [count],XtNxMin, Float2Arg(canvas -> xmin)); count++;
    XtSetArg (args [count],XtNxMax, Float2Arg(canvas -> xmax)); count++;
    XtSetArg (args [count],XtNyMin, Float2Arg(canvas -> ymin)); count++;
    XtSetArg (args [count],XtNyMax, Float2Arg(canvas -> ymax)); count++;
    XtSetArg (args [count],XtNgridSize,Float2Arg(canvas -> grid_size)); count++;
    XtSetArg (args [count],XtNsnapSize,Float2Arg(canvas -> snap_size)); count++;
    XtSetArg (args [count],XtNsnap, canvas -> snap); count++;
    XtSetArg (args [count],XtNgrid, canvas -> grid); count++; 
    XtSetArg (args [count],XtNxScale, Float2Arg (canvas -> scale)); count ++;
    XtSetArg (args [count],XtNyScale, Float2Arg (canvas -> scale)); count ++;

    XtSetValues (canvasd -> dw, args, count);

    SetNodeNumberFlag();
    SetEltNumberFlag();
}


/************************************************************************
 * Function:	CanvasDialogCreate					*
 *									*
 * Description:	Creates a new material dialog.  You would never want to	*
 *		have more than one of these but the interface is kept	*
 *		consistent with those of the other dialogs.		*
 ************************************************************************/

CanvasDialog CanvasDialogCreate (parent, dw, name, title)
    Widget parent;
    Widget dw;
    String name;
    String title;
{
    Cardinal		i;
    Arg			args [1];
    Widget		group [18];
    CanvasDialog	canvasd;
    Dimension		width;
    Position		x;
    static XtAppContext	app_context = NULL;
    static XtActionsRec	actions [ ] = {{"CanvasDialogAction", Action}};


    /* Perform one time initialization. */

    if (app_context == NULL) {
	app_context = XtWidgetToApplicationContext (parent);
	XtAppAddActions (app_context, actions, XtNumber (actions));
	AddAutoRepeatAction (app_context);

        layout_args [0].value = StringToLayout (parent, layout_string);

        toggle_translations = XtParseTranslationTable (toggle_table);
	text_translations = XtParseTranslationTable (text_table);
	command_translations = XtParseTranslationTable (command_table);
    }


    /* Create the material dialog and its widgets. */

    XtSetArg (shell_args [0], XtNtitle, title);
    XtSetArg (shell_args [1], XtNiconName, title);

    canvasd = XtNew (struct canvas_dialog);

    canvas = XtNew (struct canvas);

    canvasd -> dw = dw;

    canvasd -> shell    = XtCreatePopupShell (name,
			 topLevelShellWidgetClass, parent,
			 shell_args, XtNumber (shell_args));

    canvasd -> layout   = XtCreateManagedWidget ("layout",
			 layoutWidgetClass, canvasd -> shell,
			 layout_args, XtNumber (layout_args));

    canvasd -> xmin  = XtCreateManagedWidget ("xmin",
			 asciiTextWidgetClass, canvasd -> layout,
			 text_args, XtNumber (text_args));

    canvasd -> xmax  = XtCreateManagedWidget ("xmax",
			 asciiTextWidgetClass, canvasd -> layout,
			 text_args, XtNumber (text_args));

    canvasd -> ymin  = XtCreateManagedWidget ("ymin",
			 asciiTextWidgetClass, canvasd -> layout,
			 text_args, XtNumber (text_args));

    canvasd -> ymax = XtCreateManagedWidget ("ymax",
			 asciiTextWidgetClass, canvasd -> layout,
			 text_args, XtNumber (text_args));

    canvasd -> snap_size = XtCreateManagedWidget ("snap_size",
			 asciiTextWidgetClass, canvasd -> layout,
			 text_args, XtNumber (text_args));

    canvasd -> grid_size = XtCreateManagedWidget ("grid_size",
			 asciiTextWidgetClass, canvasd -> layout,
			 text_args, XtNumber (text_args));

    canvasd -> element_color = XtCreateManagedWidget ("element_color",
			 asciiTextWidgetClass, canvasd -> layout,
			 text_args, XtNumber (text_args));

    canvasd -> node_color = XtCreateManagedWidget ("node_color",
			 asciiTextWidgetClass, canvasd -> layout,
			 text_args, XtNumber (text_args));

    canvasd -> tool_color = XtCreateManagedWidget ("tool_color",
			 asciiTextWidgetClass, canvasd -> layout,
			 text_args, XtNumber (text_args));

    canvasd -> tool_font = XtCreateManagedWidget ("tool_font",
			 asciiTextWidgetClass, canvasd -> layout,
			 text_args, XtNumber (text_args));

    canvasd -> label_font = XtCreateManagedWidget ("label_font",
			 asciiTextWidgetClass, canvasd -> layout,
			 text_args, XtNumber (text_args));

    canvasd -> element_numbers = XtCreateManagedWidget ("element_numbers",
                         toggleWidgetClass, canvasd -> layout,
                         toggle_args, XtNumber (toggle_args));

    canvasd -> node_numbers = XtCreateManagedWidget ("node_numbers",
                         toggleWidgetClass, canvasd -> layout,
                         toggle_args, XtNumber (toggle_args));

    canvasd -> snap = XtCreateManagedWidget ("snap",
                         toggleWidgetClass, canvasd -> layout,
                         toggle_args, XtNumber (toggle_args));

    canvasd -> grid = XtCreateManagedWidget ("grid",
                         toggleWidgetClass, canvasd -> layout,
                         toggle_args, XtNumber (toggle_args));

    canvasd -> accept   = XtCreateManagedWidget ("accept",
			 commandWidgetClass, canvasd -> layout,
			 NULL, 0);

    canvasd -> dismiss  = XtCreateManagedWidget ("dismiss",
			 commandWidgetClass, canvasd -> layout,
			 NULL, 0);

    canvasd -> help     = CreateHelpButton (canvasd -> layout, "help");


    XtCreateManagedWidget ("separator", coreWidgetClass,
			canvasd -> layout, core_args, XtNumber (core_args));

    for (i = 0 ; i < XtNumber (labels) ; i++) {
        label_args [0].value = (XtArgVal) labels [i];
        XtCreateManagedWidget (label_names [i], labelWidgetClass,
                   canvasd -> layout, label_args, XtNumber (label_args));
    }

    /* Create a tab group for the canvas dialog. */

    group [0]  = canvasd -> xmin;
    group [1]  = canvasd -> xmax;
    group [2]  = canvasd -> ymin;
    group [3]  = canvasd -> ymax;
    group [4]  = canvasd -> grid_size;
    group [5]  = canvasd -> snap_size;
    group [6]  = canvasd -> snap;
    group [7]  = canvasd -> grid;
    group [8]  = canvasd -> node_color;
    group [9]  = canvasd -> element_color;
    group [10] = canvasd -> tool_color;
    group [11] = canvasd -> tool_font;
    group [12] = canvasd -> label_font;
    group [13] = canvasd -> node_numbers;
    group [14] = canvasd -> element_numbers;
    group [15] = canvasd -> help;
    group [16] = canvasd -> accept;
    group [17] = canvasd -> dismiss;

    XtGetValues (canvasd -> layout, color_args, XtNumber (color_args));
    CreateTabGroup (canvasd -> shell, group, XtNumber (group), 
                    highlight, True);

    XtRealizeWidget (canvasd -> shell);
    SetFocus (canvasd -> xmin);

    XtSetArg (args [0], XtNwidth, &width);
    XtGetValues (canvasd -> layout, args, 1);
    XtSetArg (args [0], XtNx, &x);
    XtGetValues (canvasd -> help, args, 1);
    UpdateHelpMessage (canvasd -> help, help_message, width - 2 * x);


    /* Add the translations to each widget. */

    AddDeleteWindowProtocol   (canvasd -> shell, "CanvasDialogAction()");

    XtOverrideTranslations (canvasd -> xmin,	text_translations);
    XtOverrideTranslations (canvasd -> xmax,	text_translations);
    XtOverrideTranslations (canvasd -> ymin,	text_translations);
    XtOverrideTranslations (canvasd -> ymax,	text_translations);
    XtOverrideTranslations (canvasd -> grid_size,	text_translations);
    XtOverrideTranslations (canvasd -> snap_size,	text_translations);
    XtOverrideTranslations (canvasd -> element_color,	text_translations);
    XtOverrideTranslations (canvasd -> node_color,	text_translations);
    XtOverrideTranslations (canvasd -> tool_color,	text_translations);
    XtOverrideTranslations (canvasd -> tool_font,	text_translations);
    XtOverrideTranslations (canvasd -> label_font,	text_translations);
    XtOverrideTranslations (canvasd -> element_numbers, toggle_translations);
    XtOverrideTranslations (canvasd -> node_numbers,    toggle_translations);
    XtOverrideTranslations (canvasd -> snap,            toggle_translations);
    XtOverrideTranslations (canvasd -> grid,            toggle_translations);
    XtOverrideTranslations (canvasd -> dismiss, command_translations);
    XtOverrideTranslations (canvasd -> accept,  command_translations);


    /* Add the necessary callbacks. */

    XtAddCallback(canvasd->accept, XtNcallback,Accept, (XtPointer) canvasd);
    XtAddCallback(canvasd->dismiss,XtNcallback,Dismiss,(XtPointer) canvasd);

    return canvasd;
}


/************************************************************************
 * Function:	CanvasDialogPopup					*
 *									*
 * Description:	Pops up the specified material dialog.			*
 ************************************************************************/

void CanvasDialogPopup (canvasd)
    CanvasDialog canvasd;
{
    XtPopup (canvasd -> shell, XtGrabNone);
}
