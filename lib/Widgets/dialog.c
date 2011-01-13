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
 * File:	dialog.c						*
 *									*
 * Description:	This file contains the function declarations which	*
 *		operate on dialog boxes.				*
 ************************************************************************/

# include <stdio.h>
# include <X11/Xlib.h>
# include <X11/Xutil.h>
# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <X11/Shell.h>
# include <X11/Xaw/Dialog.h>
# include "dialog.h"

# define min(x, y)                     (((x) < (y)) ? (x) : (y))
# define max(x, y)                     (((x) > (y)) ? (x) : (y))

# define FONTSIZE 14

static void SetSelected (Widget widget, XtPointer clientData, XtPointer callData);
static void SetSelectedOkay(Widget w, XEvent *event, String *params, Cardinal *num_params);

static unsigned selected;

static struct {
    char    *name;
    unsigned flag;
} buttons [ ] = {
    {"yes",    Yes},
    {"no",     No},
    {"okay",   Okay},
    {"cancel", Cancel}
};

static XtActionsRec actiontable [ ] = {
   {"SetSelectedOkay",	SetSelectedOkay},
};

/************************************************************************
 * Function:	SetSelectedOkay 					* 
 ************************************************************************/

static void SetSelectedOkay(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
   selected = Okay;
}

/************************************************************************
 * Function:	 SetSelected						*
 ************************************************************************/

static void SetSelected (Widget widget, XtPointer clientData, XtPointer callData)
{
    selected = *(unsigned *) clientData;
}


/************************************************************************
 * Function:	 CreateDialog						*
 ************************************************************************/

Dialog *CreateDialog (Widget topwidget, char *name, unsigned int options)
{
    int     		i;
    Dialog 		*dialog;
    static Boolean	actions = False;

    if (!(dialog = XtNew (Dialog)))
	return NULL;

    dialog -> topwidget = topwidget;
    dialog -> shellwidget = XtCreatePopupShell (name, transientShellWidgetClass,
					topwidget, NULL, 0);
    dialog -> dialogwidget = XtCreateManagedWidget ("dialog", dialogWidgetClass,
					dialog -> shellwidget, NULL, 0);

    for (i = 0; i < XtNumber (buttons); i ++)
	if (options & buttons [i].flag)
	    XawDialogAddButton (dialog -> dialogwidget, buttons [i].name,
				SetSelected, &buttons [i].flag);

    dialog -> options = options;

    if (!actions) {
       XtAppAddActions (XtWidgetToApplicationContext (topwidget),
			actiontable, XtNumber(actiontable));
       actions = True;
    }

    XtOverrideTranslations (dialog -> shellwidget, XtParseTranslationTable 
                                   ("<Key>Return: SetSelectedOkay()"));

    return dialog;
}


/************************************************************************
 * Function:	 PopupDialog						*
 ************************************************************************/

unsigned PopupDialog (Dialog *dialog, String message, String suggestion, String *answer)
{
    XEvent    		event;
    Cardinal  		count;
    Arg       		arglist [4];
    Dimension 		dialogwidth, dialogheight,
              		topwidth, topheight,
              		borderwidth;
    Position  		dialogx,dialogy,topx,topy;
    XtGeometryResult	geom;
    XtWidgetGeometry	preferred;
    Display   		*display;
    unsigned   		screen;

    count = 0;
    XtSetArg (arglist [count], XtNlabel, message); count ++;
    XtSetArg (arglist [count], XtNvalue, suggestion); count ++;
    XtSetValues (dialog -> dialogwidget, arglist, count);

    if (suggestion != NULL) {
       XtSetArg (arglist [0], XtNwidth, 250); 
       XtSetValues (dialog -> shellwidget, arglist, 1);
    }

    XtRealizeWidget (dialog -> shellwidget);

    geom = XtQueryGeometry (XtNameToWidget(dialog -> dialogwidget, "label"),
                            NULL, &preferred);

    count = 0;
    XtSetArg(arglist[count], XtNx, &topx); count++;
    XtSetArg(arglist[count], XtNy, &topy); count++;
    XtSetArg(arglist[count], XtNwidth, &topwidth); count++;
    XtSetArg(arglist[count], XtNheight, &topheight); count++;
    XtGetValues(dialog -> topwidget, arglist, count);

    count = 0;
    XtSetArg(arglist[count], XtNwidth, &dialogwidth); count++;
    XtSetArg(arglist[count], XtNheight, &dialogheight); count++;
    XtSetArg(arglist[count], XtNborderWidth, &borderwidth); count++;
    XtGetValues(dialog -> shellwidget, arglist, count);

    display = XtDisplay (dialog -> shellwidget);
    screen = DefaultScreen (display);

    dialogx = max(0, 
       min(topx + ((Position)topwidth - (Position)dialogwidth) / 2, 
           (Position)DisplayWidth(display,screen) - 
            (Position)dialogwidth - 2 * (Position)borderwidth));
    dialogy = max(0, 
       min(topy + ((Position)topheight - (Position)dialogheight) / 2,
           (Position)DisplayHeight(display, screen) -
            (Position)dialogheight - 2 * (Position)borderwidth));

    count = 0;
    XtSetArg(arglist[count], XtNx, dialogx); count++;
    XtSetArg(arglist[count], XtNy, dialogy); count++;
    XtSetValues(dialog -> shellwidget, arglist, count);

    if (suggestion == NULL) {
	Dimension shell_width;
	Dimension label_width;
	Arg args [10];
	Widget label;

	label = XtNameToWidget (dialog -> dialogwidget, "label");
/*
       XGetNormalHints (XtDisplay (dialog -> shellwidget), 
                        XtWindow (dialog -> shellwidget), &hints);

	printf ("preferred = %d\n", preferred.width);
       hints.width = preferred.width + 60;
       hints.max_width = hints.width ;
       hints.min_width = hints.width ;

       hints.max_height = hints.height;
       hints.min_height = hints.height;
       hints.flags = PMaxSize | PMinSize;

       XSetNormalHints (XtDisplay (dialog -> shellwidget), 
                        XtWindow (dialog -> shellwidget), &hints);
*/
	XtSetArg (args [0], XtNwidth, &shell_width);
	XtGetValues (dialog -> shellwidget, args, 1);

	XtSetArg (args [0], XtNwidth, &label_width);
	XtGetValues (label, args, 1);

	shell_width += preferred.width - label_width;

	XtSetArg (args [0], XtNwidth, shell_width);
	XtSetValues (dialog -> shellwidget, args, 1);
   }

    XtPopup (dialog -> shellwidget, XtGrabExclusive);
  
    XWarpPointer(XtDisplay(dialog -> shellwidget), 
                 XtWindow(dialog -> topwidget),
	         XtWindow(dialog -> shellwidget), 
	         0, 0, 0, 0,
	         dialogwidth / 2, dialogheight / 2);

    selected = 0;
    while (!(selected & dialog -> options)) {
	XtAppNextEvent (XtWidgetToApplicationContext (dialog -> topwidget),
							&event);
	XtDispatchEvent (&event);
    }

    if (answer)
       *answer = XawDialogGetValueString (dialog -> dialogwidget);

    XtPopdown (dialog -> shellwidget);

    return selected;
}
