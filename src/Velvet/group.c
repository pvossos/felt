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

/**************************************************************************
* 
* File:		group.c
*
* Description:	contains various utility functions for selecting groups
*		of objects from the drawing area.
*
**************************************************************************/

# include <stdio.h>
# include <string.h>
# include <stdlib.h>
# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <X11/Xaw/Simple.h>
# include <X11/Shell.h>
# include "Canvas.h"
# include "fe.h"
# include "text_entry.h"
# include "Drawing.h"
# include "Tree.h"
# include "globals.h"
# include "procedures.h"

static void    (*function) (Figure *, unsigned);


static void
DoSelectGroup(Figure box, float x1, float y1, float x2, float y2)
{
    Point    points [4];
    Figure  *figures;
    unsigned nfigures;


    points [0].x = x1;
    points [0].y = y1;
    points [1].x = x1;
    points [1].y = y2;
    points [2].x = x2;
    points [2].y = y2;
    points [3].x = x2;
    points [3].y = y1;

    figures = DW_SearchArea (drawing, points, 4, &nfigures);
    function (figures, nfigures);
    DW_RemoveFigure (drawing, box);
    SetNormalMode ( );
}


static void
SelectGroupCB(Widget w, XtPointer client_data, XtPointer call_data)
{
    static Point     corner;
    static Figure    box;
    DrawingReport   *report;
    FigureAttributes attributes;


    report = (DrawingReport *) call_data;

    switch (report -> event -> type) {
    case ButtonPress:
	if (report -> event -> xbutton.button > 2) {
	    SetNormalMode ( );
	    return;
	}

	corner = report -> unsnapped;
	ChangeStatusLine ("- Select second corner -", False);
	DW_SetInteractive (w, True);
	box = DW_DrawRectangle (w, True, corner.x, corner.y, 0.0, 0.0);
	break;

    case MotionNotify:
	attributes.width = report -> snapped.x - corner.x;
	attributes.height = report -> unsnapped.y - corner.y;
	DW_SetAttributes (w, box, DW_FigureSize, &attributes);
	break;

    case ButtonRelease:
	DW_SetInteractive (w, False);
	DoSelectGroup (box, corner.x, corner.y, report -> unsnapped.x,
			report -> unsnapped.y);
	break;
    }
}


void SelectGroupAP (void)
{
    static unsigned corner_number = 0;
    static float    xl, xr, yb, yt;
    Figure          box;
    char           *status;


    if (corner_number == 0) {
	status = GetTextCoordinates (&xl, &yb, NULL);
	if (status == NULL) {
	    corner_number ++;
	    ChangeStatusLine ("Select second corner:", True);
	}
    } else if (corner_number == 1) {
	status = GetTextCoordinates (&xr, &yt, NULL);
	if (status == NULL) {
	    corner_number = 0;
	    box = DW_DrawRectangle (drawing, True, xl, yb, xr - xl, yt - yb);
	    DoSelectGroup (box, xl, yb, xr, yt);
	}
    }
}


void SelectGroup (XtPointer call_data, void (*op) (Figure *, unsigned))
{
    Arg arglist [1];


    function = op;

    XtSetArg (arglist [0], XtNcursorName, "left_ptr");
    XtSetValues (drawing, arglist, 1);

    ChangeStatusLine ("Select first corner:", True);

    XtRemoveAllCallbacks (drawing, XtNbuttonCallback);
    XtAddCallback (drawing, XtNbuttonCallback, SelectGroupCB, NULL);

    XtOverrideTranslations (entry,
	XtParseTranslationTable ("<Key>Return: SelectGroupAP()"));

    if (DW_SetForeground (drawing, canvas -> tool_color) == False)
	(void) DW_SetForeground (drawing, "black");

    if (call_data != NULL)
	SelectGroupCB (drawing, NULL, call_data);
}
