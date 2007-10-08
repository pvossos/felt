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
*
* File:		panel.c
*
* Description:	contains all of the functions that constitute callbacks
*		and action procedures from the Velvet main control
*		panel widgets.
*
* History:	by Jason I. Gobat and Darren C. Atkinson
*
*************************************************************************/

# include <stdio.h>
# include <string.h>
# include <stdlib.h>
# include <unistd.h>
# include <X11/Xlib.h>
# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <X11/Xaw/Simple.h>
# include <X11/Xaw/AsciiText.h>
# include <X11/Xaw/Viewport.h>
# include <X11/Xaw/SmeBSB.h>
# include "fe.h"
# include "Canvas.h"
# include "Drawing.h"
# include "procedures.h"
# include "problem.h"
# include "panel.h"
# include "globals.h"
# include "text_entry.h"
# include "vfe.h"
# include "Solution.h"

# define sgn(x) (x < 0 ? -1 : x > 0)

extern CanvasDialog	canvas_d;

extern Panel   panel [ ];


void ToggleSnapStatus (void)
{
    Arg            arglist[1];

    if (canvas -> snap) 
       XtSetArg (arglist[0], XtNrightBitmap, NULL);
    else
       XtSetArg (arglist[0], XtNrightBitmap, checkmark);

    XtSetValues (XtNameToWidget (panel [3].menu, "snap") , arglist, 1);

    canvas -> snap = !canvas -> snap;

    CanvasDialogSet (canvas_d);
    changeflag = True;
}

void ToggleRenumberStatus (void)
{
   Arg 		arglist [1];

   if (solution -> renumber)
      XtSetArg (arglist [0], XtNrightBitmap, NULL);
   else
      XtSetArg (arglist [0], XtNrightBitmap, checkmark);

   XtSetValues (XtNameToWidget (panel [6].menu, "renumber_solve"), arglist, 1);

   solution -> renumber = !solution -> renumber;
}

void SetNodeNumberFlag (void)
{
   Arg		arglist [1];

   if (canvas -> node_numbers)
      XtSetArg (arglist [0], XtNrightBitmap, checkmark);
   else
      XtSetArg (arglist [0], XtNrightBitmap, NULL);

   XtSetValues (XtNameToWidget (panel [3].menu, "node_numbering"), arglist, 1);
}
   
void ToggleNodeNumberStatus (void)
{
   canvas -> node_numbers  = !canvas -> node_numbers;
   SetNodeNumberFlag();
   SetNodeNumbering (canvas -> node_numbers ? True : False);
   CanvasDialogSet (canvas_d);
   changeflag = True;
}

void SetEltNumberFlag(void)
{
   Arg		arglist [1];

   if (canvas -> element_numbers)
      XtSetArg (arglist [0], XtNrightBitmap, checkmark);
   else
      XtSetArg (arglist [0], XtNrightBitmap, NULL);

   XtSetValues (XtNameToWidget (panel [3].menu, "elt_numbering"), arglist, 1);
}

void ToggleEltNumberStatus (void)
{

   canvas -> element_numbers  = !canvas -> element_numbers;
   SetEltNumberFlag ( );
   SetElementNumbering (canvas -> element_numbers ? True : False);
   CanvasDialogSet (canvas_d);
   changeflag = True;
}


void ToggleGridStatus (void)
{
    Arg            arglist[1];

    if (canvas -> grid)
       XtSetArg (arglist[0], XtNrightBitmap, NULL);
    else
       XtSetArg (arglist[0], XtNrightBitmap, checkmark);

    XtSetValues (XtNameToWidget (panel [3].menu, "grid"), arglist, 1);
    
    canvas -> grid = !canvas -> grid;

    CanvasDialogSet (canvas_d);
    changeflag = True;
}

void ZoomAll (void)
{
   float	xscale,
		yscale;
   Dimension	height,
		width;
   Arg		arglist [4];
   Cardinal	count;

   count = 0;
   XtSetArg (arglist [count], XtNheight, &height); count++;
   XtSetArg (arglist [count], XtNwidth, &width); count++;
   XtGetValues (viewport, arglist, count);

   xscale = (float) width / (canvas -> xmax - canvas -> xmin);
   yscale = (float) height / (canvas -> ymax - canvas -> ymin);

   if (xscale < yscale) canvas -> scale = xscale;
   else canvas -> scale = yscale;

   count = 0;
   XtSetArg (arglist [count], XtNxScale, Float2Arg(canvas -> scale)); count++;
   XtSetArg (arglist [count], XtNyScale, Float2Arg(canvas -> scale)); count++;
   XtSetValues (drawing, arglist, count);

   XawViewportSetCoordinates (viewport, 0, 0);
   changeflag = True;
}

void ZoomStart (void)
{
   Cardinal	count;
   Arg		arglist [4];

   SetEditMode ( );

   count = 0;
   XtSetArg (arglist [count], XtNcursorName, "left_ptr"); count++;
   XtSetValues (drawing, arglist, count);

   ChangeStatusLine ("Select first corner:", True);

   XtRemoveAllCallbacks (drawing, XtNbuttonCallback);
   XtAddCallback (drawing, XtNbuttonCallback, ZoomSelect, NULL);
   XtOverrideTranslations (entry, 
        XtParseTranslationTable ("<Key>Return: ZoomAP()"));

   if (DW_SetForeground (drawing, canvas -> tool_color) == False)
	(void) DW_SetForeground (drawing, "black");
}

void ZoomAP (void)
{
   static unsigned	corner_number = 0;
   static float		xl,xr,
			yb,yt;
   Figure		box;
   char*		status;

   if (corner_number == 0) {
      status = GetTextCoordinates (&xl,&yb,NULL); 
      if (status == NULL) {
         corner_number++;
         ChangeStatusLine ("Select second corner:", True);
      }
   } 
   else if (corner_number == 1) {
      status = GetTextCoordinates (&xr,&yt,NULL);
      if (status == NULL) {
         DW_SetInteractive (drawing, True);
         box = DW_DrawRectangle (drawing, True, xl, yb, xr - xl, yt - yb);
         DW_SetInteractive (drawing, False);
         ZoomEnd (box);
         DW_RemoveFigure (drawing, box);
         corner_number = 0;
      }
   }
}

void ZoomSelect (Widget w, XtPointer clientData, XtPointer callData)
{
   static Point		corner;
   static Figure	box; 
   DrawingReport	*report;
   FigureAttributes	attributes;

   report = (DrawingReport *) callData;

   switch (report -> event -> type) {
   case ButtonPress:
      ChangeStatusLine ("- Select second corner -", False);
      corner = report -> unsnapped;
      DW_SetInteractive (w, True);
      box = DW_DrawRectangle (w, True, corner.x, corner.y, 0.0, 0.0);
      break;

   case MotionNotify:
      attributes.width  = report -> unsnapped.x - corner.x;
      attributes.height = report -> unsnapped.y - corner.y;
      DW_SetAttributes (w, box, DW_FigureSize, &attributes);
      break;

   case ButtonRelease:
      DW_SetInteractive (drawing, False);
      ZoomEnd (box);
      DW_RemoveFigure (drawing, box);
      break;
   }
} 

void ZoomEnd (Figure box)
{
   float		xscale;
   float		yscale;
   float		xr,xl,
			yt,yb;
   Dimension		pos_x,
			pos_y;
   Dimension		height,
			width;
   Arg			arglist [4];
   Cardinal		count;
   FigureAttributes	attributes;
   XRectangle		rect;
   
   count = 0;
   XtSetArg (arglist [count], XtNheight, &height); count++;
   XtSetArg (arglist [count], XtNwidth, &width); count++;
   XtGetValues (viewport, arglist, count);

   DW_GetAttributes (drawing, box, &attributes);

   if (attributes.width > 0) {
      xl = attributes.x;
      xr = attributes.width + xl;
   } else {
      xl = attributes.x + attributes.width;
      xr = attributes.x;
   }

   if (attributes.height > 0) {
      yb = attributes.y;
      yt = attributes.height + yb;
   } else {
      yb = attributes.y + attributes.height;
      yt = attributes.y;
   } 
  
   if (xr > xl) 
      xscale = (float) width / (xr - xl);
   else
      return;

   if (yt > yb)
      yscale = (float) height / (yt - yb);
   else
      return;

   if (xscale < yscale) canvas -> scale = xscale;
   else canvas -> scale = yscale;

   count = 0;
   XtSetArg (arglist [count], XtNxScale, Float2Arg(canvas -> scale)); count++;
   XtSetArg (arglist [count], XtNyScale, Float2Arg(canvas -> scale)); count++;
   XtSetValues (drawing, arglist, count); 

   DW_ClipBox (box, &rect);
   pos_x = rect.x;
   pos_y = rect.y;

   SetNormalMode ();

   XawViewportSetCoordinates (viewport, pos_x, pos_y);
   changeflag = True;
}

void SetWaitCursor (Widget w)
{
   Arg		arglist [1];
   Cardinal	count;

   count = 0;
   XtSetArg (arglist [count], XtNcursorName, "watch"); count++;
   XtSetValues (w, arglist, count);

   XFlush (XtDisplay (toplevel));
}

void SetNormalCursor (Widget w)
{
   Arg		arglist [1];
   Cardinal	count;

   count = 0;
   XtSetArg (arglist [count], XtNcursorName, "crosshair"); count++;
   XtSetValues (w, arglist, count);

   XFlush (XtDisplay (toplevel));
}

void ParseEntryLine (void)
{
   extern PanelId     last_command;
   Arg		      arglist [1];
   String             result;
   unsigned	      i;
   XtPointer	      execute;
   extern TextCommand commands [ ];

   XtSetArg (arglist[0], XtNstring, &result);
   XtGetValues (entry, arglist, 1);

   execute = NULL;

   if (strcmp (result, "") == 0) {
      if (last_command != -1)
         execute = (XtPointer) &last_command;
   }
   else {
      execute = NULL;

      i = 0;
      while ((int) commands [i].id > -1) {
         if (!strcmp (commands [i].name, result)) {
            execute = (XtPointer) &commands [i].id;
            last_command = commands [i].id;
            break;
         }
         i ++;
      }
   }

   XtSetArg (arglist[0], XtNstring, "");
   XtSetValues (entry, arglist, 1);

   if (execute != NULL) 
      PanelCallback (NULL, execute, NULL);
   else
      XBell (XtDisplay (toplevel), 20);
}

void MenuAction (Widget w, XEvent *event, String *params, Cardinal *num_params)
{
   XtPointer	execute;
   unsigned	i,j;

   i = 0;
   execute = NULL;
   while (strcmp (panel [i].name, "spacer")) {
      if (panel [i].numentries == 0) {
          if (!strcmp (panel [i].name, *params)) {
             execute = (XtPointer) &panel [i].menuentry[0].id;
             break;
          }
      }
      else {
         for (j = 0 ; j < panel [i].numentries ; j++) {

            if (!strcmp (panel [i].menuentry [j].name, *params)) {
                execute = (XtPointer) &panel [i].menuentry [j].id;
                break;
            }
         }
      }

      if (execute != NULL) break;
      else i++;
   }

   if (execute != NULL)
      PanelCallback (NULL, execute, NULL);
   else 
      fprintf (stderr,"velvet: could not find menu entry named %s\n",*params);
}  

void QuitEdit(Widget widget, XtPointer closure, XtPointer data)
{
   if (edit_mode == True) 
      SetNormalMode ();
}

void AbortEdit(Widget widget, XtPointer closure, XtPointer data)
{
   if (edit_mode == True) 
      SetNormalMode ();
}

void SetNormalMode (void)
{
   unsigned	i;
   Arg		arglist [2];
   Cardinal	count;
   int		depth;

   ChangeStatusLine ("Command:", True);

   XtRemoveAllCallbacks (drawing, XtNbuttonCallback);
   XtRemoveAllCallbacks (drawing, XtNmotionCallback);

   XtSetArg (arglist [0], XtNcursorName, "crosshair");
   XtSetValues (drawing, arglist, 1);

   XtOverrideTranslations (entry, 
        XtParseTranslationTable ("<Key>Return: ParseEntryLine()"));

   count = 0;
   XtSetArg (arglist[count], XtNeditType, XawtextEdit); count++;
   XtSetArg (arglist [count], XtNstring, ""); count++;
   XtSetValues (entry, arglist, 1);

   XawTextDisplayCaret (entry, True);

   XtAddCallback (drawing, XtNbuttonCallback, SelectCallback, NULL);

   AssignQuitAbort (QuitEdit, "QuitEdit", AbortEdit, "AbortEdit");

   if (sensitive_menus == True) {
      for (i = 0 ; i <= 2 ; i++)
         XtSetSensitive (panel [i].button, True);

      for (i = 2 ; i <= 4 ; i++)
         XtSetSensitive (panel [3].menuentry[i].widget, True);

      for (i = 4 ; i <= 7 ; i++) 
         XtSetSensitive (panel [i].button, True);

      XtSetSensitive (quitbutton, False);
      XtSetSensitive (abortbutton, False);
   }

   XtSetArg (arglist[0], XtNdepth, &depth);
   XtGetValues (toplevel, arglist, 1);
   if (depth < 8) {
      XtSetSensitive (panel [2].menuentry[0].widget, False);
      XtSetSensitive (panel [2].menuentry[1].widget, False);
   }

   edit_mode = False;
}

void AssignQuitAbort (XtCallbackProc quitCB, String quitAP, XtCallbackProc abortCB, String abortAP)
{
   char		cmd1 [256];
   char		cmd2 [256];

   XtRemoveAllCallbacks (quitbutton, XtNcallback);
   XtRemoveAllCallbacks (abortbutton, XtNcallback);

   XtAddCallback (quitbutton, XtNcallback, quitCB, NULL);
   XtAddCallback (abortbutton, XtNcallback, abortCB, NULL);

   sprintf (cmd1, "<Key>Escape: %s()", quitAP);
   sprintf (cmd2, "Ctrl<Key>c: %s()", abortAP);

   XtOverrideTranslations (entry,
      XtParseTranslationTable (cmd1));
   XtOverrideTranslations (entry,
      XtParseTranslationTable (cmd2));
}
                                

void SetEditMode (void)
{
   unsigned	i;

   edit_mode = True;

   if (sensitive_menus == True) {
      for (i = 0 ; i <= 2 ; i++)
         XtSetSensitive (panel [i].button, False);

      for (i = 2 ; i <= 4 ; i++)
         XtSetSensitive (panel [3].menuentry[i].widget, False);

      for (i = 4; i <= 7 ; i++)
         XtSetSensitive (panel [i].button, False);

      XtSetSensitive (quitbutton, True);
      XtSetSensitive (abortbutton, True);
   }
}

 
void ChangeStatusLine (String new_label, Boolean allow_input)
{
   Arg		arglist [1];
   Cardinal	count;

   if (!allow_input) {
      count = 0;
      XtSetArg (arglist[count], XtNeditType, XawtextRead); count++;
      XtSetValues (entry, arglist, count);

      XawTextDisplayCaret (entry, False);
   }

   XtUnmanageChild (statusline);
   XtUnmanageChild (entry);

   count = 0;
   XtSetArg (arglist [count], XtNlabel, new_label); count++;
   XtSetValues (statusline, arglist, count);

   XtManageChild (statusline);
   XtManageChild (entry);
}
