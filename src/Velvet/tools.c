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

/*****************************************************************************
 *
 * File:	tools.c
 *
 * Description:	contains functionality for the construction tools routines
 *
 *****************************************************************************/

# include <stdio.h>
# include <math.h>
# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <X11/Xaw/Simple.h>
# include "fe.h"
# include "Drawing.h"
# include "Canvas.h"
# include "procedures.h"
# include "text_entry.h"
# include "allocate.h"
# include "Tree.h"
# include "globals.h"


static unsigned op_count;

static FigureAttributes null_attrib = {0};

Tree figure_tree = NULL;


int figure_cmp (item1, item2)
    Item item1;
    Item item2;
{
    return ((int) item1) - ((int) item2);
}


void ToolsDeleteFigure ()
{
   Arg		arglist [1];

   SetEditMode ( );
   XtSetArg (arglist[0], XtNcursorName, "dotbox");
   XtSetValues (drawing, arglist, 1);

   ChangeStatusLine ("- Select figure -", False);
  
   XtRemoveAllCallbacks (drawing, XtNbuttonCallback);
   XtAddCallback (drawing, XtNbuttonCallback, DeleteToolCB, NULL);
}


static void
DeleteFigureGroup(Figure *figures, unsigned nfigures)
{
    unsigned         i;
    Figure           fig;
    Boolean          firsttime;
    FigureAttributes attr;

    firsttime = True;
    for (i = 0; i < nfigures; i ++) {
	fig = figures [i];
	DW_GetAttributes (drawing, fig, &attr);
	if (attr.user_data != NULL)
	    continue;

	if (firsttime == True) {
	    firsttime = False;
	    DW_SetAutoRedraw (drawing, False);
	}

        TreeDelete (figure_tree, fig);
	DW_RemoveFigure (drawing, fig);
    }

    if (firsttime == False) {
	DW_SetAutoRedraw (drawing, True);
	changeflag = True;
    }

    XtFree ((char *) figures);
}


void DeleteToolCB (w, clientData, callData)
   Widget	w;
   XtPointer	clientData,
		callData;
{
   DrawingReport		*report;
   Figure			figure;
   FigureAttributes		attributes;

   report = (DrawingReport *) callData;

   if (report -> event -> type != ButtonPress)
	return;

   if (report -> event -> xbutton.button == 3)
	QuitEdit ( );

   if (report -> event -> xbutton.button == 2) {
	SelectGroup (callData, DeleteFigureGroup);
	return;
   }

   if (report -> event -> xbutton.button != 1)
	return;
 
   figure = DW_FindFigure (w, report -> unsnapped.x, report -> unsnapped.y);
                              
   if (figure != NULL) {
      DW_GetAttributes (w, figure, &attributes);
      if (attributes.user_data == NULL) {
         DW_RemoveFigure (w, figure);
	 TreeDelete (figure_tree, figure);
      }
   }
}

void ToolsDrawLine ()
{
   Cardinal	count;
   Arg		arglist [2];

   SetEditMode ( );

   count = 0;
   XtSetArg (arglist [count], XtNcursorName, "pencil"); count++;
   XtSetValues (drawing, arglist, count);

   if (!(DW_SetForeground (drawing, canvas -> tool_color)))
      DW_SetForeground (drawing, "black");
  
   ChangeStatusLine ("Select first point:", True);

   op_count = 0;

   XtRemoveAllCallbacks (drawing, XtNbuttonCallback);
   XtAddCallback (drawing, XtNbuttonCallback, DoLineCB, NULL);
   XtOverrideTranslations(entry, 
       XtParseTranslationTable ("<Key>Return: DoLineAP()"));
}

void DoLineAP ()
{
   static float 	xs,xe,
			ys,ye;
   char		 	*status;
   static Figure	line;
  
   if (op_count == 0) {
      status = GetTextCoordinates (&xs, &ys, NULL);
      if (status == NULL) {
         op_count++;
         ChangeStatusLine ("Select second point:", True);
      }
   }
   else if (op_count == 1) {
      status = GetTextCoordinates (&xe, &ye, NULL);
      if (status == NULL) {
         line = DW_DrawLine (drawing, xs, ys, xe, ye);
         DW_SetAttributes (drawing, line, DW_FigureUserData, &null_attrib);
         TreeInsert (figure_tree, line);
         SetNormalMode ();
	 changeflag = True;
         op_count = 0;
      }
   }
}

void DoLineCB (w, clientData, callData)
   Widget	w;
   XtPointer	clientData;
   XtPointer	callData;
{
   static Point			start;
   static Figure		line;
   static FigureAttributes	attributes;
   DrawingReport		*report;

   report = (DrawingReport *) callData;

   switch (report -> event -> type) {
   case ButtonPress:
      start = report -> snapped;
      ChangeStatusLine ("- Select second point -", False);
      DW_SetInteractive (w, True);
      line = DW_DrawLine (w, start.x, start.y, start.x, start.y);
      DW_GetAttributes (w, line, &attributes);
      break;

   case MotionNotify:
      attributes.points [1] = report -> snapped;
      DW_SetAttributes (w, line, DW_FigurePoints, &attributes);
      break;

   case ButtonRelease:
      DW_RemoveFigure (w, line); 
      DW_SetInteractive (w, False);
      line = DW_DrawLine (w, start.x, start.y, report->snapped.x, report->snapped.y);
      DW_SetAttributes (w, line, DW_FigureUserData, &null_attrib);
      TreeInsert (figure_tree, line);
      SetNormalMode ();
      changeflag = True;
      break;
   }
}
   
void ToolsDrawRectangle ()
{
   Cardinal	count;
   Arg		arglist [2];

   SetEditMode ( );

   count = 0;
   XtSetArg (arglist [count], XtNcursorName, "pencil"); count++;
   XtSetValues (drawing, arglist, count);

   if (!(DW_SetForeground (drawing, canvas -> tool_color)))
      DW_SetForeground (drawing, "black");
  
   ChangeStatusLine ("Select first corner:", True);

   op_count = 0;

   XtRemoveAllCallbacks (drawing, XtNbuttonCallback);
   XtAddCallback (drawing, XtNbuttonCallback, DoRectangleCB, NULL);
   XtOverrideTranslations(entry, 
       XtParseTranslationTable ("<Key>Return: DoRectangleAP()"));
}

void DoRectangleAP ()
{
   static float 	xl,xr,
			yb,yt;
   char		 	*status;
   static Figure	rect;
  
   if (op_count == 0) {
      status = GetTextCoordinates (&xl, &yb, NULL);
      if (status == NULL) {
         op_count ++;
         ChangeStatusLine ("Select second corner:", True);
      }
   }
   else if (op_count == 1) {
      status = GetTextCoordinates (&xr, &yt, NULL);
      if (status == NULL) {
         rect = DW_DrawRectangle (drawing, True, xl, yb, xr - xl, yt - yb);
         DW_SetAttributes (drawing, rect, DW_FigureUserData, &null_attrib); 
         SetNormalMode ();
         changeflag = True;
         op_count = 0;
      }
   }
}

void DoRectangleCB (w, clientData, callData)
   Widget	w;
   XtPointer	callData;
   XtPointer	clientData;
{
   static Point			corner;
   static Figure		rect;
   static FigureAttributes	attributes;
   DrawingReport		*report;

   report = (DrawingReport *) callData;

   switch (report -> event -> type) {
   case ButtonPress:
      ChangeStatusLine ("- Select second point -", False);
      corner = report -> snapped;
      DW_SetInteractive (w, True);
      rect = DW_DrawRectangle (w, True, corner.x, corner.y, 0.0, 0.0);
      break;

   case MotionNotify:
      attributes.width  = report -> snapped.x - corner.x;
      attributes.height = report -> snapped.y - corner.y;
      DW_SetAttributes (w, rect, DW_FigureSize, &attributes);
      break;

   case ButtonRelease:
      DW_RemoveFigure (w, rect); 
      DW_SetInteractive (w, False);
      rect = DW_DrawRectangle (w, True, corner.x, corner.y, 
                                 attributes.width, attributes.height);
      DW_SetAttributes (w, rect, DW_FigureUserData, &null_attrib); 
      TreeInsert (figure_tree, rect);
      SetNormalMode ();
      changeflag = True;
      break;
   }
} 

void ToolsDrawCircle ()
{
   Cardinal	count;
   Arg		arglist [2];

   SetEditMode ( );

   count = 0;
   XtSetArg (arglist [count], XtNcursorName, "pencil"); count++;
   XtSetValues (drawing, arglist, count);

   if (!(DW_SetForeground (drawing, canvas -> tool_color)))
      DW_SetForeground (drawing, "black");
  
   ChangeStatusLine ("Select center:", True);

   op_count = 0;

   XtRemoveAllCallbacks (drawing, XtNbuttonCallback);
   XtAddCallback (drawing, XtNbuttonCallback, DoCircleCB, NULL);
   XtOverrideTranslations(entry, 
       XtParseTranslationTable ("<Key>Return: DoCircleAP()"));
}

void DoCircleCB (w, clientData, callData)
   Widget	w;
   XtPointer	callData;
   XtPointer	clientData;
{
   static Point			center;
   static Figure		circle;
   static FigureAttributes	attributes;
   DrawingReport		*report;
   float			radius;

   report = (DrawingReport *) callData;

   switch (report -> event -> type) {
   case ButtonPress:
      ChangeStatusLine ("- Select point on edge -", False);
      center = report -> snapped; 
      DW_SetInteractive (w, True);
      circle = DW_DrawArc (w, True, center.x, center.y, 0.0, 0.0, 0.0, 360.0);
      break;
 
   case MotionNotify:
      radius = sqrt ((report -> snapped.x - center.x)*
                     (report -> snapped.x - center.x) +
                     (report -> snapped.y - center.y)*
                     (report -> snapped.y - center.y));
      attributes.width  = 2.0*radius;
      attributes.height = 2.0*radius;
      DW_SetAttributes (w, circle, DW_FigureSize, &attributes);
      break;

   case ButtonRelease:
      DW_RemoveFigure (w, circle); 
      DW_SetInteractive (w, False);
      circle = DW_DrawArc (w, True, center.x, center.y, 
             attributes.width, attributes.height, 0.0, 360.0);
      DW_SetAttributes (w, circle, DW_FigureUserData, &null_attrib);
      TreeInsert (figure_tree, circle);
      SetNormalMode ();
      changeflag = True;
      break;
   }
}

void DoCircleAP ()
{
   static float 	xc,yc,
			radius;
   char 		*status;
   static Figure	circle;
  
   if (op_count == 0) {
      status = GetTextCoordinates (&xc, &yc, NULL);
      if (status == NULL) {
         op_count++;
         ChangeStatusLine ("Circle radius:", True);
      }
   }
   else if (op_count == 1) {
      status = GetTextCoordinates (&radius, NULL, NULL);
 
      if (status == NULL) {
         circle = DW_DrawArc (drawing, True, xc, yc, 2*radius, 2*radius, 0.0, 360.0);
         DW_SetAttributes (drawing, circle, DW_FigureUserData, &null_attrib);
         TreeInsert (figure_tree, circle);
         SetNormalMode ();
         changeflag = True;
         op_count = 0;
      }
   }
}

void ToolsDrawArc ()
{
   Cardinal	count;
   Arg		arglist [2];

   SetEditMode ( );

   count = 0;
   XtSetArg (arglist [count], XtNcursorName, "pencil"); count++;
   XtSetValues (drawing, arglist, count);

   ChangeStatusLine ("Select first endpoint:", True);

   if (!(DW_SetForeground (drawing, canvas -> tool_color)))
      DW_SetForeground (drawing, "black");
  
   XtRemoveAllCallbacks (drawing, XtNbuttonCallback);
   XtAddCallback (drawing, XtNbuttonCallback, DoArcCB, NULL);
/*
   XtOverrideTranslations(entry, 
       XtParseTranslationTable ("<Key>Return: DoArcAP()"));
*/
}

void DoArcCB (w, clientData, callData)
   Widget	w;
   XtPointer	clientData;
   XtPointer	callData;
{
   static Point			edge;
   static Figure		arc;
   static FigureAttributes	attributes;
   DrawingReport		*report;
   static float			start;
   static float			x;
   static float			y;
   unsigned long		mask;

   report = (DrawingReport *) callData;

   switch (report -> event -> type) {
   case ButtonPress:
      ChangeStatusLine ("- Select second endpoint -", False);
      edge  = report -> snapped; 
      DW_SetInteractive (w, True);
      arc = DW_DrawArc (w, True, edge.x, edge.y, 0.0, 0.0, 0.0, 90.0);
      break;
 
   case MotionNotify:
      x = edge.x - report -> snapped.x;
      y = edge.y - report -> snapped.y;
      if (x >= 0 && y < 0)
         start = 0.0;
      else if (x < 0 && y < 0)
         start = 90.0;
      else if (x >= 0 && y >= 0)
         start = 270.0;
      else if (x < 0 && y >= 0)
         start = 180.0;

      mask = DW_FigureX | DW_FigureSize | DW_FigureArcStart;
      attributes.x = edge.x - x;
      attributes.width = 2 * x;
      attributes.height = 2 * y;
      attributes.arc_start = start;

      DW_SetAttributes (w, arc, mask, &attributes);
      break;

   case ButtonRelease:
      DW_RemoveFigure (w, arc); 
      DW_SetInteractive (w, False);
      arc = DW_DrawArc (w, True, edge.x-x, edge.y, 2*x, 2*y, start, 90.0);
      DW_SetAttributes (w, arc, DW_FigureUserData, &null_attrib);
      TreeInsert (figure_tree, arc);
      SetNormalMode ();
      changeflag = True;
      break;
   }
}

# define MAXLINES 100

static int    currline;
static Figure lines  [MAXLINES];
static FigureAttributes attributes;

void QuitPolygon ( )
{
   int		i;
   Point 	points [100];
   Figure  	poly;

    DW_GetAttributes (drawing, lines [0], &attributes);
    points [0] = attributes.points [0];
    for (i = 0; i < currline; i ++) {
        DW_GetAttributes (drawing, lines [i], &attributes);
        points [i + 1] = attributes.points [1];
        DW_RemoveFigure (drawing, lines [i]);
    }

    if (lines [currline])
       DW_RemoveFigure (drawing, lines [currline]);

    DW_SetInteractive (drawing, False);
    poly = DW_DrawPolygon (drawing, True, points, currline + 1);
    DW_SetAttributes (drawing, poly, DW_FigureUserData, &null_attrib);
    TreeInsert (figure_tree, poly);
    SetNormalMode ( );
    changeflag = True;
}

void AbortPolygon ( )
{
   int		i;

   for (i = 0 ; i <= currline ; i++)
      DW_RemoveFigure (drawing, lines [i]);

   DW_SetInteractive (drawing, False);
   SetNormalMode ( );
}

void ToolsDrawPolygon ()
{
   Cardinal	count;
   Arg		arglist [2];
   int		i;

   SetEditMode ( );

   count = 0;
   XtSetArg (arglist [count], XtNcursorName, "pencil"); count++;
   XtSetValues (drawing, arglist, count);

   if (!(DW_SetForeground (drawing, canvas -> tool_color)))
      DW_SetForeground (drawing, "black");
  
   ChangeStatusLine ("Select first point:", True);
   AssignQuitAbort (QuitPolygon, "QuitPolygon", AbortPolygon, "AbortPolygon");

   currline = -1;
   for (i = 0 ; i < MAXLINES ; i++)
      lines [i] = NULL;

   DW_SetInteractive (drawing, True);
   XtRemoveAllCallbacks (drawing, XtNbuttonCallback);
   XtRemoveAllCallbacks (drawing, XtNmotionCallback);
   XtAddCallback (drawing, XtNmotionCallback, DoPolygonMotionCB, NULL);
   XtAddCallback (drawing, XtNbuttonCallback, DoPolygonButtonCB, NULL);

   XtOverrideTranslations(entry, 
       XtParseTranslationTable ("<Key>Return: DoPolygonAP()"));
}

void DoPolygonAP ()
{
   static float 	xc,yc;
   static float 	xc_prev, yc_prev;
   char 		*status;
  

   status = GetTextCoordinates (&xc, &yc, NULL);
   if (status == NULL) {
      if (currline > -1)
         lines [currline ++] = DW_DrawLine (drawing, xc_prev, yc_prev, xc, yc); 
      else {
         XtRemoveAllCallbacks (drawing, XtNbuttonCallback);
         XtRemoveAllCallbacks (drawing, XtNmotionCallback);
         currline ++;
      }
   
      ChangeStatusLine ("Select next point:", True);
      xc_prev = xc;
      yc_prev = yc;
   }

   return;
}

void DoPolygonMotionCB (w, clientData, callData)
   Widget	w;
   XtPointer	clientData;
   XtPointer	callData;
{
   DrawingReport	*report;


   report = (DrawingReport *) callData;

   if (currline >= 0) {
	attributes.points [1] = report -> snapped;
	DW_SetAttributes (w, lines [currline], DW_FigurePoints, &attributes);
   }
}


void DoPolygonButtonCB (w, clientData, callData)
   Widget	w;
   XtPointer	clientData;
   XtPointer	callData;
{
   int   	  i;
   Point 	  points [100];
   static Figure  poly;
   DrawingReport *report;

   report = (DrawingReport *) callData;

   if (report -> event -> type == ButtonPress) {
	switch (report -> event -> xbutton.button) {
	case 1:
	    lines [++ currline] = DW_DrawLine (w, report -> snapped.x,
		report -> snapped.y, report -> snapped.x, report -> snapped.y);
	    DW_GetAttributes (w, lines [currline], &attributes);
            ChangeStatusLine ("- Select next point -", False);
	    break;

	case 2:
	    DW_GetAttributes (w, lines [0], &attributes);
	    lines [++currline] = DW_DrawLine (w, report -> snapped.x,
	    report -> snapped.y, attributes.points[0].x,attributes.points[0].y);

	case 3:
	    currline ++;
	    DW_GetAttributes (w, lines [0], &attributes);
	    points [0] = attributes.points [0];
	    for (i = 0; i < currline; i ++) {
		DW_GetAttributes (w, lines [i], &attributes);
		points [i + 1] = attributes.points [1];
		DW_RemoveFigure (w, lines [i]);
	    }

	    DW_SetInteractive (w, False);
	    poly = DW_DrawPolygon (w, True, points, currline + 1);
            DW_SetAttributes (w, poly, DW_FigureUserData, &null_attrib);
            TreeInsert (figure_tree, poly);
	    SetNormalMode ( );
            changeflag = True;
	    break;
	}
   }
}

void ToolsDrawText ()
{
   SetEditMode ( );

   ChangeStatusLine ("Text string:", True);

   op_count = 0;

   if (!(DW_SetFont (drawing, canvas -> tool_font)))
      DW_SetFont (drawing, "fixed"); 

   if (!(DW_SetForeground (drawing, canvas -> tool_color)))
      DW_SetForeground (drawing, "black");
  
   XtOverrideTranslations(entry, 
       XtParseTranslationTable ("<Key>Return: DoTextAP()"));
}

void DoTextAP ()
{
   static String	result;
   float		x,y;
   char			*status;
   static Figure	text;
   
   if (op_count == 0) {
      result = GetText ();

      if (result != NULL) {
         ChangeStatusLine ("Select text location:", True);
         XtRemoveAllCallbacks (drawing, XtNbuttonCallback);
         XtAddCallback (drawing, XtNbuttonCallback, DoTextCB, &result);
         op_count ++;
      }
      else 
         SetNormalMode ();
   } else {
      status = GetTextCoordinates (&x,&y, NULL);
      if (status == NULL) {
         text = DW_DrawText (drawing, True, x, y, result);
         DW_SetAttributes (drawing, text, DW_FigureUserData, &null_attrib);
         TreeInsert (figure_tree, text);
         SetNormalMode ();
         changeflag = True;
      }
   }
}

void DoTextCB (w, clientData, callData)
   Widget	w;
   XtPointer	clientData,
		callData;
{
   DrawingReport	*report;
   String		result;
   static Figure	text;
 
   report = (DrawingReport *) callData;
   result = (*(String *) clientData);

   text = DW_DrawText (drawing, True, report -> snapped.x, 
                               report -> snapped.y, result);
   DW_SetAttributes (w, text, DW_FigureUserData, &null_attrib);
   TreeInsert (figure_tree, text);

   SetNormalMode ();
   changeflag = True;
}

static double	startx;
static double	starty;
static double	origx;
static double	origy;
static Boolean	offset_from_points;
static Figure	moving;

void QuitMoveTool ( )
{
    DW_SetInteractive (drawing, False);
    SetNormalMode ( );
}

void AbortMoveTool ( )
{
    unsigned		i;
    FigureAttributes	attr;  
   
    if (offset_from_points) { 
       DW_GetAttributes (drawing, moving, &attr);
       for (i = 1 ; i < attr.npoints ; i++) {
           attr.points [i].x = origx + (attr.points [i].x - attr.points [0].x);
           attr.points [i].y = origy + (attr.points [i].y - attr.points [0].y);
       }
       attr.points [0].x = origx;
       attr.points [0].y = origy;

       DW_SetAttributes (drawing, moving, DW_FigurePoints, &attr);
    }
    else {
       attr.x = origx;
       attr.y = origy;

       DW_SetAttributes (drawing, moving, DW_FigureLocation, &attr);
    } 

    QuitMoveTool ( );
}

static void
WalkToolCB(Widget w, XtPointer client_data, XtPointer call_data)
{
    DrawingReport 	*report;
    Figure	  	figure;
    FigureAttributes	attr;
    unsigned		i;

    report = (DrawingReport *) call_data;
    figure = (Figure) client_data;

    if (report -> event -> type == ButtonPress) {
	if (report -> event -> xbutton.button == 3)
	    QuitMoveTool ( );

	if (report -> event -> xbutton.button > 2)
	    return;

	DW_SetInteractive (drawing, False);

	SetNormalMode ( );
    } 
    else if (report -> event -> type == MotionNotify) {
        if (offset_from_points) {
           DW_GetAttributes (drawing, figure, &attr);
           for (i = 0 ; i < attr.npoints ; i++) {
              attr.points [i].x += (report -> snapped.x - startx); 
              attr.points [i].y += (report -> snapped.y - starty); 
           }
           DW_SetAttributes (drawing, figure, DW_FigurePoints, &attr);
           startx = report -> snapped.x;
           starty = report -> snapped.y;
        }
        else {
	   attr.x = origx + (report -> snapped.x - startx);
   	   attr.y = origy + (report -> snapped.y - starty);
        }
	DW_SetAttributes (drawing, figure, DW_FigureLocation, &attr);
    }
}

static void
DoMoveTool(Figure figure)
{
    SetEditMode ( );
    ChangeStatusLine ("- Select new location for figure -", False);

    AssignQuitAbort (QuitMoveTool, "QuitMoveTool", AbortMoveTool,"AbortMoveTool");

    XtRemoveAllCallbacks (drawing, XtNbuttonCallback);
    XtAddCallback (drawing, XtNbuttonCallback, WalkToolCB, figure);

    XtRemoveAllCallbacks (drawing, XtNmotionCallback);
    XtAddCallback (drawing, XtNmotionCallback, WalkToolCB, figure);

    DW_SetInteractive (drawing, True);

    return;
}

static void
MoveToolCB(Widget w, XtPointer client_data, XtPointer call_data)
{
    DrawingReport   *report;
    FigureAttributes attributes;
    Figure	     figure;

    report = (DrawingReport *) call_data;

    if (report -> event -> type != ButtonPress)
	return;

    if (report -> event -> xbutton.button == 3)
	QuitEdit ( );

    if (report -> event -> xbutton.button != 1)
	return;

    figure = DW_FindFigure (w, report -> unsnapped.x, report -> unsnapped.y);

    if (figure == NULL)
	return;

    DW_GetAttributes (w, figure, &attributes);
    if (attributes.user_data != NULL)
	return;

    startx = report -> snapped.x;
    starty = report -> snapped.y;

    if (attributes.type == LineFigure || 
        attributes.type == PolygonFigure) {
    
       offset_from_points = True;
       origx = attributes.points [0].x;
       origy = attributes.points [0].y;
    }
    else {
       origx = attributes.x;
       origy = attributes.y;
       offset_from_points = False;
    }

    moving = figure;
    DoMoveTool (figure);
}

void MoveTool ( )
{
    Arg		arglist [1];

    SetEditMode ( );
    XtSetArg (arglist[0], XtNcursorName, "dotbox");
    XtSetValues (drawing, arglist, 1);

    ChangeStatusLine (" - Select figure -", False);

    XtRemoveAllCallbacks (drawing, XtNbuttonCallback);
    XtAddCallback (drawing, XtNbuttonCallback, MoveToolCB, NULL);
}
