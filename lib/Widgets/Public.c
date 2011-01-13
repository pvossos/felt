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
 * File:	Public.c						*
 *									*
 * Description:	This file contains the public function definitions for	*
 *		the Drawing widget.					*
 ************************************************************************/

# include <stdio.h>
# include <math.h>
# include <string.h>
# include <X11/IntrinsicP.h>
# include "DrawingP.h"

# define PUSH 0
# define POP  1
# define Inside(a,x,b)	((a) <= (x) && (x) <= (b))
# define Sqr(x)		((x) * (x))


/************************************************************************
   Function:	SetColor
   Description:	Sets the cached color of the widget or a figure.
 ************************************************************************/

static Boolean SetColor (DrawingWidget dw, CacheData *old_data, String name, Pixel *pixel)
{
    Display  *display;
    CacheData data;
    XColor    color;


    display = XtDisplay ((Widget) dw);

    if ((data = DW_CacheLookup (dw -> drawing.color_cache, name)) != NULL) {
	if (*old_data != data) {
	    DW_CacheDelRef (*old_data);
	    DW_CacheAddRef (*old_data = data);
	}
	*pixel = (Pixel) data -> value;
	return True;
    }

    if (!XParseColor (display, dw -> core.colormap, name, &color))
	return False;

    if (!XAllocColor (display, dw -> core.colormap, &color))
	return False;

    *pixel = color.pixel;
    data = DW_CacheInsert (dw -> drawing.color_cache, name, (XtArgVal) *pixel);
    DW_CacheDelRef (*old_data);
    *old_data = data;
    return True;
}


/************************************************************************
   Function:	SetFont
   Description:	Sets the cached font of the widget or a figure.
 ************************************************************************/

static Boolean SetFont (DrawingWidget dw, CacheData *old_data, String name, XFontStruct **font)
{
    Display  *display;
    CacheData data;


    display = XtDisplay ((Widget) dw);

    if ((data = DW_CacheLookup (dw -> drawing.font_cache, name)) != NULL) {
	if (*old_data != data) {
	    DW_CacheDelRef (*old_data);
	    DW_CacheAddRef (*old_data = data);
	}
	*font = (XFontStruct *) data -> value;
	return True;
    }


    if ((*font = XLoadQueryFont (display, name)) == NULL)
	return False;

    data = DW_CacheInsert (dw -> drawing.font_cache, name, (XtArgVal) *font);
    DW_CacheDelRef (*old_data);
    *old_data = data;
    return True;
}


/************************************************************************
   Function:	StackAutoRedraw
   Description:	Stacks the auto redraw status for group operations.
 ************************************************************************/

static void StackAutoRedraw (Widget gw, int op)
{
   static unsigned depth = 0;
   static Boolean  redraw;


   if (op == PUSH && !depth ++) {
	redraw = ((DrawingWidget) gw) -> drawing.redraw;
	DW_SetAutoRedraw (gw, False);
   }

   if (op == POP && ! -- depth)
	DW_SetAutoRedraw (gw, redraw);
}


/************************************************************************
   Function:	GroupLeader
   Description:	Return the group leader of a figure.
 ************************************************************************/

static Figure GroupLeader (Figure fig)
{
    while (fig -> group != NULL)
	fig = fig -> group;

    return fig;
}


/************************************************************************
   Function:	DW_DrawLine
   Description: Adds a line figure to the display list.
 ************************************************************************/

Figure DW_DrawLine (Widget gw, float x1, float y1, float x2, float y2)
{
    Figure	  fig;
    DrawingWidget dw;


    dw = (DrawingWidget) gw;
    fig = DW_CreateFigure (dw, LineFigure, False, 2);

    fig -> info.line.points [0].x = x1;
    fig -> info.line.points [0].y = y1;
    fig -> info.line.points [1].x = x2;
    fig -> info.line.points [1].y = y2;

    DW_AppendFigure (dw, fig);
    DW_ScaleFigure  (dw, fig);
    DW_DrawFigure   (dw, fig);

    return fig;
}


/************************************************************************
   Function:	DW_DrawPolygon
   Description: Adds a polygon figure to the display list.
 ************************************************************************/

Figure DW_DrawPolygon (Widget gw, Boolean scaled, Point *points, int npoints)
{
    int		  i;
    Figure	  fig;
    DrawingWidget dw;


    dw = (DrawingWidget) gw;
    fig = DW_CreateFigure (dw, PolygonFigure, scaled, npoints);

    for (i = 0; i < npoints; i ++)
	fig -> info.polygon.points [i] = points [i];

    DW_AppendFigure (dw, fig);
    DW_ScaleFigure  (dw, fig);
    DW_DrawFigure   (dw, fig);

    return fig;
}


/************************************************************************
   Function:	DW_FillPolygon
   Description: Adds a filled polygon figure to the display list.
 ************************************************************************/

Figure DW_FillPolygon (Widget gw, Boolean scaled, Point *points, int npoints)
{
    int		  i;
    Figure	  fig;
    DrawingWidget dw;


    dw = (DrawingWidget) gw;
    fig = DW_CreateFigure (dw, PolygonFigure, scaled, npoints);
    fig -> info.polygon.filled = True;

    for (i = 0; i < npoints; i ++)
	fig -> info.polygon.points [i] = points [i];

    DW_AppendFigure (dw, fig);
    DW_ScaleFigure  (dw, fig);
    DW_DrawFigure   (dw, fig);

    return fig;
}

/************************************************************************
   Function:	DW_DrawRectangle
   Description: Adds a rectangle figure to the display list.
 ************************************************************************/

Figure DW_DrawRectangle (Widget gw, Boolean scaled, float x, float y, float width, float height)
{
    Figure	  fig;
    DrawingWidget dw;


    dw = (DrawingWidget) gw;
    fig = DW_CreateFigure (dw, RectangleFigure, scaled, 0);

    fig -> info.rectangle.x      = x;
    fig -> info.rectangle.y      = y;
    fig -> info.rectangle.width  = width;
    fig -> info.rectangle.height = height;

    DW_AppendFigure (dw, fig);
    DW_ScaleFigure  (dw, fig);
    DW_DrawFigure   (dw, fig);

    return fig;
}


/************************************************************************
   Function:	DW_FillRectangle
   Description: Adds a filled rectangle figure to the display list.
 ************************************************************************/

Figure DW_FillRectangle (Widget gw, Boolean scaled, float x, float y, float width, float height)
{
    Figure	  fig;
    DrawingWidget dw;


    dw = (DrawingWidget) gw;
    fig = DW_CreateFigure (dw, RectangleFigure, scaled, 1);

    fig -> info.rectangle.x      = x;
    fig -> info.rectangle.y      = y;
    fig -> info.rectangle.width  = width;
    fig -> info.rectangle.height = height;

    DW_AppendFigure (dw, fig);
    DW_ScaleFigure  (dw, fig);
    DW_DrawFigure   (dw, fig);

    return fig;
}


/************************************************************************
   Function:	DW_DrawArc
   Description: Adds an arc figure to the display list.
 ************************************************************************/

Figure DW_DrawArc (Widget gw, Boolean scaled, float x, float y, float width, float height, float start, float length)
{
    Figure	  fig;
    DrawingWidget dw;


    dw = (DrawingWidget) gw;
    fig = DW_CreateFigure (dw, ArcFigure, scaled, 0);

    fig -> info.arc.x      = x;
    fig -> info.arc.y      = y;
    fig -> info.arc.width  = width;
    fig -> info.arc.height = height;
    fig -> info.arc.start  = start * 64;
    fig -> info.arc.length = length * 64;

    DW_AppendFigure (dw, fig);
    DW_ScaleFigure  (dw, fig);
    DW_DrawFigure   (dw, fig);

    return fig;
}


/************************************************************************
   Function:	DW_FillArc
   Description: Adds an filled arc figure to the display list.
 ************************************************************************/

Figure DW_FillArc (Widget gw, Boolean scaled, float x, float y, float width, float height, float start, float length)
{
    Figure	  fig;
    DrawingWidget dw;


    dw = (DrawingWidget) gw;
    fig = DW_CreateFigure (dw, ArcFigure, scaled, 1);

    fig -> info.arc.x      = x;
    fig -> info.arc.y      = y;
    fig -> info.arc.width  = width;
    fig -> info.arc.height = height;
    fig -> info.arc.start  = start * 64;
    fig -> info.arc.length = length * 64;

    DW_AppendFigure (dw, fig);
    DW_ScaleFigure  (dw, fig);
    DW_DrawFigure   (dw, fig);

    return fig;
}


/************************************************************************
   Function:	DW_DrawText
   Description: Adds a text figure to the display list.
 ************************************************************************/

Figure DW_DrawText (Widget gw, Boolean scaled, float x, float y, String text)
{
    Figure	  fig;
    DrawingWidget dw;


    dw = (DrawingWidget) gw;
    fig = DW_CreateFigure (dw, TextFigure, scaled, 0);

    fig -> info.text.rx     = x;
    fig -> info.text.ry     = y;
    fig -> info.text.string = XtNewString (text);
    fig -> info.text.length = strlen (text);

    DW_AppendFigure (dw, fig);
    DW_ScaleFigure  (dw, fig);
    DW_DrawFigure   (dw, fig);

    return fig;
}


/************************************************************************
   Function:	DW_DrawPixmap
   Description:	Adds a pixmap figure to the display list.  The pixmap is
		is neither copied nor scaled.
 ************************************************************************/

Figure DW_DrawPixmap (Widget gw, float x, float y, Pixmap pixmap)
{
    Figure        fig;
    DrawingWidget dw;


    dw = (DrawingWidget) gw;
    fig = DW_CreateFigure (dw, PixmapFigure, False, 0);

    fig -> info.pixmap.x      = x;
    fig -> info.pixmap.y      = y;
    fig -> info.pixmap.pixmap = pixmap;

    DW_AppendFigure (dw, fig);
    DW_ScaleFigure  (dw, fig);
    DW_DrawFigure   (dw, fig);

    return fig;
}


/************************************************************************
   Function:	DW_DrawBitmap
   Description:	Adds a bitmap figure to the display list.  The pixmap is
		is neither copied nor scaled.  The difference between
		a Pixmap and a Bitmap is that a Pixmap is copied into
		the window while a Bitmap is rendered such that only the
		pixels which are set are displayed in the foreground
		color and the pixels which are not set are ignored.
 ************************************************************************/

Figure DW_DrawBitmap (Widget gw, float x, float y, Pixmap pixmap)
{
    Figure        fig;
    DrawingWidget dw;


    dw = (DrawingWidget) gw;
    fig = DW_CreateFigure (dw, BitmapFigure, False, 0);

    fig -> info.pixmap.x      = x;
    fig -> info.pixmap.y      = y;
    fig -> info.pixmap.pixmap = pixmap;

    DW_AppendFigure (dw, fig);
    DW_ScaleFigure  (dw, fig);
    DW_DrawFigure   (dw, fig);

    return fig;
}


/************************************************************************
   Function:	DW_SetForeground
   Description:	Sets the foreground pixel value for drawing.
 ************************************************************************/

Boolean DW_SetForeground (Widget gw, String name)
{
    DrawingWidget dw;


    dw = (DrawingWidget) gw;
    return SetColor (dw, &dw -> drawing.color_data, name, &dw -> drawing.fg);
}


/************************************************************************
   Function:	DW_SetFont
   Description:	Sets the font for drawing.
 ************************************************************************/

Boolean DW_SetFont (Widget gw, String name)
{
    DrawingWidget dw;


    dw = (DrawingWidget) gw;
    return SetFont (dw, &dw -> drawing.font_data, name, &dw -> drawing.font);
}

/************************************************************************
   Function:	DW_GetTextExtents
   Description:	Gets the scaled width and height of a text string
 ************************************************************************/

void DW_GetTextExtents (Widget gw, String string, float *w, float *h)
{
    DrawingWidget dw;
    int	          far;
    int           dr;
    int           fdr;
    XCharStruct   cstruct;	

    dw = (DrawingWidget) gw;

    XTextExtents (dw -> drawing.font, string, strlen (string), 
                  &dr, &far, &fdr, &cstruct);

    *w = cstruct.width / dw -> drawing.xScale;
    *h = (cstruct.ascent + cstruct.descent) / dw -> drawing.yScale;
} 

/************************************************************************
   Function:	DW_SetLineWidth
   Description:	Sets the line width for drawing.
 ************************************************************************/

void DW_SetLineWidth (Widget gw, unsigned int width)
{
    DrawingWidget dw;


    dw = (DrawingWidget) gw;
    dw -> drawing.line_width = width;
}


/************************************************************************
   Function:	DW_SetLineStyle
   Description:	Sets the line style for drawing.
 ************************************************************************/

void DW_SetLineStyle (Widget gw, int style)
{
    DrawingWidget dw;


    if (style >= DW_LineSolid && style <= DW_LineLongDashed) {
	dw = (DrawingWidget) gw;
	dw -> drawing.line_style = style;
    }
}


/************************************************************************
   Function:	DW_RaiseFigure
   Description:	Place the figure at the top of the display list.
 ************************************************************************/

void DW_RaiseFigure (Widget gw, Figure fig)
{
    unsigned i;


    if (fig != NULL) {
	if (fig -> type == GroupFigure)
	    for (i = 0; i < fig -> info.group.nfigs; i ++)
		DW_RaiseFigure (gw, fig -> info.group.fig [i]);
	else {
	    DW_DeleteFigure ((DrawingWidget) gw, fig);
	    DW_AppendFigure ((DrawingWidget) gw, fig);
	    DW_DrawFigure   ((DrawingWidget) gw, fig);
	}
    }
}


/************************************************************************
   Function:	DW_LowerFigure
   Description:	Place the figure at the bottom of the display list.
 ************************************************************************/

void DW_LowerFigure (Widget gw, Figure fig)
{
    unsigned i;


    if (fig != NULL) {
	if (fig -> type == GroupFigure) {
	    StackAutoRedraw (gw, PUSH);
	    for (i = 0; i < fig -> info.group.nfigs; i ++)
		DW_LowerFigure (gw, fig -> info.group.fig [i]);
	    StackAutoRedraw (gw, POP);
	} else {
	    DW_DeleteFigure  ((DrawingWidget) gw, fig);
	    DW_PrependFigure ((DrawingWidget) gw, fig);
	    DW_ClearFigure   ((DrawingWidget) gw, fig);
	}
    }
}


/************************************************************************
   Function:	DW_RemoveFigure
   Description:	Remove the figure from the display list.
 ************************************************************************/

void DW_RemoveFigure (Widget gw, Figure fig)
{
    unsigned i;


    if (fig != NULL) {
	if (fig -> type == GroupFigure) {
	    StackAutoRedraw (gw, PUSH);
	    for (i = 0; i < fig -> info.group.nfigs; i ++)
		DW_RemoveFigure (gw, fig -> info.group.fig [i]);
	    StackAutoRedraw (gw, POP);
	} else {
	    DW_DeleteFigure  ((DrawingWidget) gw, fig);
	    DW_ClearFigure   ((DrawingWidget) gw, fig);
	}
	DW_DestroyFigure (fig);
    }
}


/************************************************************************
   Function:	DW_RemoveAll
   Descripton:	Remove all figures from the display list.
 ************************************************************************/

void DW_RemoveAll (Widget gw)
{
    Figure        fig;
    Figure	  next;
    DrawingWidget dw;


    dw = (DrawingWidget) gw;
    fig = dw -> drawing.head;

    while (fig != NULL) {
	next = fig -> next;
	DW_DestroyFigure (fig);
	fig = next;
    }

    dw -> drawing.head = NULL;
    dw -> drawing.tail = NULL;

    XClearArea (XtDisplay (gw), XtWindow (gw), 0, 0, dw -> drawing.width,
		dw -> drawing.height, True);
}


/************************************************************************
   Function:	DW_Redraw
   Description:	Force a total redraw of the widget.
 ************************************************************************/

void DW_Redraw (Widget gw)
{
    DrawingWidget dw;


    dw = (DrawingWidget) gw;
    XClearArea (XtDisplay (gw), XtWindow (gw), 0, 0, dw -> drawing.width,
		dw -> drawing.height, True);
}


/************************************************************************
   Function:	DW_GetAttributes
   Description: Get the attributes of a figure.
 ************************************************************************/

void DW_GetAttributes (Widget gw, Figure fig, FigureAttributes *values)
{
    if (fig == NULL || values == NULL)
	return;


    values -> type	 = fig -> type;
    values -> color	 = fig -> color_data -> name;
    values -> user_data  = fig -> userdata;
    values -> visible	 = fig -> visible;
    values -> group	 = fig -> group;


    switch (fig -> type) {
    case LineFigure:
	values -> line_width = fig -> line_width;
	values -> line_style = fig -> line_style;
	values -> points     = fig -> info.line.points;
	values -> npoints    = 2;
	break;


    case PolygonFigure:
	values -> line_width = fig -> line_width;
	values -> line_style = fig -> line_style;
	values -> filled     = fig -> info.polygon.filled;
	values -> scaled     = fig -> info.polygon.scaled;
	values -> points     = fig -> info.polygon.points;
	values -> npoints    = fig -> info.polygon.npoints;
	break;


    case RectangleFigure:
	values -> line_width = fig -> line_width;
	values -> line_style = fig -> line_style;
	values -> filled     = fig -> info.rectangle.filled;
	values -> scaled     = fig -> info.rectangle.scaled;
	values -> x	     = fig -> info.rectangle.x;
	values -> y	     = fig -> info.rectangle.y;
	values -> width	     = fig -> info.rectangle.width;
	values -> height     = fig -> info.rectangle.height;
	break;


    case ArcFigure:
	values -> line_width = fig -> line_width;
	values -> line_style = fig -> line_style;
	values -> filled     = fig -> info.arc.filled;
	values -> scaled     = fig -> info.arc.scaled;
	values -> x	     = fig -> info.arc.x;
	values -> y	     = fig -> info.arc.y;
	values -> width	     = fig -> info.arc.width;
	values -> height     = fig -> info.arc.height;
	values -> arc_start  = fig -> info.arc.start / 64.0;
	values -> arc_length = fig -> info.arc.length / 64.0;
	break;


    case TextFigure:
	values -> scaled      = fig -> info.text.scaled;
	values -> x	      = fig -> info.text.rx;
	values -> y	      = fig -> info.text.ry;
	values -> text	      = fig -> info.text.string;
	values -> font	      = fig -> info.text.font_data -> name;
	values -> font_struct = fig -> info.text.font;
	break;


    case GroupFigure:
	values -> figures  = fig -> info.group.fig;
	values -> nfigures = fig -> info.group.nfigs;
	break;


    case PixmapFigure:
    case BitmapFigure:
	values -> x      = fig -> info.pixmap.x;
	values -> y      = fig -> info.pixmap.y;
	values -> pixmap = fig -> info.pixmap.pixmap;
	break;
    }
}


/************************************************************************
   Function:	DW_SetAttributes
   Description: Set the attributes of a figure.
 ************************************************************************/

Boolean DW_SetAttributes (Widget gw, Figure fig, long unsigned int valuemask, FigureAttributes *values)
{
    int		  i;
    int		  style;
    Boolean	  redraw;
    Boolean	  status;
    Region	  buffer;
    XRectangle	  rect;
    DrawingWidget dw;
    struct figure copy;


    if (fig == NULL || values == NULL)
	return True;


    if (fig -> type == GroupFigure) {
	for (i = 0; i < fig -> info.group.nfigs; i ++)
	    (void) DW_SetAttributes (gw, fig -> info.group.fig [i], valuemask,
									values);
	return True;
    }


    copy = *fig;
    redraw = False;
    status = True;
    dw = (DrawingWidget) gw;
    buffer = dw -> drawing.bufferRegion;


    if (valuemask & DW_FigureGroup) {
	DW_Detach (dw, fig);
	DW_Attach (dw, fig, values -> group);
    }


    if (valuemask & DW_FigureLineWidth) {
	switch (fig -> type) {
	case LineFigure:
	case PolygonFigure:
	case RectangleFigure:
	case ArcFigure:
	    redraw = True;
	    fig -> line_width = values -> line_width;
	    break;
	default:
	    status = False;
	}
    }


    if (valuemask & DW_FigureLineStyle) {
	style = values -> line_style;
	if (style >= DW_LineSolid && style <= DW_LineLongDashed) {
	    switch (fig -> type) {
	    case LineFigure:
	    case PolygonFigure:
	    case RectangleFigure:
	    case ArcFigure:
		redraw = True;
		fig -> line_style = values -> line_style;
		break;
	    default:
		status = False;
	    }
	} else
	    status = False;
    }


    if (valuemask & DW_FigureVisible) {
	redraw = fig -> visible != values -> visible ? True : False;
	fig -> visible = values -> visible;
    }


    if (valuemask & DW_FigureFilled) {
	switch (fig -> type) {
	case PolygonFigure:
	    redraw = True;
	    fig -> info.polygon.filled = values -> filled;
	    break;
	case RectangleFigure:
	    redraw = True;
	    fig -> info.rectangle.filled = values -> filled;
	    break;
	case ArcFigure:
	    redraw = True;
	    fig -> info.arc.filled = values -> filled;
	    break;
	default:
	    status = False;
	}
    }


    if (valuemask & DW_FigureScaled) {
	switch (fig -> type) {
	case PolygonFigure:
	    redraw = True;
	    fig -> info.polygon.scaled = values -> scaled;
	    break;
	case RectangleFigure:
	    redraw = True;
	    fig -> info.rectangle.scaled = values -> scaled;
	    break;
	case ArcFigure:
	    redraw = True;
	    fig -> info.arc.scaled = values -> scaled;
	    break;
	case TextFigure:
	    redraw = True;
	    fig -> info.text.scaled = values -> scaled;
	    break;
	default:
	    status = False;
	}
    }


    if (valuemask & DW_FigureColor) {
	redraw = True;
	if (SetColor (dw, &fig -> color_data, values -> color, &fig -> fg))
	    status = False;
    }


    if (valuemask & DW_FigureFont) {
	switch (fig -> type) {
	case TextFigure:
	    redraw = True;
	    if (SetFont (dw, &fig -> info.text.font_data, values -> font,
	    &fig -> info.text.font))
		status = False;
	    break;
	default:
	    status = False;
	}
    }


    if (valuemask & DW_FigureText) {
	switch (fig -> type) {
	case TextFigure:
	    redraw = True;
	    fig -> info.text.string = XtNewString (values -> text);
	    fig -> info.text.length = strlen (values -> text);
	    break;
	default:
	    status = False;
	}
    }


    if (valuemask & DW_FigureX) {
	switch (fig -> type) {
	case RectangleFigure:
	    redraw = True;
	    fig -> info.rectangle.x = values -> x;
	    break;
	case ArcFigure:
	    redraw = True;
	    fig -> info.arc.x = values -> x;
	    break;
	case TextFigure:
	    redraw = True;
	    fig -> info.text.rx = values -> x;
	    break;
	case PixmapFigure:
	case BitmapFigure:
	    redraw = True;
	    fig -> info.pixmap.x = values -> x;
	    break;
	default:
	    status = False;
	}
    }


    if (valuemask & DW_FigureY) {
	switch (fig -> type) {
	case RectangleFigure:
	    redraw = True;
	    fig -> info.rectangle.y = values -> y;
	    break;
	case ArcFigure:
	    redraw = True;
	    fig -> info.arc.y = values -> y;
	    break;
	case TextFigure:
	    redraw = True;
	    fig -> info.text.ry = values -> y;
	    break;
	case PixmapFigure:
	case BitmapFigure:
	    redraw = True;
	    fig -> info.pixmap.y = values -> y;
	    break;
	default:
	    status = False;
	}
    }


    if (valuemask & DW_FigureWidth) {
	switch (fig -> type) {
	case RectangleFigure:
	    redraw = True;
	    fig -> info.rectangle.width = values -> width;
	    break;
	case ArcFigure:
	    redraw = True;
	    fig -> info.arc.width = values -> width;
	    break;
	default:
	    status = False;
	}
    }


    if (valuemask & DW_FigureHeight) {
	switch (fig -> type) {
	case RectangleFigure:
	    redraw = True;
	    fig -> info.rectangle.height = values -> height;
	    break;
	case ArcFigure:
	    redraw = True;
	    fig -> info.arc.height = values -> height;
	    break;
	default:
	    status = False;
	}
    }


    if (valuemask & DW_FigureArcStart) {
	switch (fig -> type) {
	case ArcFigure:
	    redraw = True;
	    fig -> info.arc.start = values -> arc_start * 64;
	    break;
	default:
	    status = False;
	}
    }


    if (valuemask & DW_FigureArcLength) {
	switch (fig -> type) {
	case ArcFigure:
	    redraw = True;
	    fig -> info.arc.length = values -> arc_length * 64;
	    break;
	default:
	    status = False;
	}
    }


    if (valuemask & DW_FigurePoints) {
	switch (fig -> type) {
	case LineFigure:
	    redraw = True;
	    fig -> info.line.points [0] = values -> points [0];
	    fig -> info.line.points [1] = values -> points [1];
	    break;
	case PolygonFigure:
	    redraw = True;
	    break;
	default:
	    status = False;
	}
    }


    if (valuemask & DW_FigurePixmap) {
	switch (fig -> type) {
	case PixmapFigure:
	case BitmapFigure:
	    redraw = True;
	    fig -> info.pixmap.pixmap = values -> pixmap;
	    break;
	default:
	    status = False;
	}
    }


    if (valuemask & DW_FigureUserData)
	fig -> userdata = values -> user_data;


    if (redraw == True) {
	if (dw -> drawing.interactive == True)
	    DW_DrawFigure (dw, &copy);
	else {
	    DW_ClipBox (&copy, &rect);
	    rect.x -= copy.line_width + 1;
	    rect.y -= copy.line_width + 1;
	    rect.width += 2 * copy.line_width + 2;
	    rect.height += 2 * copy.line_width + 2;
	    XUnionRectWithRegion (&rect, buffer, buffer);
	}


	if (valuemask & DW_FigureText)
	    XtFree (copy.info.text.string);

	if (valuemask & DW_FigurePoints && fig -> type == PolygonFigure) {
	    fig -> info.polygon.points  = values -> points;
	    fig -> info.polygon.npoints = values -> npoints;
	}


	DW_ScaleFigure (dw, fig);

	if (dw -> drawing.interactive == True)
	    DW_DrawFigure  (dw, fig);
	else {
	    DW_ClipBox (fig, &rect);
	    rect.x -= fig -> line_width + 1;
	    rect.y -= fig -> line_width + 1;
	    rect.width += 2 * fig -> line_width + 2;
	    rect.height += 2 * fig -> line_width + 2;
	    XUnionRectWithRegion (&rect, buffer, buffer);

	    if (dw -> drawing.redraw == True) {
		XClipBox (buffer, &rect);
		XClearArea (XtDisplay (gw), XtWindow (gw), rect.x, rect.y,
			    rect.width, rect.height, True);
		XIntersectRegion (dw -> drawing.nullRegion, buffer, buffer);
	    }
	}
    }

    return status;
}
    

/************************************************************************
   Function:	online
   Description: Determines if a point lies close to a line.
 ************************************************************************/

static int online (XPoint start, XPoint end, int x, int y, int delta)
{
    int deltax;
    int deltay;
    int distx;
    int disty;
    int temp;


    deltax = start.x - end.x;
    deltay = start.y - end.y;
    distx  = x - start.x;
    disty  = y - start.y;

    if (Inside (-4, deltax, 4))
	return Inside (-6, distx, 6);

    temp = deltay * distx / deltax;
    return Inside (temp - delta, disty, temp + delta);
}


/************************************************************************
   Function:	DW_FindFigure
   Description:	Find the topmost figure containing the specified
		coordinates.
 ************************************************************************/

Figure DW_FindFigure (Widget gw, float realx, float realy)
{
    Figure	  fig;
    int		  i;
    int		  x;
    int		  y;
    float	  theta;
    float	  cx;
    float	  cy;
    float	  rx;
    float	  ry;
    int		  fx;
    int		  fy;
    unsigned	  wd;
    unsigned	  ht;
    unsigned	  hw;
    DrawingWidget dw;


    dw = (DrawingWidget) gw;
    fig = dw -> drawing.tail;

    x = XAbs (realx);
    y = YAbs (realy);

    while (fig != NULL) {
	if (fig -> visible == False) {
	    fig = fig -> prev;
	    continue;
	}

	fx = fig -> x;
	fy = fig -> y;
	wd = fig -> width;
	ht = fig -> height;

	if (Inside (fx - 2, x, fx + wd + 2) && Inside (fy - 2, y, fy + ht + 2)){

	    hw = (fig -> line_width + 1) / 2;
	    if (hw < 5)
		hw = 5;

	    switch (fig -> type) {
	    case TextFigure:
	    case PixmapFigure:
	    case BitmapFigure:
		return GroupLeader (fig);


	    case LineFigure:
		if (online (fig -> info.line.xpoints [0],
			    fig -> info.line.xpoints [1], x, y, hw))
		    return GroupLeader (fig);
		break;


	    case PolygonFigure:
		if (fig -> info.polygon.filled == True) {
		    if (XPointInRegion (fig -> info.polygon.region, x, y)==True)
			return GroupLeader (fig);
		} else
		    for (i = 1; i < fig -> info.polygon.npoints; i ++)
			if (online (fig -> info.polygon.xpoints [i - 1],
			    fig -> info.polygon.xpoints [i], x, y, hw))
			    return GroupLeader (fig);
		break;


	    case RectangleFigure:
		if (fig -> info.rectangle.filled == True)
		     return GroupLeader (fig);

		if (Inside (fx - hw, x, fx + hw) ||
		    Inside (fy - hw, y, fy + hw) ||
		    Inside (fx + wd - hw, x, fx + wd + hw) ||
		    Inside (fy + ht - hw, y, fy + ht + hw))
		    return GroupLeader (fig);

		break;


	    case ArcFigure:
		rx = wd / 2;
		ry = ht / 2;
		cx = x - fx - rx;
		cy = -y + fy + ry;
		theta = cx || cy ? atan2 (cy, cx) * 180 / 3.14159 : 0;
		if (theta < 0) theta = 360 + theta;
		theta *= 64;

		if (theta < fig -> info.arc.start)
		    break;

		if (theta > fig -> info.arc.start + fig -> info.arc.length)
		    break;

		if (fig -> info.arc.filled == True) {
		    if (Sqr(cx)/Sqr(rx) + Sqr(cy)/Sqr(ry) <= 1)
			return GroupLeader (fig);

		} else {
		    if (Sqr(cx)/Sqr(rx+hw) + Sqr(cy)/Sqr(ry+hw) <= 1 &&
			Sqr(cx)/Sqr(rx-hw) + Sqr(cy)/Sqr(ry-hw) >= 1)
			return GroupLeader (fig);
		}
		break;


	    case GroupFigure:
		break;
	    }
	}

	fig = fig -> prev;
    }

    return NULL;
}


/************************************************************************
   Function:	DW_ClipBox
   Description:	Retrieves the clip box of a figure.
 ************************************************************************/

void DW_ClipBox (Figure fig, XRectangle *rect)
{
    if (fig != NULL) {
	rect -> x      = fig -> x;
	rect -> y      = fig -> y;
	rect -> width  = fig -> width;
	rect -> height = fig -> height;
    }
}


/************************************************************************
   Function:	DW_SetInteractive
   Description: Set the interactive mode.
 ************************************************************************/

void DW_SetInteractive (Widget gw, Boolean interactive)
{
    GC		  gc;
    DrawingWidget dw;


    dw = (DrawingWidget) gw;

    if (interactive != dw -> drawing.interactive) {
	dw -> drawing.interactive = interactive;
	gc = dw -> drawing.drawgc;
	dw -> drawing.drawgc = dw -> drawing.intergc;
	dw -> drawing.intergc = gc;
    }
}


/************************************************************************
   Function:	DW_SetAutoFind
   Description: Set the auto find mode.
 ************************************************************************/

void DW_SetAutoFind (Widget gw, Boolean autofind)
{
    ((DrawingWidget) gw) -> drawing.search = autofind;
}


/************************************************************************
   Function:	DW_SetAutoRedraw
   Description: Set the auto redraw mode.
 ************************************************************************/

void DW_SetAutoRedraw (Widget gw, Boolean autoredraw)
{
    Region        null;
    Region        buffer;
    XRectangle    rect;
    DrawingWidget dw;


    dw = (DrawingWidget) gw;
    null = dw -> drawing.nullRegion;
    buffer = dw -> drawing.bufferRegion;

    if (dw -> drawing.redraw == False && autoredraw == True) {
	XClipBox (buffer, &rect);
	XClearArea (XtDisplay (gw), XtWindow (gw), rect.x - 1, rect.y - 1,
		    rect.width + 2, rect.height + 2, True);

    } else if (dw -> drawing.redraw == True && autoredraw == False)
	XIntersectRegion (null, buffer, buffer);


    dw -> drawing.redraw = autoredraw;
}


/************************************************************************
   Function:	DW_SearchArea
   Description:	Return all figures with an area.
 ************************************************************************/

Figure *DW_SearchArea (Widget gw, Point *points, unsigned int npoints, unsigned int *nfigs)
{
    int		  loc;
    unsigned	  i;
    Figure	  fig;
    Figure	 *figs;
    unsigned	  size;
    XPoint	 *xpoints;
    Region	  region;
    DrawingWidget dw;
    int           x;
    int           y;
    unsigned      width;
    unsigned      height;
    unsigned      line_width;


    dw = (DrawingWidget) gw;
    xpoints = (XPoint *) XtMalloc (sizeof (XPoint) * npoints);

    for (i = 0; i < npoints; i ++) {
	xpoints [i].x = XAbs (points [i].x);
	xpoints [i].y = YAbs (points [i].y);
    }

    *nfigs = 0;
    size = 0;
    figs = NULL;
    region = XPolygonRegion (xpoints, npoints, EvenOddRule);

    for (fig = dw -> drawing.tail; fig != NULL; fig = fig -> prev) {
	if (fig -> visible == False)
	    continue;

	line_width = fig -> line_width;
	if (line_width == 0)
	    line_width = 1;

	x = fig -> x - line_width;
	y = fig -> y - line_width;
	width = fig -> width + 2 * line_width;
	height = fig -> height + 2 * line_width;

	loc = XRectInRegion (region, x, y, width, height);
	if (loc == RectangleIn) {
	    if (*nfigs == size) {
		size = size ? size << 1 : 8;
		figs = (Figure *) XtRealloc ((char *) figs,sizeof(Figure)*size);
	    }

	    figs [(*nfigs) ++] = fig;
	}
    }

    XDestroyRegion (region);
    XtFree ((char *) xpoints);
    return figs;
}


/************************************************************************
   Function:	DW_RetrieveAll
   Description:	Retrieve all the figures on the display list.
 ************************************************************************/

Figure *DW_RetrieveAll (Widget gw, Boolean visible, unsigned int *nfigs)
{
    Figure	  fig;
    Figure	 *figs;
    unsigned	  size;
    DrawingWidget dw;


    dw = (DrawingWidget) gw;

    *nfigs = 0;
    size = 0;
    figs = NULL;

    for (fig = dw -> drawing.tail; fig != NULL; fig = fig -> prev) {
	if (visible == True && fig -> visible == False)
	    continue;

	if (*nfigs == size) {
	    size = size ? size << 1 : 8;
	    figs = (Figure *) XtRealloc ((char *) figs,sizeof (Figure) * size);
	}

	figs [(*nfigs) ++] = fig;
    }

    return figs;
}


/************************************************************************
   Function:	DW_Group
   Description:	Create a group figure from a set of figures.
 ************************************************************************/

Figure DW_Group (Widget gw, Figure *figs, unsigned int nfigs)
{
    unsigned i;
    Figure   fig;


    fig = DW_CreateFigure ((DrawingWidget) gw, GroupFigure, False, nfigs);

    for (i = 0; i < nfigs; i ++) {
	fig -> info.group.fig [i] = figs [i];
	DW_Detach ((DrawingWidget) gw, figs [i]);
    }

    return fig;
}


/************************************************************************
   Function:	DW_Ungroup
   Description:	Remove a group figure.
 ************************************************************************/

void DW_Ungroup (Widget gw, Figure fig)
{
    unsigned i;


    if (fig != NULL && fig -> type == GroupFigure) {
	for (i = 0; i < fig -> info.group.nfigs; i ++)
	    fig -> info.group.fig [i] -> group = NULL;

	DW_DestroyFigure (fig);
    }
}


/************************************************************************
   Function:	DW_CreatePixmap
   Description:	Utility function to create a pixmap of a specified width
		and height and of the same depth as the drawing widget.
 ************************************************************************/

Pixmap DW_CreatePixmap (Widget gw, unsigned int width, unsigned int height)
{
    unsigned depth;


    depth = ((DrawingWidget) gw) -> core.depth;
    return XCreatePixmap (XtDisplay (gw), XtWindow (gw), width, height, depth);
}


/************************************************************************
   Function:	DW_CreateBitmap
   Description:	Utility function to create a pixmap of a specified width
		and height but of depth one (a bitmap).
 ************************************************************************/

Pixmap DW_CreateBitmap (Widget gw, unsigned int width, unsigned int height)
{
    return XCreatePixmap (XtDisplay (gw), XtWindow (gw), width, height, 1);
}


/************************************************************************
   Function:	DW_TranslateCoords
   Description:	Translate from window coordinates to real coordinates
		(useful in action procedures).
 ************************************************************************/

void DW_TranslateCoords (Widget gw, int x, int y, float *rx, float *ry)
{
    DrawingWidget dw;


    dw = (DrawingWidget) gw;

    *rx = RealX (x);
    *ry = RealY (y);
}


/************************************************************************
   Function:	Float2Arg
   Description:	Convert a floating point argument to an XtArgVal suitable
		for use with XtSetArg.
 ************************************************************************/

XtArgVal Float2Arg (float value)
{
    float   *new_float;
    float    float_val;
    XtArgVal arg_val;


    /* This is in case the float was promoted to a double. */

    float_val = value;


    /* The simple case of when a float will fit within an XtArgVal. */

    if (sizeof (float) <= sizeof (XtArgVal)) {
	arg_val = *(XtArgVal *) &float_val;
	return arg_val;
    }


    /* The difficult case of when a float will not fit within an XtArgVal
       and thus we need to pass the address of a float to XtSetArg so we
       have to do a malloc to be safe. */

    new_float = XtNew (float);
    *new_float = float_val;
    return (XtArgVal) new_float;
}
