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
 * File:	Figure.c						*
 *									*
 * Description:	This file contains the private functions definitions	*
 *		for the	figures of the Drawing widget.			*
 ************************************************************************/

# include <stdio.h>
# include <X11/IntrinsicP.h>
# include "DrawingP.h"


static struct {
    int  length;
    char dash_list [4];
} line_styles [ ] = {
    {1, {1}},			/* solid (not really used) */
    {2, {3, 3}},		/* dashed		   */
    {2, {3, 1}},		/* dotted		   */
    {4, {7, 2, 1, 2}},		/* dot-dashed		   */
    {2, {7, 7}},		/* long-dashed		   */
};


/************************************************************************
   Function:	DW_CreateFigure
   Description:	Creates a new figure by allocating memory and
	        initializing.
 ************************************************************************/

Figure DW_CreateFigure (dw, type, flag, arg)
    DrawingWidget dw;
    FigureType	  type;
    BOOLEAN	  flag;
    int		  arg;
{
    Figure fig;


    fig = XtNew (struct figure);

    fig -> type	      = type;
    fig -> prev	      = NULL;
    fig -> next	      = NULL;
    fig -> line_width = dw -> drawing.line_width;
    fig -> line_style = dw -> drawing.line_style;
    fig -> fg	      = dw -> drawing.fg;
    fig -> visible    = True;
    fig -> group      = NULL;
    fig -> color_data = dw -> drawing.color_data;

    DW_CacheAddRef (fig -> color_data);

    switch (fig -> type) {
    case LineFigure:
    case PixmapFigure:
    case BitmapFigure:
	break;

    case PolygonFigure:
	fig -> info.polygon.filled  = False;
	fig -> info.polygon.scaled  = flag;
	fig -> info.polygon.npoints = arg;
	fig -> info.polygon.region  = NULL;
	fig -> info.polygon.points  = (Point *) XtMalloc (sizeof (Point) * arg);
	fig -> info.polygon.xpoints = (XPoint *) XtMalloc (sizeof (XPoint)*arg);
	break;

    case RectangleFigure:
	fig -> info.rectangle.filled = arg ? True : False;
	fig -> info.rectangle.scaled = flag;
	break;

    case ArcFigure:
	fig -> info.arc.filled = arg ? True : False;
	fig -> info.arc.scaled = flag;
	break;

    case TextFigure:
	fig -> info.text.font = dw -> drawing.font;
	fig -> info.text.scaled = flag;
	fig -> info.text.font_data = dw -> drawing.font_data;
	DW_CacheAddRef (fig -> info.text.font_data);
	break;

    case GroupFigure:
	fig -> info.group.fig = (Figure *) XtMalloc (sizeof (Figure) * arg);
	fig -> info.group.nfigs = arg;
	break;
    }

    return fig;
}


/************************************************************************
   Function:	DestroyFigure
   Description:	Destroys an figure by deallocating its memory.
 ************************************************************************/

void DW_DestroyFigure (fig)
    Figure fig;
{
    if (fig -> type == PolygonFigure) {
	XtFree ((char *) fig -> info.polygon.points);
	XtFree ((char *) fig -> info.polygon.xpoints);
	if (fig -> info.polygon.region)
	    XDestroyRegion (fig -> info.polygon.region);

    } else if (fig -> type == TextFigure) {
	DW_CacheDelRef (fig -> info.text.font_data);
	XtFree ((char *) fig -> info.text.string);

    } else if (fig -> type == GroupFigure)
	XtFree ((char *) fig -> info.group.fig);

    DW_CacheDelRef (fig -> color_data);
    XtFree ((char *) fig);
}


/************************************************************************
   Function:	DW_AppendFigure
   Description: Adds a figure to the end of the display list.
 ************************************************************************/

void DW_AppendFigure (dw, fig)
    DrawingWidget dw;
    Figure	  fig;
{
    fig -> prev = dw -> drawing.tail;
    if (dw -> drawing.tail != NULL)
	dw -> drawing.tail -> next = fig;
    else
	dw -> drawing.head = fig;

    dw -> drawing.tail = fig;
}


/************************************************************************
   Function:	DW_PrependFigure
   Description:	Adds a figure to the beginning of the display list.
 ************************************************************************/

void DW_PrependFigure (dw, fig)
    DrawingWidget dw;
    Figure        fig;
{
    fig -> next = dw -> drawing.head;
    if (dw -> drawing.head != NULL)
	dw -> drawing.head -> prev = fig;
    else
	dw -> drawing.tail = fig;

    dw -> drawing.head = fig;
}


/************************************************************************
   Function:	DW_DeleteFigure
   Description:	Deletes a figure from the display list.
 ************************************************************************/

void DW_DeleteFigure (dw, fig)
    DrawingWidget dw;
    Figure        fig;
{
    if (fig -> next != NULL)
	fig -> next -> prev = fig -> prev;
    else
	dw -> drawing.tail = fig -> prev;

    if (fig -> prev != NULL)
	fig -> prev -> next = fig -> next;
    else
	dw -> drawing.head = fig -> next;

    fig -> prev = fig -> next = NULL;
}


/************************************************************************
   Function:	DW_ScaleFigure
   Description:	Scale a figure to window coordinates.
 ************************************************************************/

void DW_ScaleFigure (dw, fig)
    DrawingWidget dw;
    Figure        fig;
{
    int		i;
    int		direction;
    int		ascent;
    int		descent;
    unsigned	dont_care;
    short	minx;
    short	maxx;
    short	miny;
    short	maxy;
    short	x;
    short	y;
    Window	root;
    Figure	member;
    XCharStruct overall;


    switch (fig -> type) {
    case LineFigure:
	fig -> info.line.xpoints [0].x = XAbs (fig -> info.line.points [0].x);
	fig -> info.line.xpoints [0].y = YAbs (fig -> info.line.points [0].y);
	fig -> info.line.xpoints [1].x = XAbs (fig -> info.line.points [1].x);
	fig -> info.line.xpoints [1].y = YAbs (fig -> info.line.points [1].y);

	if (fig -> info.line.xpoints [0].x < fig -> info.line.xpoints [1].x) {
	    fig -> x = fig -> info.line.xpoints [0].x;
	    fig -> width = fig -> info.line.xpoints [1].x - fig -> x;
	} else {
	    fig -> x = fig -> info.line.xpoints [1].x;
	    fig -> width = fig -> info.line.xpoints [0].x - fig -> x;
	}

	if (fig -> info.line.xpoints [0].y < fig -> info.line.xpoints [1].y) {
	    fig -> y = fig -> info.line.xpoints [0].y;
	    fig -> height = fig -> info.line.xpoints [1].y - fig -> y;
	} else {
	    fig -> y = fig -> info.line.xpoints [1].y;
	    fig -> height = fig -> info.line.xpoints [0].y - fig -> y;
	}
	break;


    case PolygonFigure:
	minx = maxx = XAbs (fig -> info.polygon.points [0].x);
	miny = maxy = YAbs (fig -> info.polygon.points [0].y);

	if (fig -> info.polygon.scaled == True) {
	    for (i = 0; i < fig -> info.polygon.npoints; i ++) {
		x = fig -> info.polygon.xpoints [i].x =
		    XAbs (fig -> info.polygon.points [i].x);
		y = fig -> info.polygon.xpoints [i].y =
		    YAbs (fig -> info.polygon.points [i].y);

		if (x < minx)
		    minx = x;
		else if (x > maxx)
		    maxx = x;

		if (y < miny)
		    miny = y;
		else if (y > maxy)
		    maxy = y;
	    }
	} else {
	}


	if (fig -> info.polygon.filled == True) {
	    if (fig -> info.polygon.region)
		XDestroyRegion (fig -> info.polygon.region);

	    fig -> info.polygon.region = XPolygonRegion (
		fig -> info.polygon.xpoints, fig -> info.polygon.npoints,
		EvenOddRule);
	}

	if (minx < maxx) {
	    fig -> x = minx;
	    fig -> width = maxx - minx;
	} else {
	    fig -> x = maxx;
	    fig -> width = minx - maxx;
	}

	if (miny < maxy) {
	    fig -> y = miny;
	    fig -> height = maxy - miny;
	} else {
	    fig -> y = maxy;
	    fig -> height = miny - maxy;
	}
	break;


    case RectangleFigure:
	if (fig -> info.rectangle.width > 0) {
	    if (fig -> info.rectangle.scaled == True)
		fig -> width = XRel (fig -> info.rectangle.width);
	    else
		fig -> width = Round (fig -> info.rectangle.width);
	    fig -> x = XAbs (fig -> info.rectangle.x);

	} else {
	    if (fig -> info.rectangle.scaled == True)
		fig -> width = -XRel (fig -> info.rectangle.width);
	    else
		fig -> width = Round (-fig -> info.rectangle.width);
	    fig -> x = XAbs (fig -> info.rectangle.x) - fig -> width;
	}

	if (fig -> info.rectangle.height > 0) {
	    if (fig -> info.rectangle.scaled == True)
		fig -> height = YRel (fig -> info.rectangle.height);
	    else
		fig -> height = Round (fig -> info.rectangle.height);
	    fig -> y = YAbs (fig -> info.rectangle.y) - fig -> height;

	} else {
	    if (fig -> info.rectangle.scaled == True)
		fig -> height = -YRel (fig -> info.rectangle.height);
	    else
		fig -> height = Round (-fig -> info.rectangle.height);
	    fig -> y = YAbs (fig -> info.rectangle.y);
	}
	break;


    case ArcFigure:
	if (fig -> info.arc.width > 0)
	    if (fig -> info.arc.scaled == True)
		fig -> width = XRel (fig -> info.arc.width);
	    else
		fig -> width = Round (fig -> info.arc.width);
	else
	    if (fig -> info.arc.scaled == True)
		fig -> width = -XRel (fig -> info.arc.width);
	    else
		fig -> width = Round (-fig -> info.arc.width);

	if (fig -> info.arc.height > 0)
	    if (fig -> info.arc.scaled == True)
		fig -> height = XRel (fig -> info.arc.height);
	    else
		fig -> height = Round (fig -> info.arc.height);
	else
	    if (fig -> info.arc.scaled == True)
		fig -> height = -XRel (fig -> info.arc.height);
	    else
		fig -> height = Round (-fig -> info.arc.height);

	fig -> x = XAbs (fig -> info.arc.x) - fig -> width / 2;
	fig -> y = YAbs (fig -> info.arc.y) - fig -> height / 2;
	break;


    case TextFigure:
	fig -> info.text.x = XAbs (fig -> info.text.rx);
	fig -> info.text.y = YAbs (fig -> info.text.ry);
	XTextExtents (fig -> info.text.font, fig -> info.text.string,
		      fig -> info.text.length, &direction, &ascent, &descent,
		      &overall);

	fig -> x = fig -> info.text.x + overall.lbearing;
	fig -> y = fig -> info.text.y - overall.ascent;
	fig -> width = overall.rbearing - overall.lbearing;
	fig -> height = overall.descent + overall.ascent;
	break;


    case GroupFigure:
	if (fig -> info.group.nfigs > 0) {
	    minx = fig -> info.group.fig [0] -> x;
	    miny = fig -> info.group.fig [0] -> y;
	    maxx = minx + fig -> info.group.fig [0] -> width;
	    maxy = miny + fig -> info.group.fig [0] -> height;
	} else {
	    minx = miny = maxx = maxy = 0;
	}

	for (i = 1; i < fig -> info.group.nfigs; i ++) {
	    member = fig -> info.group.fig [i];
	    if (minx > member -> x)
		minx = member -> x;
	    if (miny > member -> y)
		miny = member -> y;
	    if (maxx < member -> x + member -> width)
		maxx = member -> x + member -> width;
	    if (maxy < member -> y + member -> height)
		maxy = member -> y + member -> height;
	}

	if (minx < maxx) {
	    fig -> x = minx;
	    fig -> width = maxx - minx;
	} else {
	    fig -> x = maxx;
	    fig -> width = minx - maxx;
	}

	if (miny < maxy) {
	    fig -> y = miny;
	    fig -> height = maxy - miny;
	} else {
	    fig -> y = maxy;
	    fig -> height = miny - maxy;
	}
	break;


    case PixmapFigure:
    case BitmapFigure:
	XGetGeometry (XtDisplay ((Widget) dw), fig -> info.pixmap.pixmap,
	    &root, (int *) &dont_care, (int *) &dont_care, &fig -> width,
	    &fig -> height, &dont_care, &dont_care);

	fig -> x = XAbs (fig -> info.pixmap.x);
	fig -> y = YAbs (fig -> info.pixmap.y) - fig -> height;
	break;
    }
}


/************************************************************************
   Function:	DW_DrawFigure
   Description:	Draws a figure in the window of the specified widget.
 ************************************************************************/

void DW_DrawFigure (dw, fig)
    DrawingWidget dw;
    Figure        fig;
{
    Display	       *display;
    Window		window;
    GC			drawgc;
    Font		font;
    unsigned long	valuemask;
    XGCValues		values;
    Boolean		first_time;


    if (fig -> visible == False)
	return;

    valuemask = 0;
    display = XtDisplay ((Widget) dw);
    window = XtWindow ((Widget) dw);
    drawgc = dw -> drawing.drawgc;
    first_time = dw -> drawing.last_style == -1 ? True : False;

    if (first_time || dw -> drawing.last_fg != fig -> fg) {
	valuemask |= GCForeground;
	values.foreground = dw -> drawing.last_fg = fig -> fg;
    }

    if (first_time || dw -> drawing.last_width != fig -> line_width) {
	valuemask |= GCLineWidth;
	values.line_width = dw -> drawing.last_width = fig -> line_width;
    }

    if (first_time || dw -> drawing.last_style != fig -> line_style) {
	dw -> drawing.last_style = fig -> line_style;
	valuemask |= GCLineStyle;
	if (fig -> line_style != DW_LineSolid) {
	    values.line_style = LineOnOffDash;
	    XSetDashes (display, dw -> drawing.drawgc, 0,
			line_styles [fig -> line_style].dash_list,
			line_styles [fig -> line_style].length);
	    XSetDashes (display, dw -> drawing.intergc, 0,
			line_styles [fig -> line_style].dash_list,
			line_styles [fig -> line_style].length);
	} else
	    values.line_style = LineSolid;
    }

    if (valuemask) {
	XChangeGC (display, drawgc, valuemask, &values);
	XChangeGC (display, dw -> drawing.intergc, valuemask, &values);
    }


    switch (fig -> type) {
    case LineFigure:
	XDrawLine (display, window, drawgc,
	    fig -> info.line.xpoints [0].x, fig -> info.line.xpoints [0].y,
	    fig -> info.line.xpoints [1].x, fig -> info.line.xpoints [1].y);
	break;


    case PolygonFigure:
	if (fig -> info.polygon.filled == True)
	    XFillPolygon (display, window, drawgc,
		fig -> info.polygon.xpoints, fig -> info.polygon.npoints,
		Complex, CoordModeOrigin);
	else
	    XDrawLines (display, window, drawgc,
		fig -> info.polygon.xpoints, fig -> info.polygon.npoints,
		CoordModeOrigin);
	break;


    case RectangleFigure:
	if (fig -> info.rectangle.filled == True)
	    XFillRectangle (display, window, drawgc, fig -> x, fig -> y,
		fig -> width, fig -> height);
	else
	    XDrawRectangle (display, window, drawgc, fig -> x, fig -> y,
		fig -> width, fig -> height);
	break;


    case ArcFigure:
	if (fig -> info.arc.filled == True)
	    XFillArc (display, window, drawgc, fig -> x, fig -> y, fig -> width,
		fig -> height, fig -> info.arc.start, fig -> info.arc.length);
	else
	    XDrawArc (display, window, drawgc, fig -> x, fig -> y, fig -> width,
		fig -> height, fig -> info.arc.start, fig -> info.arc.length);
	break;


    case TextFigure:
	font = fig -> info.text.font -> fid;
	if (first_time || dw -> drawing.last_font != font) {
	    dw -> drawing.last_font = font;
	    XSetFont (display, drawgc, font);
	    XSetFont (display, dw -> drawing.intergc, font);
	}

	XDrawString (display, window, drawgc, fig -> info.text.x,
	    fig -> info.text.y, fig -> info.text.string,
	    fig -> info.text.length);
	break;


    case GroupFigure:
	break;


    case PixmapFigure:
	XCopyArea (display, fig -> info.pixmap.pixmap, window, drawgc,
	    0, 0, fig -> width, fig -> height, fig -> x, fig -> y);
	break;


    case BitmapFigure:
	XSetFunction   (display, drawgc, GXand);
	XSetBackground (display, drawgc, 255);
	XCopyPlane (display, fig -> info.pixmap.pixmap, window, drawgc,
	    0, 0, fig -> width, fig -> height, fig -> x, fig -> y, 1L);

	XSetFunction (display, drawgc, GXor);
	XSetBackground (display, drawgc, 0);
	XCopyPlane (display, fig -> info.pixmap.pixmap, window, drawgc,
	    0, 0, fig -> width, fig -> height, fig -> x, fig -> y, 1L);

	if (dw -> drawing.interactive == True)
	    XSetFunction (display, drawgc, GXxor);
	else
	    XSetFunction (display, drawgc, GXcopy);
	XSetBackground (display, drawgc, dw -> core.background_pixel);
	break;
    }

    first_time = 0;
}


/************************************************************************
   Function:	DW_ClearFigure
   Description:	Clears the area occupied by a figure.
 ************************************************************************/

void DW_ClearFigure (dw, fig)
    DrawingWidget dw;
    Figure        fig;
{
    XRectangle rect;


    if (fig -> visible == False)
	return;

    if (dw -> drawing.interactive == True)
	DW_DrawFigure (dw, fig);
    else {
	DW_ClipBox (fig, &rect);
	rect.x -= fig -> line_width + 1;
	rect.y -= fig -> line_width + 1;
	rect.width += 2 * fig -> line_width + 2;
	rect.height += 2 * fig -> line_width + 2;
	if (dw -> drawing.redraw == True)
	    XClearArea (XtDisplay ((Widget) dw), XtWindow ((Widget) dw),
			rect.x, rect.y, rect.width, rect.height, True);
	else
	    XUnionRectWithRegion (&rect, dw -> drawing.bufferRegion,
					 dw -> drawing.bufferRegion);
    }
}


/************************************************************************
   Function:	DW_Detach
   Description:	Detach a figure from a group.
 ************************************************************************/

void DW_Detach (dw, fig)
    DrawingWidget dw;
    Figure        fig;
{
    unsigned i;
    unsigned j;
    Figure   leader;


    if (fig -> group != NULL) {
	leader = fig -> group;

	for (i = 0; i < leader -> info.group.nfigs; i ++)
	    if (leader -> info.group.fig [i] == fig) {
		for (j = i + 1; j < leader -> info.group.nfigs; j ++)
		    leader->info.group.fig [j - 1] = leader->info.group.fig [j];
		leader -> info.group.nfigs --;
		break;
	    }

	DW_ScaleFigure (dw, leader);
    }
}


/************************************************************************
   Function:	DW_Attach
   Description:	Attach a figure to a group.
 ************************************************************************/

void DW_Attach (dw, fig, group)
    DrawingWidget dw;
    Figure	  fig;
    Figure	  group;
{
    if (fig -> group == group)
	return;

    if (group != NULL) {
	group -> info.group.fig = (Figure *) XtRealloc ((char *) group ->
	    info.group.fig, sizeof (Figure) * (group -> info.group.nfigs + 1));
	group -> info.group.fig [group -> info.group.nfigs] = fig;
	group -> info.group.nfigs ++;
	DW_ScaleFigure (dw, group);
    }

    fig -> group = group;
}
