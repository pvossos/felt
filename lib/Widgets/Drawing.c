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
 * File:	Drawing.c						*
 *									*
 * Description:	This file contains the widget functions for the Drawing	*
 *		widget.							*
 ************************************************************************/

# include <stdio.h>
# include <math.h>
# include <X11/IntrinsicP.h>
# include <X11/StringDefs.h>
# include <X11/Xaw/XawInit.h>
# include "DrawingP.h"


/* Private functions */

static void    ClassInitialize ( );
static void    Initialize ( );
static void    Realize ( );
static void    Redisplay ( );
static void    Destroy ( );
static Boolean SetValues ( );


/* Resource defaults */

# define DefaultGrid		FALSE
# define DefaultGridSize	1.00
# define DefaultSnap		FALSE
# define DefaultSnapSize	0.25
# define DefaultXMin		0.00
# define DefaultXMax		10.0
# define DefaultYMin		0.00
# define DefaultYMax		10.0
# define DefaultXScale		50.0
# define DefaultYScale		50.0
# define DefaultCursor		"crosshair"


/* Private variables */

static float defaultgridsize	= DefaultGridSize;
static float defaultsnapsize	= DefaultSnapSize;
static float defaultxmin	= DefaultXMin;
static float defaultxmax	= DefaultXMax;
static float defaultymin	= DefaultYMin;
static float defaultymax	= DefaultYMax;
static float defaultxscale	= DefaultXScale;
static float defaultyscale	= DefaultYScale;


/* Resources */

# define offset(field) XtOffsetOf(DrawingRec, field)

static XtResource resources[] = {
{XtNforeground, XtCForeground, XtRPixel, sizeof (Pixel),
    offset(drawing.foreground), XtRString, (XtPointer) XtDefaultForeground},
{XtNfont, XtCFont, XtRFontStruct, sizeof (XFontStruct *),
    offset(drawing.font), XtRString, (XtPointer) XtDefaultFont},
{XtNcoordinates, XtCCoordinates, XtRWidget, sizeof (Widget),
    offset(drawing.coord), XtRWidget, (XtPointer) NULL},
{XtNgrid, XtCGrid, XtRBoolean, sizeof (Boolean),
    offset(drawing.grid), XtRImmediate, (XtPointer) DefaultGrid},
{XtNgridSize, XtCGridSize, XtRFloat, sizeof (float),
    offset(drawing.gridSize), XtRFloat, (XtPointer) &defaultgridsize},
{XtNsnap, XtCSnap, XtRBoolean, sizeof (Boolean),
    offset(drawing.snap), XtRImmediate, (XtPointer) DefaultSnap},
{XtNsnapSize, XtCSnapSize, XtRFloat, sizeof (float),
    offset(drawing.snapSize), XtRFloat, (XtPointer) &defaultsnapsize},
{XtNxMin, XtCXMin, XtRFloat, sizeof (float),
    offset(drawing.xMin), XtRFloat, (XtPointer) &defaultxmin},
{XtNxMax, XtCXMax, XtRFloat, sizeof (float),
    offset(drawing.xMax), XtRFloat, (XtPointer) &defaultxmax},
{XtNyMin, XtCYMin, XtRFloat, sizeof (float),
    offset(drawing.yMin), XtRFloat, (XtPointer) &defaultymin},
{XtNyMax, XtCYMax, XtRFloat, sizeof (float),
    offset(drawing.yMax), XtRFloat, (XtPointer) &defaultymax},
{XtNxScale, XtCXScale, XtRFloat, sizeof (float),
    offset(drawing.xScale), XtRFloat, (XtPointer) &defaultxscale},
{XtNyScale, XtCYScale, XtRFloat, sizeof (float),
    offset(drawing.yScale), XtRFloat, (XtPointer) &defaultyscale},
{XtNbuttonCallback, XtCButtonCallback, XtRCallback, sizeof (XtPointer),
    offset(drawing.button), XtRImmediate, (XtPointer) NULL},
{XtNmotionCallback, XtCMotionCallback, XtRCallback, sizeof (XtPointer),
    offset(drawing.motion), XtRImmediate, (XtPointer) NULL},
{XtNinteractive, XtCInteractive, XtRBoolean, sizeof (Boolean),
    offset(drawing.interactive), XtRImmediate, (XtPointer) FALSE},
{XtNautoFind, XtCAutoFind, XtRBoolean, sizeof (Boolean),
    offset(drawing.search), XtRImmediate, (XtPointer) FALSE},
{XtNautoRedraw, XtCAutoRedraw, XtRBoolean, sizeof (Boolean),
    offset(drawing.redraw), XtRImmediate, (XtPointer) TRUE},
{XtNcursor, XtCCursor, XtRCursor, sizeof (Cursor),
    offset(simple.cursor), XtRString, (XtPointer) DefaultCursor},
};
# undef offset


DrawingClassRec drawingClassRec = {
  { /* core fields */
    /* superclass		*/	(WidgetClass) &simpleClassRec,
    /* class_name		*/	"Drawing",
    /* widget_size		*/	sizeof (DrawingRec),
    /* class_initialize		*/	ClassInitialize,
    /* class_part_initialize	*/	NULL,
    /* class_inited		*/	FALSE,
    /* initialize		*/	Initialize,
    /* initialize_hook		*/	NULL,
    /* realize			*/	Realize,
    /* actions			*/	NULL,
    /* num_actions		*/	0,
    /* resources		*/	resources,
    /* num_resources		*/	XtNumber (resources),
    /* xrm_class		*/	NULLQUARK,
    /* compress_motion		*/	FALSE,
    /* compress_exposure	*/	TRUE,
    /* compress_enterleave	*/	TRUE,
    /* visible_interest		*/	TRUE,
    /* destroy			*/	Destroy,
    /* resize			*/	NULL,
    /* expose			*/	Redisplay,
    /* set_values		*/	SetValues,
    /* set_values_hook		*/	NULL,
    /* set_values_almost	*/	XtInheritSetValuesAlmost,
    /* get_values_hook		*/	NULL,
    /* accept_focus		*/	NULL,
    /* version			*/	XtVersion,
    /* callback_private		*/	NULL,
    /* tm_table			*/	NULL,
    /* query_geometry		*/	XtInheritQueryGeometry,
    /* display_accelerator	*/	XtInheritDisplayAccelerator,
    /* extension		*/	NULL
  },
  { /* simple fields */
    /* change_sensitive		*/	XtInheritChangeSensitive
  },
  { /* drawing fields */
    /* empty			*/	0
  }
};


WidgetClass drawingWidgetClass = (WidgetClass) &drawingClassRec;


/* Private functions */

/************************************************************************
   Function:	SnapCoord
   Description:	Snap the coordinates to the snap grid if enabled.
 ************************************************************************/

static void SnapCoord (dw, x, y)
    DrawingWidget dw;
    float	 *x;
    float	 *y;
{
    double snap;

    if (dw -> drawing.snap == True) {
	snap = dw -> drawing.snapSize;
	*x = (int) ((*x + snap / (*x >= 0 ? 2 : -2)) / snap) * snap;
	*y = (int) ((*y + snap / (*y >= 0 ? 2 : -2)) / snap) * snap;
    }
}



/************************************************************************
   Function:	MotionHandler
   Description:	Handles motion events when a button is not depressed.
 ************************************************************************/

static void MotionHandler (gw, clientData, event, cont)
    Widget    gw;
    XtPointer clientData;
    XEvent   *event;
    Boolean  *cont;
{
    DrawingReport report;
    DrawingWidget dw;
    Arg		  al [1];
    char	  string [40];
    float	  realx;
    float	  realy;
    int		  x;
    int		  y;


    dw = (DrawingWidget) gw;
    x = event -> xmotion.x;
    y = event -> xmotion.y;

    if (x < 0 || x > (int) dw -> drawing.width)
	return;

    if (y < 0 || y > (int) dw -> drawing.height)
	return;

    realx = RealX (x);
    realy = RealY (y);

    report.event       = event;
    report.unsnapped.x = realx;
    report.unsnapped.y = realy;
    report.snapped.x   = realx;
    report.snapped.y   = realy;
    SnapCoord (dw, &report.snapped.x, &report.snapped.y);

    if (dw -> drawing.coord != NULL) {
	sprintf (string, "%.*f,%.*f", dw -> drawing.xprecision,
	report.snapped.x, dw -> drawing.yprecision, report.snapped.y);
	XtSetArg (al [0], XtNlabel, string);
	XtSetValues (dw -> drawing.coord, al, 1);
    }

    XtCallCallbacks (gw, XtNmotionCallback, (XtPointer) &report);
}


/************************************************************************
   Function:	ButtonHandler
   Description:	Handles mouse events when a button is depressed.
 ************************************************************************/

static void ButtonHandler (gw, clientData, event, cont)
    Widget    gw;
    XtPointer clientData;
    XEvent   *event;
    Boolean  *cont;
{
    static DrawingReport report;
    DrawingWidget	 dw;
    float		 realx;
    float		 realy;
    int			 x;
    int			 y;


    dw = (DrawingWidget) gw;
    x = event -> xmotion.x;
    y = event -> xmotion.y;

    if (x < 0)
	x = 0;
    else if (x > (int) dw -> drawing.width)
	x = dw -> drawing.width;

    if (y < 0)
	y = 0;
    else if (y > (int) dw -> drawing.height)
	y = dw -> drawing.height;


    realx = RealX (x);
    realy = RealY (y);

    report.event       = event;
    report.unsnapped.x = realx;
    report.unsnapped.y = realy;
    report.snapped.x   = realx;
    report.snapped.y   = realy;
    SnapCoord (dw, &report.snapped.x, &report.snapped.y);


    if (event -> type == ButtonPress)
	if (dw -> drawing.search == True)
	    report.figure = DW_FindFigure (gw, realx, realy);
	else
	    report.figure = NULL;

    XtCallCallbacks (gw, XtNbuttonCallback, (XtPointer) &report);
}


/************************************************************************
   Function:	SetClipRegion
   Description:	Sets the clip region to the size of the widget.
 ************************************************************************/

static void SetClipRegion (dw)
    DrawingWidget dw;
{
    Region     nullRegion;
    XRectangle rect;


    rect.x = 0;
    rect.y = 0;
    rect.width = dw -> drawing.width;
    rect.height = dw -> drawing.height;

    nullRegion = dw -> drawing.nullRegion;
    XIntersectRegion (nullRegion, dw -> drawing.region, dw -> drawing.region);
    XUnionRectWithRegion (&rect, dw -> drawing.region, dw -> drawing.region);
}


/************************************************************************
   Function:	DrawGrid
   Description:	Draw the grid on the window.
 ************************************************************************/

static void DrawGrid (gw)
    Widget gw;
{
    int		  coord;
    float	  size;
    float	  x;
    float	  y;
    float	  min;
    float	  max;
    float	  scale;
    Display	 *display;
    DrawingWidget dw;


    dw = (DrawingWidget) gw;
    display = XtDisplay (gw);

    min   = dw -> drawing.xMin;
    max   = dw -> drawing.xMax;
    size  = dw -> drawing.gridSize;
    scale = dw -> drawing.xScale;

    for (x = min - size; x <= max + size; x += size) {
	coord = ((int) (x / size) * size - min) * scale;
	XDrawLine (display, XtWindow (gw), dw -> drawing.gridgc, coord, 0,
		   coord, dw -> drawing.height);
    }

    min   = dw -> drawing.yMin;
    max   = dw -> drawing.yMax;
    scale = dw -> drawing.yScale;

    for (y = min - size; y <= max + size; y += size) {
	coord = (max - (int) (y / size) * size) * scale;
	XDrawLine (display, XtWindow (gw), dw -> drawing.gridgc, 0, coord,
		   dw -> drawing.width, coord);
    }
}


/************************************************************************
   Function:	DrawList
   Description:	Draws the display list.
 ************************************************************************/

static void DrawList (dw)
    DrawingWidget dw;
{
    Figure fig;


    for (fig = dw -> drawing.head; fig != NULL; fig = fig -> next)
	DW_DrawFigure (dw, fig);
}


/************************************************************************
   Function:	ScaleList
   Description:	Scales the display list.
 ************************************************************************/

static void ScaleList (dw)
    DrawingWidget dw;
{
    Figure fig;


    for (fig = dw -> drawing.head; fig != NULL; fig = fig -> next)
	DW_ScaleFigure (dw, fig);
}


/* Widget class functions */

/************************************************************************
   Function:	ClassInitialize
   Description: Initializes the widget class.
 ************************************************************************/

static void ClassInitialize ( )
{
    XawInitializeWidgetSet ( );
}


/************************************************************************
   Function:	Initialize
   Description:	Initializes the widget.
 ************************************************************************/

static void Initialize (request, new, argv, argc)
    Widget   request;
    Widget   new;
    ArgList  argv;
    Cardinal argc;
{
    Display	 *display;
    XtArgVal	  value;
    CacheData	  data;
    DrawingWidget dw;


    dw = (DrawingWidget) new;
    display = XtDisplay (new);


    if (dw -> drawing.xMin >= dw -> drawing.xMax) {
	dw -> drawing.xMin = DefaultXMin;
	dw -> drawing.xMax = DefaultXMax;
    }

    if (dw -> drawing.yMin >= dw -> drawing.yMax) {
	dw -> drawing.yMin = DefaultYMin;
	dw -> drawing.yMax = DefaultYMax;
    }

    if (dw -> drawing.xScale <= 0)
	dw -> drawing.xScale = DefaultXScale;

    if (dw -> drawing.yScale <= 0)
	dw -> drawing.yScale = DefaultYScale;

    if (dw -> drawing.gridSize <= 0)
	dw -> drawing.gridSize = DefaultGridSize;

    if (dw -> drawing.snapSize <= 0)
	dw -> drawing.snapSize = DefaultSnapSize;


    dw -> drawing.width  = (dw -> drawing.xMax - dw -> drawing.xMin) *
			    dw -> drawing.xScale;
    dw -> drawing.height = (dw -> drawing.yMax - dw -> drawing.yMin) *
			    dw -> drawing.yScale;

    dw -> core.width  = dw -> drawing.width;
    dw -> core.height = dw -> drawing.height;

    dw -> drawing.gridgc = NULL;
    dw -> drawing.drawgc = NULL;
    dw -> drawing.head   = NULL;
    dw -> drawing.tail   = NULL;

    dw -> drawing.xprecision  = ceil (log10 (dw -> drawing.xScale));
    dw -> drawing.yprecision  = ceil (log10 (dw -> drawing.yScale));
    dw -> drawing.line_width  = 0;
    dw -> drawing.line_style  = DW_LineSolid;
    dw -> drawing.search      = False;
    dw -> drawing.interactive = False;
    dw -> drawing.redraw      = True;
    dw -> drawing.last_style  = -1;
    dw -> drawing.last_fg     = -1;
    dw -> drawing.last_width  = 0;
    dw -> drawing.last_font   = 0;

    dw -> drawing.color_cache = DW_CacheCreate ( );
    dw -> drawing.font_cache = DW_CacheCreate ( );

    value = (XtArgVal) dw -> drawing.foreground;
    data = DW_CacheInsert (dw -> drawing.color_cache, "initial", value);
    dw -> drawing.fg = dw -> drawing.foreground;
    dw -> drawing.color_data = data;

    value = (XtArgVal) dw -> drawing.font;
    data = DW_CacheInsert (dw -> drawing.font_cache, "initial", value);
    dw -> drawing.font_data = data;
}


/************************************************************************
   Function:	Realize
   Description: Realizes the widget by realizing the superclass.  Adds
		events handlers and creates the GCs.
 ************************************************************************/

static void Realize (gw, valuemaskp, attr)
    Widget		  gw;
    XtValueMask		 *valuemaskp;
    XSetWindowAttributes *attr;
{
    Display	 *display;
    DrawingWidget dw;
    unsigned long mask;
    XGCValues	  values;
    static char   dashes [ ] = {1, 4};


    dw = (DrawingWidget) gw;
    display = XtDisplay (gw);

    (*drawingWidgetClass -> core_class.superclass -> core_class.realize)
	(gw, valuemaskp, attr);

    mask = ButtonPressMask | ButtonReleaseMask | OwnerGrabButtonMask;
    mask |= Button1MotionMask | Button2MotionMask | Button3MotionMask;
    XtAddEventHandler (gw, mask, False, ButtonHandler, NULL);
    XtAddEventHandler (gw, PointerMotionMask, False, MotionHandler, NULL);


    mask = GCFunction | GCForeground | GCBackground | GCLineStyle;

    values.function   = GXcopy;
    values.line_style = LineOnOffDash;
    values.background = dw -> core.background_pixel;
    values.foreground = dw -> drawing.foreground;

    dw -> drawing.gridgc = XCreateGC (display, XtWindow (gw), mask, &values);
    XSetDashes (display, dw -> drawing.gridgc, 0, dashes, 2);


    mask = GCFunction | GCForeground | GCBackground;

    values.function   = GXcopy;
    values.background = dw -> core.background_pixel;
    values.foreground = dw -> drawing.foreground;

    dw -> drawing.drawgc = XCreateGC (display, XtWindow (gw), mask, &values);


    mask = GCFunction | GCForeground | GCBackground;

    values.function   = GXxor;
    values.background = dw -> core.background_pixel;
    values.foreground = dw -> drawing.foreground ^ values.background;

    if (values.foreground == values.background)
	values.function = GXequiv;

    dw -> drawing.intergc = XCreateGC (display, XtWindow (gw), mask, &values);


    dw -> drawing.region = XCreateRegion ( );
    dw -> drawing.nullRegion = XCreateRegion ( );
    dw -> drawing.bufferRegion = XCreateRegion ( );
    SetClipRegion (dw);
}


/************************************************************************
   Function:	Destroy
   Description:	Destroys private resources.
 ************************************************************************/

static void Destroy (gw)
    Widget gw;
{
    Figure	  fig;
    Figure	  next;
    Display	 *display;
    DrawingWidget dw;


    dw = (DrawingWidget) gw;
    display = XtDisplay (gw);

    if (dw -> drawing.region)
	XDestroyRegion (dw -> drawing.region);

    if (dw -> drawing.gridgc)
	XFreeGC (display, dw -> drawing.gridgc);

    if (dw -> drawing.drawgc)
	XFreeGC (display, dw -> drawing.drawgc);

    if (dw -> drawing.intergc)
	XFreeGC (display, dw -> drawing.intergc);

    fig = dw -> drawing.head;
    while (fig != NULL) {
	next = fig -> next;
	DW_DestroyFigure (fig);
	fig = next;
    }

    DW_CacheDestroy (dw -> drawing.font_cache);
    DW_CacheDestroy (dw -> drawing.color_cache);
}


/************************************************************************
   Function:	Redisplay
   Description:	Redisplays the widget.
 ************************************************************************/

static void Redisplay (gw, event, region)
    Widget  gw;
    XEvent *event;
    Region  region;
{
    Display	 *display;
    DrawingWidget dw;


    dw = (DrawingWidget) gw;
    display = XtDisplay (gw);

    if (dw -> core.visible == True) {
	XIntersectRegion (region, dw -> drawing.region, region);
	XSetRegion (display, dw -> drawing.gridgc, region);
	XSetRegion (display, dw -> drawing.drawgc, region);

	if (dw -> drawing.grid == True)
	    DrawGrid (gw);

	DrawList (dw);
	XSetRegion (display, dw -> drawing.gridgc, dw -> drawing.region);
	XSetRegion (display, dw -> drawing.drawgc, dw -> drawing.region);
    }
}


/************************************************************************
   Function:	SetValues
   Description:	Checks validity of new values and updates private
		information.
 ************************************************************************/

static Boolean SetValues (old, request, new, argv, argc)
    Widget   old;
    Widget   request;
    Widget   new;
    ArgList  argv;
    Cardinal argc;
{
    Boolean       resize;
    Boolean	  rescale;
    Boolean       redisplay;
    DrawingWidget odw;
    DrawingWidget ndw;


    odw = (DrawingWidget) old;
    ndw = (DrawingWidget) new;

    resize    = False;
    rescale   = False;
    redisplay = False;


    if (ndw -> drawing.xMin >= ndw -> drawing.xMax) {
	ndw -> drawing.xMin = odw -> drawing.xMin;
	ndw -> drawing.xMax = odw -> drawing.xMax;
    }

    if (ndw -> drawing.yMin >= ndw -> drawing.yMax) {
	ndw -> drawing.yMin = odw -> drawing.yMin;
	ndw -> drawing.yMax = odw -> drawing.yMax;
    }

    if (ndw -> drawing.xScale <= 0)
	ndw -> drawing.xScale = odw -> drawing.xScale;

    if (ndw -> drawing.yScale <= 0)
	ndw -> drawing.yScale = odw -> drawing.yScale;

    if (ndw -> drawing.gridSize <= 0)
	ndw -> drawing.gridSize = odw -> drawing.gridSize;

    if (ndw -> drawing.snapSize <= 0)
	ndw -> drawing.snapSize = odw -> drawing.snapSize;


    ndw -> drawing.width  = (ndw -> drawing.xMax - ndw -> drawing.xMin) *
			     ndw -> drawing.xScale;
    ndw -> drawing.height = (ndw -> drawing.yMax - ndw -> drawing.yMin) *
			     ndw -> drawing.yScale;

    ndw -> drawing.xprecision  = ceil (log10 (ndw -> drawing.xScale));
    ndw -> drawing.yprecision  = ceil (log10 (ndw -> drawing.yScale));


    if (odw -> drawing.xMin != ndw -> drawing.xMin)
	redisplay = rescale = True;
    if (odw -> drawing.xMax != ndw -> drawing.xMax)
	redisplay = rescale = True;
    if (odw -> drawing.yMin != ndw -> drawing.yMin)
	redisplay = rescale = True;
    if (odw -> drawing.yMax != ndw -> drawing.yMax)
	redisplay = rescale = True;
    if (odw -> drawing.xScale != ndw -> drawing.xScale)
	redisplay = rescale = True;
    if (odw -> drawing.yScale != ndw -> drawing.yScale)
	redisplay = rescale = True;
    if (odw -> drawing.width != ndw -> drawing.width)
	redisplay = resize = True;
    if (odw -> drawing.height != ndw -> drawing.height)
	redisplay = resize = True;
    if (odw -> drawing.gridSize != ndw -> drawing.gridSize)
	redisplay = resize = True;
    if (odw -> drawing.grid != ndw -> drawing.grid)
	redisplay = True;

    if (odw -> drawing.interactive != ndw -> drawing.interactive)
	DW_SetInteractive (new, ndw -> drawing.interactive);

    if (odw -> drawing.redraw != ndw -> drawing.redraw)
	DW_SetAutoRedraw (new, ndw -> drawing.redraw);


    ndw -> core.width = ndw -> drawing.width;
    ndw -> core.height = ndw -> drawing.height;


    if (resize == True)
	SetClipRegion (ndw);

    if (rescale == True)
	ScaleList (ndw);

    return redisplay;
}
