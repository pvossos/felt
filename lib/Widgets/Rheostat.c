/*************************************************************
 *
 * Rheostat.c
 * Rheostat widget implementation
 *
 * Author: Joe English, joe@trystero.art.com
 *
 *************************************************************
 *
 * Future enhancements:
 *
 * BUG: If you drag the arrow to a position outside the valid
 *    range too quickly, it sticks inside the dial; it should
 *    peg out at the minimum or maximum value.  I'm not sure
 *    how to determine which, though...
 * write query_geometry method.
 * Add set(value) action
 * SetValues() doesn't check for as much as it should;
 * SetValues() and Initialize() need to do MUCH more range checking
 * When number_intervals is is not a divisor of ValueRange, ticks
 *    aren't drawn where the arrow actually goes.
 * Do we need a tickThickness resource?
 */

#include <stdlib.h>
#include <math.h>

#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>

#include "RheostatP.h"
#include "Rheostat.h"


/***********************************************************************
 *
 * Convenience macros, defaults, and declarations:
 *
 ***********************************************************************/

#define RADIANS(d)  (M_PI * (d)/ 180.0)
#define DEGREES(r)  ((r) * 180.0 / M_PI)
#define DEG_TO_RAD (180.0 / M_PI)
#define RAD_TO_DEG (M_PI / 180.0)
#define MIN(x,y)	((x) < (y) ? (x) : (y))
#define MAX(x,y)	((x) > (y) ? (x) : (y))

/* Rheostat-specific: */
#define MinAngle(w)     (w->rheostat.minimum_angle)
#define MaxAngle(w)     (w->rheostat.maximum_angle)
#define MinValue(w)     (w->rheostat.minimum_value)
#define MaxValue(w)     (w->rheostat.maximum_value)
#define AngleRange(w)   (w->rheostat.maximum_angle-w->rheostat.minimum_angle)
#define ValueRange(w)   (w->rheostat.maximum_value-w->rheostat.minimum_value)
#define ValueInc(w)   	((w->rheostat.maximum_value-w->rheostat.minimum_value) \
			 / w->rheostat.number_intervals)
#define CenterX(w)      ((int)w->core.width/2)
#define CenterY(w)      ((int)w->core.height/2)

/* MARGIN(w) = space from border to radius. */
#define _MARGIN(w) \
    ( w->rheostat.outer_margin \
    + w->rheostat.tick_length \
    + w->rheostat.dial_thickness \
    + w->rheostat.inner_margin)
#ifdef MOTIF
#define MARGIN(w) (_MARGIN(w)  \
    + w->primitive.highlight_thickness  \
    + w->primitive.shadow_thickness)
#else
#define MARGIN(w) _MARGIN(w)
#endif

/* Cast operator to keep lint/gcc happy: */
#define W (Widget)

/*
 * Default values for various resources:
 */
#define MIN_RADIUS	2
#define DFLT_RADIUS	30
#define DFLT_OUTER	25
#define DFLT_INNER	20
#define DFLT_WIDTH	20
#define DFLT_ARROWTHICKNESS	0
/* Another good set of defaults: RADIUS=INNER=30, OUTER=15, WIDTH=10 */

#define DFLT_TICKLEN	5
#define DFLT_TICKTHICKNESS	0
#define DFLT_DIALTHICKNESS	3
#define DFLT_MINVALUE	0
#define DFLT_MAXVALUE	100
#define DFLT_MINANGLE	45
#define DFLT_MAXANGLE	315

#define DFLT_NUMTICKS	10
#define DFLT_MARGIN	2

/*
 * Method functions:
 */
static void     Initialize();
static void     Redisplay();
static void     Resize();
static void     Destroy();
static Boolean  SetValues();

/*
 * Action functions:
 */
static void     Set();
static void     Notify();
static void     Increment();

/*
 * Private functions:
 */
static void     draw_arrow	(/* RheostatWidget, GC */);
static void     draw_ticks      (/* RheostatWidget, GC */);
static void     draw_dial      	(/* RheostatWidget, GC */);
static void     calculate_position	(/* RheostatWidget */);
static void     get_GCs     	(/* RheostatWidget */);
static void     free_GCs     	(/* RheostatWidget */);
static void	call_callbacks	(/* RheostatWidget *, String, XEvent * */);

/***********************************************************************
 *
 * Translation, action, and resource tables:
 *
 ***********************************************************************/

#ifdef MOTIF		/* use osfXXX instead of XXX */
static char     default_translations[] =
    "<Btn1Down>:        set() \n\
     <Btn1Motion>:      set() \n\
     <Btn1Up>:      	notify() \n\
     <Key>minus:	increment(-1) notify() \n\
     <Key>plus:		increment(+1) notify() \n\
     <Key>osfPageUp:   	increment(+1i) notify() \n\
     <Key>osfPageDown:  increment(-1i) notify() \n\
     <Key>Return:	notify() \n\
    ";
#else			/* no "virtual keysym" braindamage */
static char     default_translations[] =
    "<Btn1Down>:        set() \n\
     <Btn1Motion>:      set() \n\
     <Btn1Up>:      	notify() \n\
     <Key>minus:	increment(-1) notify() \n\
     <Key>plus:		increment(+1) notify() \n\
     <Key>Prior:   	increment(-1i) notify() \n\
     <Key>Next:  	increment(+1i) notify() \n\
     <Key>Home:		increment(min) notify() \n\
     <Key>End:		increment(max) notify() \n\
     <Key>Return:	notify() \n\
    ";
#endif

static XtActionsRec actions[] = {
    { "set",    (XtActionProc) Set },
    { "notify", (XtActionProc) Notify },
    { "increment", (XtActionProc) Increment }
};

static XtResource resources[] = {
#   define OFFSET(x) (XtOffset(RheostatWidget, rheostat.x))
    {XtNvalue, XtCValue, XtRInt, sizeof(int),
        OFFSET(value), XtRImmediate, (caddr_t)0},
    {XtNminimumValue, XtCMinimum, XtRInt, sizeof(int),
        OFFSET(minimum_value), XtRImmediate, (caddr_t) DFLT_MINVALUE},
    {XtNmaximumValue, XtCMaximum, XtRInt, sizeof(int),
        OFFSET(maximum_value), XtRImmediate, (caddr_t) DFLT_MAXVALUE},
    {XtNminimumAngle, XtCMinimum, XtRInt, sizeof(int),
        OFFSET(minimum_angle), XtRImmediate, (caddr_t) DFLT_MINANGLE},
    {XtNmaximumAngle, XtCMaximum, XtRInt, sizeof(int),
        OFFSET(maximum_angle), XtRImmediate, (caddr_t) DFLT_MAXANGLE},
    {XtNtickGravity, XtCGravity, XtRBoolean, sizeof(Boolean),
        OFFSET(tick_gravity), XtRImmediate, (caddr_t)True},
    {XtNnumberIntervals, XtCNumberIntervals, XtRInt, sizeof(int),
        OFFSET(number_intervals), XtRImmediate, (caddr_t) DFLT_NUMTICKS},
    {XtNresizeArrow, XtCBoolean, XtRBoolean, sizeof(Boolean),
	OFFSET(resize_arrow), XtRString, "True"},
    {XtNsetCallback, XtCCallback, XtRCallback, sizeof(XtPointer),
        OFFSET(set), XtRCallback, NULL},
    {XtNnotify, XtCCallback, XtRCallback, sizeof(XtPointer),
        OFFSET(notify), XtRCallback, NULL},
    {XtNouterMargin, XtCMargin, XtRDimension, sizeof(Dimension),
	OFFSET(outer_margin), XtRImmediate, (caddr_t) DFLT_MARGIN},
    {XtNtickLength, XtCMargin, XtRDimension, sizeof(Dimension),
        OFFSET(tick_length), XtRImmediate, (caddr_t) DFLT_TICKLEN},
    {XtNdialThickness, XtCThickness, XtRDimension, sizeof(Dimension),
        OFFSET(dial_thickness), XtRImmediate, (caddr_t) DFLT_DIALTHICKNESS},
    {XtNinnerMargin, XtCMargin, XtRDimension, sizeof(Dimension),
	OFFSET(inner_margin), XtRImmediate, (caddr_t) DFLT_MARGIN},
    {XtNradius, XtCLength, XtRDimension, sizeof(Dimension),
        OFFSET(radius), XtRImmediate, (caddr_t) DFLT_RADIUS},
    {XtNouterArrowLength, XtCLength, XtRDimension, sizeof(Dimension),
	OFFSET(outer_arrow_length), XtRImmediate, (caddr_t) DFLT_OUTER},
    {XtNinnerArrowLength, XtCLength, XtRDimension, sizeof(Dimension),
	OFFSET(inner_arrow_length), XtRImmediate, (caddr_t) DFLT_INNER},
    {XtNarrowWidth, XtCWidth, XtRDimension, sizeof(Dimension),
	OFFSET(arrow_width), XtRImmediate, (caddr_t) DFLT_WIDTH},
    {XtNfillArrow, XtCBoolean, XtRBoolean, sizeof(Boolean),
	OFFSET(fill_arrow), XtRString, "False" },
    {XtNarrowThickness, XtCThickness, XtRDimension, sizeof(Dimension),
	OFFSET(arrow_thickness), XtRImmediate, (caddr_t) DFLT_ARROWTHICKNESS},
    {XtNtickThickness, XtCThickness, XtRDimension, sizeof(Dimension),
        OFFSET(tick_thickness), XtRImmediate, (caddr_t) DFLT_TICKTHICKNESS},
#ifdef MOTIF
    {XtNuseShadowColors, XtCBoolean, XtRBoolean, sizeof(Boolean),
	OFFSET(use_shadow_colors), XtRString, "False"},
#endif
    {XtNarrowColor, XtCForeground, XtRPixel, sizeof(Pixel),
        OFFSET(arrow_pixel), XtRString, XtDefaultForeground},
    {XtNdialColor, XtCForeground, XtRPixel, sizeof(Pixel),
        OFFSET(dial_pixel), XtRString, XtDefaultForeground},
    {XtNtickColor, XtCForeground, XtRPixel, sizeof(Pixel),
        OFFSET(tick_pixel), XtRString, XtDefaultForeground},
#   undef OFFSET
};

RheostatClassRec    rheostatClassRec =
{
    /* CoreClassPart */
    {
#ifdef MOTIF
	/* superclass            */	(WidgetClass) &xmPrimitiveClassRec,
#else
	/* superclass            */	(WidgetClass) &widgetClassRec,
#endif
	/* class_name            */	"Rheostat",
	/* widget_size           */	sizeof(RheostatRec),
	/* class_initialize      */	NULL,
	/* class_part_initialize */	NULL,
	/* class_inited          */	FALSE,
	/* initialize            */	Initialize,
	/* initialize_hook       */	NULL,
	/* realize               */	XtInheritRealize,
	/* actions               */	actions,
	/* num_actions           */	XtNumber(actions),
	/* resources             */	resources,
	/* num_resources         */	XtNumber(resources),
	/* xrm_class             */	NULLQUARK,
	/* compress_motion       */	TRUE,
	/* compress_exposure     */	TRUE,
	/* compress_enterleave   */	TRUE,
	/* visible_interest      */	TRUE,
	/* destroy               */	Destroy,
	/* resize                */	Resize,
	/* expose                */	Redisplay,
	/* set_values            */	SetValues,
	/* set_values_hook       */	NULL,
	/* set_values_almost     */	XtInheritSetValuesAlmost,
	/* get_values_hook       */	NULL,
	/* accept_focus          */	NULL,
	/* version               */	XtVersion,
	/* callback private      */	NULL,
	/* tm_table              */	default_translations,
	/* query_geometry        */	XtInheritQueryGeometry,
	/* display_accelerator   */	XtInheritDisplayAccelerator,
	/* extension             */	NULL
    },
#ifdef MOTIF
    /* Primitive class fields */
    {
	/* border_highlight      */	(XtWidgetProc) _XtInherit,
	/* border_unhighlight    */	(XtWidgetProc) _XtInherit,
	/* translations          */	XtInheritTranslations,
	/* arm_and_activate      */	(XmArmAndActivate)Notify,
	/* syn_resources         */	NULL,
	/* num_syn_resources     */	0,
	/* extension             */	NULL
    },
#endif
    /* Rheostat class fields */
    {
	/* ignore                */	0
    }
};

WidgetClass     rheostatWidgetClass = (WidgetClass) &rheostatClassRec;

/***********************************************************************
 *
 * Method functions:
 *
 **********************************************************************/

/*
 * Initialize method:
 */
static void Initialize(request, new)
    RheostatWidget      request, new;
{
    int margin = MARGIN(new);
    int user_radius = new->rheostat.radius;	/* request from user */
    int size_radius = 				/* calculated from size */
	MIN(new->core.height,new->core.width)/2 - margin;
    int min_radius = 			/* from arrow dimens */
	MAX(new->rheostat.inner_arrow_length,new->rheostat.outer_arrow_length);
    int min_dimen;

    /*
     * Check geometry:
     * Set radius from  user value, else widget size, else default.
     * Make sure radius is >= inner length & outer length.
     *  %%% This is a bit restrictive -- e.g., r=10,i=10,o=12 should be allowed
     * Make sure width and height are >= 2*(radius + margins)
     */
    if (user_radius != 0)
	new->rheostat.radius = user_radius;
    else if (new->core.width != 0 && new->core.height != 0)
	new->rheostat.radius = size_radius;
    else
        new->rheostat.radius = DFLT_RADIUS;

    /* Make sure radius is large enough: */
    if (new->rheostat.radius < min_radius)
	new->rheostat.radius = min_radius;

    /* Make sure widget is large enough: */
    min_dimen = 2*(new->rheostat.radius+margin);
    if (new->core.width < min_dimen)
	new->core.width = min_dimen;
    if (new->core.height < min_dimen)
	new->core.height = min_dimen;

#ifdef MOTIF
    if (new->rheostat.use_shadow_colors) {
	new->rheostat.arrow_pixel = new->primitive.bottom_shadow_color;
	new->rheostat.dial_pixel = new->primitive.top_shadow_color;
    }
#endif
    get_GCs(new);
    calculate_position(new);

    new->rheostat.orig_radius = new->rheostat.radius;
    new->rheostat.orig_outer_length = new->rheostat.outer_arrow_length;
    new->rheostat.orig_inner_length = new->rheostat.inner_arrow_length;
    new->rheostat.orig_width = new->rheostat.arrow_width;
}

/*
 * Destroy method:
 */
static void Destroy(w)
    RheostatWidget      w;
{
    free_GCs(w);
}

/*
 * Resize method:
 */
static void Resize(w)
    RheostatWidget      w;
{
    int newr;

    newr =
       (w->core.height < w->core.width
      ? w->core.height : w->core.width) / 2 - MARGIN(w);

    if (newr < 2)
	newr = 2;

    if (w->rheostat.resize_arrow) {
	int oldr = w->rheostat.orig_radius;

	w->rheostat.outer_arrow_length =
		(w->rheostat.orig_outer_length * newr) / oldr;
	w->rheostat.inner_arrow_length =
		(w->rheostat.orig_inner_length * newr) / oldr;
	w->rheostat.arrow_width = (w->rheostat.orig_width * newr) / oldr;
    }
    w->rheostat.radius = newr;

    calculate_position(w);
}

/*
 * Expose method:
 */
static void Redisplay(w, event, region)
    RheostatWidget      w;
    XEvent         *event;
    Region          region;
{
#	ifdef MOTIF
	int hlt = w->primitive.highlight_thickness;
	_XmDrawShadow(XtDisplay(w), XtWindow(w),
		w->primitive.top_shadow_GC, w->primitive.bottom_shadow_GC,
		w->primitive.shadow_thickness,
		hlt, hlt, w->core.width - 2*hlt, w->core.height - 2*hlt);
#	endif

	draw_ticks(w, w->rheostat.tick_GC);
	draw_dial(w, w->rheostat.dial_GC);
        draw_arrow(w, w->rheostat.arrow_GC);
}

/*
 * SetValues:
 */
static Boolean SetValues(current, request, new)
    RheostatWidget      current, request, new;
{
    Boolean redraw = FALSE;		/* TRUE=>widget needs to be redrawn */
    Boolean recalc = FALSE;		/* TRUE=>arrow position changed */
#   define  CHECK(fld)  (new->fld != current->fld)

    /*
     * Check rheostat parameters:
     */
    if (   CHECK(rheostat.value)
        || CHECK(rheostat.minimum_value) || CHECK(rheostat.maximum_value)
        || CHECK(rheostat.maximum_angle) || CHECK(rheostat.minimum_angle)
    )
    {
        recalc=TRUE;
        redraw=TRUE;
    }

    /*
     * Bounds check:
     */
    if (new->rheostat.value > new->rheostat.maximum_value)
        new->rheostat.value = new->rheostat.maximum_value;
    if (new->rheostat.value < new->rheostat.minimum_value)
        new->rheostat.value = new->rheostat.minimum_value;

    /*
     * Margin, geometry parameters -- may affect radius;
     */
    if (    CHECK(rheostat.outer_margin)
	 || CHECK(rheostat.dial_thickness)
	 || CHECK(rheostat.inner_margin)
#ifdef MOTIF
	 || CHECK(primitive.shadow_thickness)
	 || CHECK(primitive.highlight_thickness)
#endif
    )
    {
	int newr =
	   (new->core.height < new->core.width
	  ? new->core.height : new->core.width) / 2 - MARGIN(new);
	if (newr < MIN_RADIUS)
	    newr = MIN_RADIUS;
	new->rheostat.radius = newr;
	recalc=TRUE;
    }

    /*
     * Radius and arrow sizes:
     */
    if (   CHECK(rheostat.radius)
	|| CHECK(rheostat.outer_arrow_length)
	|| CHECK(rheostat.inner_arrow_length)
	|| CHECK(rheostat.arrow_width)
    )
    {
	new->rheostat.orig_radius = new->rheostat.radius;
	new->rheostat.orig_outer_length = new->rheostat.outer_arrow_length;
	new->rheostat.orig_inner_length = new->rheostat.inner_arrow_length;
	new->rheostat.orig_width = new->rheostat.arrow_width;
	recalc = TRUE;
	redraw = TRUE;
    }

    /*
     * Check for color change:
     */
    if (   CHECK(core.background_pixel)
        || CHECK(rheostat.tick_pixel)
        || CHECK(rheostat.dial_pixel)
        || CHECK(rheostat.arrow_pixel)
	|| CHECK(rheostat.arrow_thickness)
	|| CHECK(rheostat.dial_thickness)
    )
    {
        get_GCs(new);
	free_GCs(current);
        redraw = TRUE;
    }

    /*
     * Other display resources:
     */
    if (   CHECK(rheostat.number_intervals)
	|| CHECK(rheostat.fill_arrow)
    )
    {
	redraw = TRUE;
    }

    /*
     * Wrap up:
     */
#   undef CHECK
    if (recalc)
	calculate_position(new);
    return redraw;
}

/***********************************************************************
 *
 * Action functions:
 *
 ***********************************************************************/

    /*ARGSUSED*/
static void Set(w, event, params, nparams)
    RheostatWidget	w;
    XEvent	*event;
    String	*params;
    Cardinal	*nparams;
{
    if (event->type == ButtonPress || event->type == MotionNotify)
    {
        int x,y,v;
        double theta,length,radius;

        x = event->xbutton.x - CenterX(w);
        y = event->xbutton.y - CenterY(w);

        if (!x && !y)       /* click at center of widget -- no angle */
            return;
        /* else */

        radius = sqrt((double)(x*x + y*y));

        /*
         * Calculate value in range 0 .. 360
         */
        theta = DEGREES(atan2((double)(-x), (double)(y)));
	theta -= w->rheostat.minimum_angle;
	if (theta < 0.0)
	    theta += 360.0;

        v = (int)(
            theta * (double)ValueRange(w)
          / (double)AngleRange(w)
        ) + w->rheostat.minimum_value;


	/*
	 * If tick_gravity is on, and click is in tick region,
	 * snap to nearest increment:
	 */
        length = (double)(w->rheostat.radius + w->rheostat.inner_margin);
	if (   w->rheostat.tick_gravity
	    && radius >= length
	    && radius <= length
		+ w->rheostat.dial_thickness + w->rheostat.tick_length)
	{
	    double inc = ValueInc(w);
	    v = (v / inc + 0.5);
	    v *= inc;
	}

	/*
	 * Bounds-check:
	 * Note: v should never be < minimum_value.
	 */
        if (v > w->rheostat.maximum_value || v < w->rheostat.minimum_value)
	    return;

        draw_arrow(w,w->rheostat.eraser_GC);
	w->rheostat.value = v;
	calculate_position(w);
        draw_arrow(w,w->rheostat.arrow_GC);
    }

    call_callbacks(w, XtNsetCallback, event);
}


static void Increment(w, event, params, nparams)
    RheostatWidget      w;
    XEvent         *event;
    String 	   *params;
    Cardinal	   *nparams;
{
    double inc = ValueInc(w);
    Boolean snap = False;
    double v;

    /*
     * Figure out increment from parameter:
     */
    v = w->rheostat.value;
    if (*nparams != 1) {	/* default: step to nearest interval: */
	v += inc;
	snap = True;
    } else {
	if (!strcmp(params[0],"max")) v = w->rheostat.maximum_value;
	else if (!strcmp(params[0],"min")) v = w->rheostat.minimum_value;
	else if (!strcmp(params[0],"+1i")) { v += inc; snap = True; }
	else if (!strcmp(params[0],"-1i")) { v -= inc; snap = True; }
	else v += atof(params[0]);
    }
    if (snap)
	v = (int)(v / inc + 0.5) * inc;

    /*
     * Bounds-check:
     */
    if (v > w->rheostat.maximum_value)
	v = w->rheostat.maximum_value;
    if (v < w->rheostat.minimum_value)
	v = w->rheostat.minimum_value;

    draw_arrow(w,w->rheostat.eraser_GC);
    w->rheostat.value = v;
    calculate_position(w);
    draw_arrow(w,w->rheostat.arrow_GC);

    call_callbacks(w,XtNsetCallback,event);
}

    /*ARGSUSED*/
static void Notify(w, event, params, nparams)
    RheostatWidget      w;
    XEvent         *event;
    String 	   *params;
    Cardinal	   *nparams;
{
    call_callbacks(w, XtNnotify, event);
}


/***********************************************************************
 *
 * Utility routines:
 *
 ***********************************************************************/
static void call_callbacks(w, callback_name, event)
    RheostatWidget w;
    char *callback_name;
    XEvent *event;
{
    RheostatCallbackStruct cb;

    cb.reason = 0;		/* this is never used, even under Motif */
    cb.event = event;
    cb.value = w->rheostat.value;

    XtCallCallbacks(W w, callback_name, (XtPointer)&cb);
}


static void draw_arrow(w, gc)
    RheostatWidget      w;
    GC              gc;

{
    XfwfDrawArrow(XtDisplay(w), XtWindow(w), gc,
	w->rheostat.tip_x, w->rheostat.tip_y,
	w->rheostat.tip_x - CenterX(w), w->rheostat.tip_y - CenterY(w),
	w->rheostat.outer_arrow_length,
	w->rheostat.inner_arrow_length,
	w->rheostat.arrow_width,
	w->rheostat.fill_arrow);
}

static void draw_dial(w, gc)
    RheostatWidget      w;
    GC     	         gc;
{
    int radius  = w->rheostat.radius
	+ w->rheostat.inner_margin
	+(w->rheostat.dial_thickness+1) / 2;

    XDrawArc(XtDisplay(w), XtWindow(w), w->rheostat.dial_GC,
	CenterX(w) - radius,
	CenterY(w) - radius,
	2*radius, 2*radius,
	64 * ((270 - w->rheostat.minimum_angle + 360) % 360),
	64 * -AngleRange(w)
    );
}

static void draw_ticks(w, gc)
    RheostatWidget      w;
    GC              gc;
{
    int i,cx,cy;
    double theta,inc;
    double ro,ri;       /* inner & outer radii of ticks */


    if (!w->rheostat.number_intervals)
	return;

    /*
     * %%% should check if minimum_angle == maximum_angle (mod 360),
     * so the last tick doesn't coincide with the first.
     */
    inc = RADIANS((double)AngleRange(w))/(double)(w->rheostat.number_intervals);
    ri = (double)(w->rheostat.radius + w->rheostat.inner_margin
		+ (w->rheostat.dial_thickness+1)/2);
    ro = ri +  w->rheostat.tick_length + (w->rheostat.dial_thickness+1)/2;
    cx = CenterX(w);
    cy = CenterY(w);

    /*
     * Draw segments:
     */
    theta = RADIANS((double)MinAngle(w));
    i = w->rheostat.number_intervals + 1;
    while (i--) {
        double c = cos(theta);
        double s = sin(theta);

	XDrawLine(XtDisplay(w),XtWindow(w),gc,
	    cx - (int)(ro * s), cy + (int)(ro * c),
	    cx - (int)(ri * s), cy + (int)(ri * c));
        theta += inc;
    }
}

static void calculate_position(w)
    RheostatWidget      w;
{
    double theta,length;

    length = (double)w->rheostat.radius;
    /*
     * Calculate angle: theta = V*(maxTheta-minTheta) / (maxV - minV) + minTheta
     */
    theta = (double)(w->rheostat.value)
          * (double)(AngleRange(w))
          / (double)(ValueRange(w))
          + (double)(MinAngle(w));
    theta = RADIANS(theta);

    w->rheostat.tip_x = CenterX(w) - (int)(length * sin(theta));
    w->rheostat.tip_y = CenterY(w) + (int)(length * cos(theta));
}

/*
 * get_GCs
 * allocate foreground & background. GCs.
 */
static void get_GCs(w)
    RheostatWidget w;
{
    XGCValues       values;
    XtGCMask        mask;

    /*
     * dial:
     */
    mask = GCForeground | GCBackground | GCLineWidth | GCCapStyle;
    values.foreground = w->rheostat.dial_pixel;
    values.background = w->core.background_pixel;
    values.line_width = w->rheostat.dial_thickness;
    values.cap_style = CapRound;
    w->rheostat.dial_GC = XtGetGC(W w, mask, &values);

    /*
     * tick marks:
     */
    mask = GCForeground | GCBackground | GCFunction | GCLineWidth | GCCapStyle;
    values.foreground = w->rheostat.tick_pixel;
    values.background = w->core.background_pixel;
    values.function = GXcopy;
    values.line_width = w->rheostat.tick_thickness;
    values.cap_style = CapRound;
    w->rheostat.tick_GC = XtGetGC(W w, mask, &values);

    /*
     * Arrow:
     */
    mask = GCForeground | GCBackground | GCFunction | GCLineWidth
	 | GCCapStyle | GCFillStyle;
    values.foreground = w->rheostat.arrow_pixel;
    values.background = w->core.background_pixel;
    values.line_width = w->rheostat.arrow_thickness;
    values.cap_style = CapRound;
    values.fill_style = FillSolid;
    w->rheostat.arrow_GC = XtGetGC(W w, mask, &values);

    /*
     * Eraser (identical to Arrow except fg & bg pixels are swapped):
     */
    values.foreground = w->core.background_pixel;
    values.background = w->rheostat.arrow_pixel;
    w->rheostat.eraser_GC = XtGetGC(W w, mask, &values);

    return;
}


static void free_GCs(w)
    RheostatWidget w;
{
    XtReleaseGC(W w,w->rheostat.arrow_GC);
    XtReleaseGC(W w,w->rheostat.eraser_GC);
    XtReleaseGC(W w,w->rheostat.dial_GC);
    XtReleaseGC(W w,w->rheostat.tick_GC);
    return;
}

void XfwfDrawArrow(dpy, d, gc, endx, endy, dx, dy,
		       outer_length, inner_length, width, fill)
    Display *dpy;
    Drawable d;
    GC gc;
    Position endx, endy;		/* position of arrow tip */
    int dx, dy;				/* slope of arrow */
    Dimension outer_length;		/* distance tip->base */
    Dimension inner_length;		/* distance tip->inner */
    Dimension width;			/* distance base->outer points */
    Boolean fill;			/* True=>fill arrow,False=>outline */

{
    XPoint points[5];
    float scalef = sqrt((double)(dx*dx+dy*dy)); /* normalization factor */
    int al = (int)outer_length,
	bl = (int)inner_length,
	aw = (int)width / 2,
	lx = al *  dx / scalef,	/* distance from tip to base */
	ly = al *  dy / scalef,
	mx = bl *  dx / scalef,	/* distance from tip to middle point */
	my = bl *  dy / scalef,
	wx = aw * -dy / scalef,	/* distance from base to outer points */
	wy = aw *  dx / scalef;

    points[0].x = endx;  	points[0].y = endy;
    points[1].x = endx-lx + wx;	points[1].y = endy-ly + wy;
    points[2].x = endx-mx;	points[2].y = endy-my;
    points[3].x = endx-lx - wx;	points[3].y = endy-ly - wy;
    points[4].x = endx; 	points[4].y = endy;

    if (fill)
	XFillPolygon(dpy,d,gc,
		points,5,
		inner_length <= outer_length ? Nonconvex : Convex,
		CoordModeOrigin);
    else
	XDrawLines(dpy,d,gc,points,5,CoordModeOrigin);
}

/***********************************************************************
 *
 * Public functions:
 *
 ***********************************************************************/

/* RheostatSetIntCallback()
 * General-purpose callback function for Rheostat widgets;
 * gets the position of the Rheostat, sets *(int *)closure
 */

void XfwfRheostatSetIntCallback(w,closure,call_data)
    Widget w;
    XtPointer closure;
    XtPointer call_data;
{
    *((int *)closure) = ((RheostatCallbackStruct *)call_data)->value;
}
