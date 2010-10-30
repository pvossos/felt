/*************************************************************
 *
 * RheostatP.h
 * Private header file for Rheostat widget.
 *
 * Author: Joe English, joe@trystero.art.com
 *
 *************************************************************
 */

#ifndef RHEOSTATP_H
#define RHEOSTATP_H

#ifdef __cplusplus
extern "C" {
#endif 

/*----------------------------------------------------------------------*/

typedef struct _RheostatClassPart {
    int             hosebrain;
} RheostatClassPart;

typedef struct _RheostatClassRec {
    CoreClassPart   		core_class;
#   ifdef MOTIF
    XmPrimitiveClassPart	primitive_class;
#   endif
    RheostatClassPart   	rheostat_class;
} RheostatClassRec;

extern RheostatClassRec rheostatClassRec;

typedef struct _RheostatPart 
{
    int             	value;              	/* value */
    int             	minimum_value;      	/* minimum value */
    int             	maximum_value;      	/* maximum value */
    int             	minimum_angle;      	/* angle of minimum value */
    int             	maximum_angle;		/* angle of maximum value */
    int   	 	number_intervals;	/* #tick mark intervals */
    Boolean		tick_gravity;	/* click by tick sticks to tick? */
    Boolean		resize_arrow;	/* resize arrow? */

    XtCallbackList	set;            /* callbacks for Set() action */
    XtCallbackList	notify;         /* callbacks for Notify() */

    Dimension		outer_margin;
    Dimension		tick_length;
    Dimension		dial_thickness;
    Dimension		inner_margin;
    Dimension		radius;
    Dimension		outer_arrow_length;	/* from tip to outer points */
    Dimension		inner_arrow_length;	/* from tip to inner point */
    Dimension		arrow_width;		/* width of arrow */
    Boolean		fill_arrow;		/* filled/outlined arrow */
    Dimension		arrow_thickness;	/* line thickness if outlined */
    Dimension		tick_thickness;

#ifdef MOTIF
    Boolean		use_shadow_colors;	/* for arrow & dial */
#endif
    Pixel		arrow_pixel;		/* arrow color */
    Pixel		tick_pixel;		/* ticks  */
    Pixel		dial_pixel;		/* circular border */

    /* Private: */
    GC          	dial_GC;            	/* assorted GCs */
    GC          	arrow_GC;		/* to draw pointer */
    GC          	eraser_GC;		/* to erase pointer */
    GC          	tick_GC;		/* to erase pointer */
    Position    	tip_x;              	/* coords of arrow tip */
    Position    	tip_y;

    Dimension		orig_radius;		/* Used to resize arrow */
    Dimension		orig_outer_length;	/*  if resize_arrow True */
    Dimension		orig_inner_length;
    Dimension		orig_width;

} RheostatPart;

typedef struct _RheostatRec {
    CorePart        	core;
#   ifdef MOTIF
    XmPrimitivePart	primitive;
#   endif
    RheostatPart        rheostat;
} RheostatRec;

/*----------------------------------------------------------------------*/

#ifdef __cplusplus
}
#endif 

#endif  /* RHEOSTATP_H */
