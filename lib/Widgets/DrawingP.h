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
 * File:	DrawingP.h						*
 *									*
 * Description:	This file contains the widget definition for the	*
 *		Drawing widget.						*
 ************************************************************************/

# ifndef _DrawingP_h
# define _DrawingP_h

# include "Drawing.h"
# include "Figure.h"
# include <X11/Xaw/SimpleP.h>

#ifdef __cplusplus
extern "C" {
#endif 

/*----------------------------------------------------------------------*/

typedef struct {
    int empty;
} DrawingClassPart;


typedef struct _DrawingClassRec {
    CoreClassPart	core_class;
    SimpleClassPart	simple_class;
    DrawingClassPart	drawing_class;
} DrawingClassRec;

extern DrawingClassRec drawingClassRec;


typedef struct {
    /* resources */
    Pixel		foreground;
    XFontStruct	       *font;
    Widget		coord;
    Boolean		grid;
    float		gridSize;
    Boolean		snap;
    float		snapSize;
    float		xMin;
    float		xMax;
    float		yMin;
    float		yMax;
    float		xScale;
    float		yScale;
    XtCallbackList	button;
    XtCallbackList	motion;
    Boolean		interactive;
    Boolean		search;
    Boolean		redraw;
    /* private state */
    Dimension		width;
    Dimension		height;
    Region		region;
    Region		nullRegion;
    Region		bufferRegion;
    GC			gridgc;
    GC			drawgc;
    GC			intergc;
    int			xprecision;
    int			yprecision;
    unsigned		line_width;
    int			line_style;
    Figure		head;
    Figure		tail;
    Pixel		fg;
    Pixel		last_fg;
    unsigned		last_width;
    int			last_style;
    Font		last_font;
    Cache		color_cache;
    CacheData		color_data;
    Cache		font_cache;
    CacheData		font_data;
} DrawingPart;


typedef struct _DrawingRec {
    CorePart		core;
    SimplePart		simple;
    DrawingPart		drawing;
} DrawingRec;

/*----------------------------------------------------------------------*/

#ifdef __cplusplus
}
#endif 

# endif /* _DrawingP_h */
