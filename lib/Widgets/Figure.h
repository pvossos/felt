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
 * File:	Figure.h						*
 *									*
 * Description:	This file contains the private function declarations	*
 *		for the figures of the Drawing widget.			*
 ************************************************************************/

# ifndef _Figure_h
# define _Figure_h

# include "Drawing.h"
# include "Cache.h"

/*----------------------------------------------------------------------*/

# if NeedWidePrototypes
# define FLOAT double
# define BOOLEAN int
# else
# define FLOAT float
# define BOOLEAN Boolean
# endif

# define RealX(x)	(dw -> drawing.xMin + (x) / dw -> drawing.xScale)
# define RealY(y)	(dw -> drawing.yMax - (y) / dw -> drawing.yScale)
# define XAbs(x)        (dw -> drawing.xScale * ((x) - dw -> drawing.xMin))
# define YAbs(y)        (dw -> drawing.yScale * (dw -> drawing.yMax - (y)))
# define XRel(x)        ((x) * dw -> drawing.xScale)
# define YRel(y)        ((y) * dw -> drawing.yScale)
# define Round(x)       ((x) + .5)
# define Min(x,y)       ((x) < (y) ? (x) : (y))
# define Max(x,y)       ((x) > (y) ? (x) : (y))


struct figure {
    Figure		 prev;		/* previous figure in list	*/
    Figure		 next;		/* next figure in list		*/
    Figure		 group;		/* group leader			*/
    FigureType		 type;		/* type of figure		*/
    int			 x;		/* bounding box x coordinate	*/
    int			 y;		/* bounding box y coordinate	*/
    unsigned		 width;		/* bounding box width		*/
    unsigned		 height;	/* bounding box height		*/
    unsigned		 line_width;	/* line width			*/
    int			 line_style;	/* line style			*/
    Pixel		 fg;		/* foreground pixel		*/
    CacheData		 color_data;	/* cached color data		*/
    Boolean		 visible;	/* visible flag			*/
    boost::any userdata;	/* user data pointer		*/
    union {
	struct {
	    Point	 points [2];	/* array of real coordinates	*/
	    XPoint	 xpoints [2];	/* array of window coordinates	*/
	} line;
	struct {
	    int		 npoints;	/* number of points		*/
	    Point	*points;	/* array of real coordinates	*/
	    XPoint	*xpoints;	/* array of window coordinates	*/
	    Boolean	 filled;	/* filled flag			*/
	    Boolean	 scaled;	/* scaled flag			*/
	    Region	 region;	/* region			*/
	} polygon;
	struct {
	    float	 x;		/* real lower left x coordinate	*/
	    float	 y;		/* real lower left y coordinate	*/
	    float	 width;		/* real width			*/
	    float	 height;	/* real height			*/
	    Boolean	 filled;	/* filled flag			*/
	    Boolean	 scaled;	/* scaled flag			*/
	} rectangle;
	struct {
	    float	 x;		/* real center x coordinate	*/
	    float	 y;		/* real center y coordinate	*/
	    float	 width;		/* real width (x diameter)	*/
	    float	 height;	/* real height (y diameter)	*/
	    short	 start;		/* starting angle in degrees	*/
	    short	 length;	/* arc length in degrees	*/
	    Boolean	 filled;	/* filled flag			*/
	    Boolean	 scaled;	/* scaled flag			*/
	} arc;
	struct {
	    float	 rx;		/* real lower left x coordinate	*/
	    float	 ry;		/* real lower left y coordinate	*/
	    XFontStruct *font;		/* font structure		*/
	    CacheData	 font_data;	/* cached font data		*/
	    String	 string;	/* character string		*/
	    Boolean	 scaled;	/* scaled flag			*/
	    int		 length;	/* length of string		*/
	    int		 x;		/* window x coordinate		*/
	    int		 y;		/* window y coordinate		*/
	} text;
	struct {
	    Figure	*fig;		/* group members		*/
	    unsigned	 nfigs;		/* number of members		*/
	} group;
	struct {
	    float	 x;		/* real lower left x coordinate	*/
	    float	 y;		/* real lower left y coordinate	*/
	    Pixmap	 pixmap;	/* pixmap			*/
	} pixmap;
    } info;
};


Figure DW_CreateFigure (DrawingWidget dw, FigureType type, Boolean flag, int arg);

void DW_DestroyFigure (Figure fig);

void DW_AppendFigure (DrawingWidget dw, Figure fig);

void DW_PrependFigure (DrawingWidget dw, Figure fig);

void DW_DeleteFigure (DrawingWidget dw, Figure fig);

void DW_ScaleFigure (DrawingWidget dw, Figure fig);

void DW_DrawFigure (DrawingWidget dw, Figure fig);

void DW_ClearFigure (DrawingWidget dw, Figure fig);

void DW_Detach (DrawingWidget dw, Figure fig);

void DW_Attach (DrawingWidget dw, Figure fig, Figure group);

/*----------------------------------------------------------------------*/

# endif /* _Figure_h */
