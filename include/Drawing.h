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

# ifndef _Drawing_h
# define _Drawing_h

/****************************************************************
 *
 * Drawing widget
 *
 ****************************************************************/

/* Resources:

 Name		     Class		RepType		Default Value
 ----		     -----		-------		-------------
 background	     Background		Pixel		XtDefaultBackground
 border		     BorderColor	Pixel		XtDefaultForeground
 borderWidth	     BorderWidth	Dimension	1
 cursor		     Cursor		Cursor		crosshair
 cursorName	     CursorName		String		NULL
 destroyCallback     Callback		Pointer		NULL
 height		     Height		Dimension	0
 mappedWhenManaged   MappedWhenManaged	Boolean		True
 sensitive	     Sensitive		Boolean		True
 width		     Width		Dimension	0
 x		     Position		Position	0
 y		     Position		Position	0
 foreground	     Foreground		Pixel		XtDefaultForeground
 font		     Font		XFontStruct *	XtDefaultFont
 coordinates	     Coordinates	Widget		NULL
 grid		     Grid		Boolean		False
 gridSize	     GridSize		Float		1.00
 snap		     Snap		Boolean		False
 snapSize	     SnapSize		Float		0.25
 xMin		     XMin		Float		0.00
 xMax		     XMax		Float		10.0
 yMin		     YMin		Float		0.00
 yMax		     YMax		Float		10.0
 xScale		     XScale		Float		50.0
 yScale		     YScale		Float		50.0
 buttonCallback	     ButtonCallback	Callback	NULL
 motionCallback	     MotionCallback	Callback	NULL
 interactive	     Interactive        Boolean		False
 autoFind	     AutoFind		Boolean		False
 autoRedraw	     AutoRedraw		Boolean		True

*/


/* Define any special resource names here that are not in <X11/StringDefs.h> */

# define XtNcoordinates		"coordinates"
# define XtNgrid		"grid"
# define XtNgridSize		"gridSize"
# define XtNsnap		"snap"
# define XtNsnapSize		"snapSize"
# define XtNxMin		"xMin"
# define XtNxMax		"xMax"
# define XtNyMin		"yMin"
# define XtNyMax		"yMax"
# define XtNxScale		"xScale"
# define XtNyScale		"yScale"
# define XtNbuttonCallback	"buttonCallback"
# define XtNmotionCallback	"motionCallback"
# define XtNinteractive		"interactive"
# define XtNautoFind		"autoFind"
# define XtNautoRedraw		"autoRedraw"

# define XtCCoordinates		"Coordinates"
# define XtCGrid		"Grid"
# define XtCGridSize		"GridSize"
# define XtCSnap		"Snap"
# define XtCSnapSize		"SnapSize"
# define XtCXMin		"XMin"
# define XtCXMax		"XMax"
# define XtCYMin		"YMin"
# define XtCYMax		"YMax"
# define XtCXScale		"XScale"
# define XtCYScale		"YScale"
# define XtCButtonCallback	"ButtonCallback"
# define XtCMotionCallback	"MotionCallback"
# define XtCInteractive		"Interactive"
# define XtCAutoFind		"AutoFind"
# define XtCAutoRedraw		"AutoRedraw"


/* Declare specific DrawingWidget class and instance datatypes */

typedef struct _DrawingClassRec	*DrawingWidgetClass;
typedef struct _DrawingRec	*DrawingWidget;


/* Declare the class constant */

extern WidgetClass drawingWidgetClass;


/* Declare public types */

typedef struct figure *Figure;

typedef struct {
    float x, y;
} Point;

typedef struct {
    XEvent *event;
    Point   snapped;
    Point   unsnapped;
    Figure  figure;
} DrawingReport;

typedef enum {
    LineFigure, PolygonFigure, RectangleFigure, ArcFigure, TextFigure,
    GroupFigure, PixmapFigure, BitmapFigure
} FigureType;

typedef struct {
    FigureType   type;		/* all figures (read only)		    */
    Figure	 group;		/* all figures				    */
    unsigned	 line_width;	/* lines, polygons, rectangles, arcs	    */
    int		 line_style;	/* lines, polygons, rectangles, arcs	    */
    Boolean	 visible;	/* lines, polygons, rectangles, arcs, text  */
    Boolean	 filled;	/* polygons, rectangles, arcs		    */
    Boolean	 scaled;	/* polygons, rectangles, arcs, text	    */
    String	 color;		/* lines, polygons, rectangles, arcs, text  */
    String	 text;		/* text					    */
    String	 font;		/* text					    */
    XFontStruct	*font_struct;	/* text (read only)			    */
    float	 x;		/* rectangles, arcs, text, pixmaps, bitmaps */
    float	 y;		/* rectangles, arcs, text, pixmaps, bitmaps */
    float	 width;		/* rectangles, arcs			    */
    float	 height;	/* rectangles, arcs			    */
    float	 arc_start;	/* arcs					    */
    float	 arc_length;	/* arcs					    */
    Point       *points;	/* lines, polygons			    */
    unsigned	 npoints;	/* lines, polygons (read only)		    */
    Figure      *figures;	/* groups (read only)			    */
    unsigned	 nfigures;	/* groups (read only)			    */
    Pixmap	 pixmap;	/* pixmaps, bitmaps			    */
    char        *user_data;	/* all figures				    */
} FigureAttributes;

# define DW_FigureGroup		(1 << 0)
# define DW_FigureLineWidth	(1 << 1)
# define DW_FigureLineStyle	(1 << 2)
# define DW_FigureVisible	(1 << 3)
# define DW_FigureFilled	(1 << 4)
# define DW_FigureScaled	(1 << 5)
# define DW_FigureColor		(1 << 6)
# define DW_FigureFont		(1 << 7)
# define DW_FigureText		(1 << 8)
# define DW_FigureX		(1 << 9)
# define DW_FigureY		(1 << 10)
# define DW_FigureWidth		(1 << 11)
# define DW_FigureHeight	(1 << 12)
# define DW_FigureArcStart	(1 << 13)
# define DW_FigureArcLength	(1 << 14)
# define DW_FigurePoints	(1 << 15)
# define DW_FigurePixmap	(1 << 16)
# define DW_FigureUserData	(1 << 17)
# define DW_FigureLocation	(DW_FigureX | DW_FigureY)
# define DW_FigureSize		(DW_FigureWidth | DW_FigureHeight)
# define DW_FigureArc		(DW_FigureArcStart | DW_FigureArcLength)

# define DW_LineSolid		0
# define DW_LineDashed		1
# define DW_LineDotted		2
# define DW_LineDotDashed	3
# define DW_LineLongDashed	4


/* Public function declarations. */

# if NeedWidePrototypes
# define FLOAT double
# define BOOLEAN int
# else
# define FLOAT float
# define BOOLEAN Boolean
# endif


extern Figure DW_DrawLine (
# if NeedFunctionPrototypes
    Widget,		/* drawing		 */
    FLOAT,		/* starting x coordinate */
    FLOAT,		/* starting y coordinate */
    FLOAT,		/* ending x coordinate	 */
    FLOAT		/* ending y coordinate	 */
# endif
);


extern Figure DW_DrawPolygon (
# if NeedFunctionPrototypes
    Widget,		/* drawing	    */
    BOOLEAN,		/* scaled	    */
    Point [ ],		/* array of points  */
    int			/* number of points */
# endif
);


extern Figure DW_FillPolygon (
# if NeedFunctionPrototypes
    Widget,		/* drawing	    */
    BOOLEAN,		/* scaled	    */
    Point [ ],		/* array of points  */
    int			/* number of points */
# endif
);


extern Figure DW_DrawRectangle (
# if NeedFunctionPrototypes
    Widget,		/* drawing		   */
    BOOLEAN,		/* scaled		   */
    FLOAT,		/* lower left x coordinate */
    FLOAT,		/* lower left y coordinate */
    FLOAT,		/* width		   */
    FLOAT		/* height		   */
# endif
);


extern Figure DW_FillRectangle (
# if NeedFunctionPrototypes
    Widget,		/* drawing		   */
    BOOLEAN,		/* scaled		   */
    FLOAT,		/* lower left x coordinate */
    FLOAT,		/* lower left y coordinate */
    FLOAT,		/* width		   */
    FLOAT		/* height		   */
# endif
);


extern Figure DW_DrawArc (
# if NeedFunctionPrototypes
    Widget,		/* drawing	       */
    BOOLEAN,		/* scaled	       */
    FLOAT,		/* center x coordinate */
    FLOAT,		/* center y coordinate */
    FLOAT,		/* width	       */
    FLOAT,		/* height	       */
    FLOAT,		/* starting angle      */
    FLOAT		/* arc length	       */
# endif
);


extern Figure DW_FillArc (
# if NeedFunctionPrototypes
    Widget,		/* drawing	       */
    BOOLEAN,		/* scaled	       */
    FLOAT,		/* center x coordinate */
    FLOAT,		/* center y coordinate */
    FLOAT,		/* width	       */
    FLOAT,		/* height	       */
    FLOAT,		/* starting angle      */
    FLOAT		/* arc length	       */
# endif
);


extern Figure DW_DrawText (
# if NeedFunctionPrototypes
    Widget,		/* drawing		   */
    BOOLEAN,		/* scaled		   */
    FLOAT,		/* lower left x coordinate */
    FLOAT,		/* lower left y coordinate */
    String		/* text string		   */
# endif
);


extern Figure DW_DrawPixmap (
# if NeedFunctionPrototypes
    Widget,		/* drawing		   */
    FLOAT,		/* lower left x coordinate */
    FLOAT,		/* lower left y coordinate */
    Pixmap		/* pixmap		   */
# endif
);


extern Figure DW_DrawBitmap (
# if NeedFunctionPrototypes
    Widget,		/* drawing		   */
    FLOAT,		/* lower left x coordinate */
    FLOAT,		/* lower left y coordinate */
    Pixmap		/* bitmap		   */
# endif
);


extern Figure DW_FindFigure (
# if NeedFunctionPrototypes
    Widget,		/* drawing	*/
    FLOAT,		/* x coordinate	*/
    FLOAT		/* y coordinate	*/
# endif
);


extern void DW_RaiseFigure (
# if NeedFunctionPrototypes
    Widget,		/* drawing */
    Figure		/* figure  */
# endif
);


extern void DW_LowerFigure (
# if NeedFunctionPrototypes
    Widget,		/* drawing */
    Figure		/* figure  */
# endif
);


extern void DW_RemoveFigure (
# if NeedFunctionPrototypes
    Widget,		/* drawing */
    Figure		/* figure  */
# endif
);


extern void DW_RemoveAll (
# if NeedFunctionPrototypes
    Widget		/* drawing */
# endif
);


extern void DW_Redraw (
# if NeedFunctionPrototypes
    Widget		/* drawing */
# endif
);


extern void DW_GetAttributes (
# if NeedFunctionPrototypes
    Widget,		/* drawing    */
    Figure,		/* figure     */
    FigureAttributes *	/* attributes */
# endif
);


extern Boolean DW_SetAttributes (
# if NeedFunctionPrototypes
    Widget,		/* drawing    */
    Figure,		/* figure     */
    unsigned long,	/* mask       */
    FigureAttributes *	/* attributes */
# endif
);


extern void DW_ClipBox (
# if NeedFunctionPrototypes
    Figure,		/* figure   */
    XRectangle *	/* clip box */
# endif
);


extern Boolean DW_SetForeground (
# if NeedFunctionPrototypes
    Widget,		/* drawing    */
    String		/* color name */
# endif
);


extern Boolean DW_SetFont (
# if NeedFunctionPrototypes
    Widget,		/* drawing   */
    String		/* font name */
# endif
);

extern void DW_GetTextExtents (
# if NeedFunctionPrototypes
    Widget,		/* drawing   */
    String,		/* string    */
    float *,		/* width     */
    float *		/* height    */
# endif
);

extern void DW_SetLineWidth (
# if NeedFunctionPrototypes
    Widget,		/* drawing    */
    unsigned		/* line width */
# endif
);


extern void DW_SetLineStyle (
# if NeedFunctionPrototypes
    Widget,		/* drawing    */
    int			/* line style */
# endif
);


extern void DW_SetInteractive (
# if NeedFunctionPrototypes
    Widget,		/* drawing     */
    BOOLEAN		/* interactive */
# endif
);


extern void DW_SetAutoFind (
# if NeedFunctionPrototypes
    Widget,		/* drawing  */
    BOOLEAN		/* autofind */
# endif
);


extern void DW_SetAutoRedraw (
# if NeedFunctionPrototypes
    Widget,		/* drawing    */
    BOOLEAN		/* autoredraw */
# endif
);


extern Figure *DW_SearchArea (
# if NeedFunctionPrototypes
    Widget,		/* drawing	     */
    Point [ ],		/* boundary points   */
    unsigned,		/* number of points  */
    unsigned *		/* number of figures */
# endif
);


extern Figure *DW_RetrieveAll (
# if NeedFunctionPrototypes
    Widget,		/* drawing	     */
    BOOLEAN,		/* visible	     */
    unsigned *		/* number of figures */
# endif
);


extern Figure DW_Group (
# if NeedFunctionPrototypes
    Widget,		/* drawing	     */
    Figure [ ],		/* group members     */
    unsigned		/* number of members */
# endif
);


extern void DW_Ungroup (
# if NeedFunctionPrototypes
    Widget,		/* drawing */
    Figure		/* group   */
# endif
);


extern Pixmap DW_CreatePixmap (
# if NeedFunctionPrototypes
    Widget,		/* drawing */
    unsigned,		/* width   */
    unsigned		/* height  */
# endif
);


extern Pixmap DW_CreateBitmap (
# if NeedFunctionPrototypes
    Widget,		/* drawing */
    unsigned,		/* width   */
    unsigned		/* height  */
# endif
);


extern void DW_TranslateCoords (
# if NeedFunctionPrototypes
    Widget,		/* drawing  */
    int,		/* window x */
    int,		/* window y */
    float *,		/* real x   */
    float *		/* real y   */
# endif
);


extern XtArgVal Float2Arg (
# if NeedFunctionPrototypes
    FLOAT		/* value */
# endif
);


# undef FLOAT
# undef BOOLEAN

# endif /* _Drawing_h */
