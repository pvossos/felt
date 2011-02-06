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
 * File:	vfe.c							*
 *									*
 * Description:	This file contains the function definitions for the	*
 *		visualization of the finite element structures.		*
 ************************************************************************/

# include <algorithm>
# include <stdio.h>
# include <string.h>
# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <X11/Xaw/Viewport.h>
# include "Drawing.h"
# include "Canvas.h"
# include "fe.h"
# include "vfe.h"
# include "error.h"
# include "problem.h"
# include "objects.h"
# include "allocate.h"
# include "globals.h"
# include "procedures.h"

# define Valid(x)	(appearance.x != UnspecifiedValue)
# define Present(x)	(appearance.x != NULL)


extern CanvasDialog	canvas_d;
static FigureAttributes	attributes;


/************************************************************************
 * Function:	DrawDisplayList						*
 *									*
 * Description:	Draws the display list from the appearance structure.	*
 ************************************************************************/

static void DrawDisplayList (void)
{
    unsigned  i;
    unsigned  j;
    Figure    figure;
    Point     points [1024];
    FigInfo  *info;


    /* In case no colors are fonts are specified. */

    figure = NULL;

    if (DW_SetForeground (drawing, canvas -> tool_color) == False)
	DW_SetForeground (drawing, "black");

    if (DW_SetFont (drawing, canvas -> tool_font) == False)
	DW_SetFont (drawing, "fixed");

    for (i = 0; i < appearance.num_figures; i ++) {
	info = &appearance.figures [i];

	if (info -> color)
	    DW_SetForeground (drawing, info -> color);


	switch (info -> type) {
	case RECTANGLE:
	    figure = DW_DrawRectangle (drawing, True, info -> x, info -> y,
					info -> width, info -> height);
	    TreeInsert (figure_tree, figure);
	    break;

	case POLYLINE:
	    if (info -> num_points == 2) {
		figure = DW_DrawLine (drawing, info -> points [0].x,
					info -> points [0].y,
					info -> points [1].x,
					info -> points [1].y);

	    } else if (info -> num_points > 2) {
		for (j = 0; j < info -> num_points; j ++) {
		    points [j].x = info -> points [j].x;
		    points [j].y = info -> points [j].y;
		}

		figure = DW_DrawPolygon (drawing, True, points,
					info -> num_points);
	    }

	    TreeInsert (figure_tree, figure);
	    break;

	case TEXT:
	    if (info -> font)
	        DW_SetFont (drawing, info -> font);

	    figure = DW_DrawText (drawing, True, info -> x, info -> y,
					info -> text);
	    TreeInsert (figure_tree, figure);
	    break;

	case ARC:
	    figure = DW_DrawArc (drawing, True, info -> x, info -> y,
					info -> width, info -> height,
					info -> start, info -> length);
	    TreeInsert (figure_tree, figure);
	    break;
	}
    }
}


/************************************************************************
 * Function:	DrawProblem						*
 *									*
 * Description: Draw the problem.  We try to present a suitable viewing	*
 *		of the problem if the appearance structure is not	*
 *		presence.  If only some of the appearance structure is	*
 *		valid then we try to compromise and keep what		*
 *		information is valid.					*
 ************************************************************************/

void DrawProblem (double z)
{
    unsigned  i;
    Node      n;
    float     xmin, xmax;
    float     ymin, ymax;
    float     scale;
    float     xrange;
    float     yrange;
    Dimension height;
    Dimension width;
    Cardinal  count;
    Boolean   new_limits;
    Arg       arglist [10];


    /* Compute the maximum and minimum values. */

    xmin = 1;
    xmax = -1;
    ymin = 1;
    ymax = -1;

    for (i = 1; i <= problem.nodes.size(); i ++) {
	n = problem.nodes [i];

	if (n -> z == z)
	    if (xmin > xmax) {
		xmin = xmax = n -> x;
		ymin = ymax = n -> y;
	    } else {
		if (n -> x < xmin)
		    xmin = n -> x;
		else if (n -> x > xmax)
		    xmax = n -> x;
		if (n -> y < ymin)
		    ymin = n -> y;
		else if (n -> y > ymax)
		    ymax = n -> y;
	    }
    }

    if (xmin > xmax && !problem.nodes.empty())
        error ("No nodes lie within the plane z = %g.", z);


    /* Expand the minimum and maximum a bit. */

    if (xmin >= xmax) {
	xmin -= .1 * (ymax - ymin);
	xmax += .1 * (ymax - ymin);
    }

    if (ymin >= ymax) {
	ymin -= .1 * (xmax - xmin);
	ymax += .1 * (xmax - xmin);
    }


    if (xmax - xmin < ymax - ymin)
	canvas -> snap_size = canvas -> grid_size = (ymax - ymin) / 10;
    else
	canvas -> snap_size = canvas -> grid_size = (xmax - xmin) / 10;

    xrange = xmax - xmin;
    xmin -= .05 * xrange;
    xmax += .05 * xrange;
    xrange = xmax - xmin;

    yrange = ymax - ymin;
    ymin -= .05 * yrange;
    ymax += .05 * yrange;
    yrange = ymax - ymin;

    new_limits = False;


    /* If the x-axis limits are valid, then use them. */

    if (Valid (x_min) && Valid (x_max))
	if (appearance.x_min < appearance.x_max) {
	    xmin = appearance.x_min;
	    xmax = appearance.x_max;
	    new_limits = True;
	}


    /* If the y-axis limits are valid, then use them. */

    if (Valid (y_min) && Valid (y_max))
	if (appearance.y_min < appearance.y_max) {
	    ymin = appearance.y_min;
	    ymax = appearance.y_max;
	    new_limits = True;
	}


    /* Check the grid size and recompute it if necessary. */

    if (Valid (grid_size))
	canvas -> grid_size = appearance.grid_size;
    else if (new_limits) {
	if (xmax - xmin < ymax - ymin)
	    canvas -> grid_size = (ymax - ymin) / 10;
	else
	    canvas -> grid_size = (xmax - xmin) / 10;
    }


    /* Check the snap size and recompute it if necessary. */

    if (Valid (snap_size))
	canvas -> snap_size = appearance.snap_size;
    else if (new_limits) {
	if (xmax - xmin < ymax - ymin)
	    canvas -> snap_size = (ymax - ymin) / 10;
	else
	    canvas -> snap_size = (xmax - xmin) / 10;
    }


    count = 0;
    XtSetArg (arglist [count], XtNwidth, &width); count ++;
    XtSetArg (arglist [count], XtNheight, &height); count ++;
    XtGetValues (viewport, arglist, count);


    /* If the width, height, and scale are ALL valid, then use them. */

    if (Valid (width) && Valid (height) && Valid (scale)) {
	scale = appearance.scale;
	width = appearance.width;
	height = appearance.height;
    } else {
	if (xrange / width > yrange / height) {
	    scale = width / xrange;
	    ymax += (xrange - yrange) / 2;
	    ymin -= (xrange - yrange) / 2;
	} else {
	    scale = height / yrange;
	    xmax += (yrange - xrange) / 2;
	    xmin -= (yrange - xrange) / 2;
	}
    }


    /* Set up the canvas dialog. */

    canvas -> xmin = xmin;
    canvas -> xmax = xmax;
    canvas -> ymin = ymin;
    canvas -> ymax = ymax;
    canvas -> scale = scale;

    if (Valid (node_numbers))
	canvas -> node_numbers = appearance.node_numbers;

    if (Valid (element_numbers))
	canvas -> element_numbers = appearance.element_numbers;

    if (Present (node_color))
	canvas -> node_color = strdup (appearance.node_color);

    if (Present (element_color))
	canvas -> element_color = strdup (appearance.element_color);

    if (Present (label_font))
	canvas -> label_font = strdup (appearance.label_font);

    if (Present (tool_color))
	canvas -> tool_color = strdup (appearance.tool_color);

    if (Present (tool_font))
	canvas -> tool_font = strdup (appearance.tool_font);

    CanvasDialogSet (canvas_d);


    if (Valid (grid) && appearance.grid != canvas -> grid)
	ToggleGridStatus ( );

    if (Valid (snap) && appearance.snap != canvas -> snap)
	ToggleSnapStatus ( );


    /* Draw the problem. */

    for (i = 1; i <= problem.elements.size(); i ++)
	if (problem.elements [i] != NULL)
         DrawElement (problem.elements [i]);

    for (i = 1; i <= problem.nodes.size(); i ++)
        if (problem.nodes [i] != NULL)
            DrawNode (problem.nodes [i]);

    DrawDisplayList ( );

    count = 0;
    XtSetArg (arglist [count], XtNwidth, width); count ++;
    XtSetArg (arglist [count], XtNheight, height); count ++;
    XtSetValues (viewport, arglist, count);

    XawViewportSetCoordinates (viewport, -appearance.x_pos, -appearance.y_pos);
    InitAppearance ( );
}


/************************************************************************
 * Function:	DestroyProblem						*
 *									*
 * Description:	Destroy the current problem invocation.			*
 ************************************************************************/

void DestroyProblem (MaterialDestructor material_op)
{
    (void) TreeSetDestructor (problem.node_tree, (ItemDestructor)
		DestroyNode);
    (void) TreeDestroy (problem.node_tree);

    (void) TreeSetDestructor (problem.element_tree, (ItemDestructor)
		DestroyElement);
    (void) TreeDestroy (problem.element_tree);

    (void) TreeSetDestructor (problem.force_tree, (ItemDestructor)
		DestroyForce);
    (void) TreeDestroy (problem.force_tree);

    if (material_op)
        std::for_each(problem.material_set.begin(), problem.material_set.end(), material_op);
    problem.material_set.clear();

    (void) TreeSetDestructor (problem.constraint_tree, (ItemDestructor)
		DestroyConstraint);
    (void) TreeDestroy (problem.constraint_tree);

    (void) TreeSetDestructor (problem.distributed_tree, (ItemDestructor)
		DestroyDistributed);
    (void) TreeDestroy (problem.distributed_tree);
}


/************************************************************************
 * Function:	setnodenum						*
 *									*
 * Description:	Sets the node numbering for a specified node.		*
 ************************************************************************/

static int setnodenum (Item item)
{
    static char	     number [10];
    FigureAttributes attr;
    Node	     node;
    Drawn	     drawn;
    float	     x;
    float	     y;


    node = (Node) item;
    drawn = (Drawn) node -> aux;

    if (drawn -> figure == NULL)
	return 0;

    if (drawn -> label == NULL) {
	x = node -> x;
	y = node -> y;
	sprintf (number, " %d", node -> number);
	drawn -> label = DW_DrawText (drawing, True, x, y, number);
	attr.user_data = (char *) node;
	DW_SetAttributes (drawing, drawn -> label, DW_FigureUserData, &attr);
    }

    DW_SetAttributes (drawing, drawn -> label, DW_FigureVisible, &attributes);
    return 0;
}


/************************************************************************
 * Function:	SetNodeNumbering					*
 *									*
 * Description:	Set the node numbering status.				*
 ************************************************************************/

void SetNodeNumbering (int value)
{
    if (DW_SetForeground (drawing, canvas -> node_color) == False)
	(void) DW_SetForeground (drawing, "black");

    if (DW_SetFont (drawing, canvas -> label_font) == False)
	(void) DW_SetFont (drawing, "fixed");

    attributes.visible = value;
    DW_SetAutoRedraw (drawing, False);
    (void) TreeSetIterator (problem.node_tree, setnodenum);
    (void) TreeIterate (problem.node_tree);
    DW_SetAutoRedraw (drawing, True);
}


/************************************************************************
 * Function:	setelementnum						*
 *									*
 * Description:	Sets the element numbering for a specified element.	*
 ************************************************************************/

static int setelementnum (Item item)
{
    static char	     number [10];
    FigureAttributes attr;
    Element	     element;
    Drawn	     drawn;
    float	     x;
    float	     y;


    element = (Element) item;
    drawn = (Drawn) element -> aux;

    if (drawn -> figure == NULL)
	return 0;

    if (drawn -> label == NULL) {
	ComputeCenter (element, &x, &y);
	sprintf (number, "%d", element -> number);
	drawn -> label = DW_DrawText (drawing, True, x, y, number);
	attr.user_data = (char *) element;
	DW_SetAttributes (drawing, drawn -> label, DW_FigureUserData, &attr);
    }

    DW_SetAttributes (drawing, drawn -> label, DW_FigureVisible, &attributes);
    return 0;
}


/************************************************************************
 * Function:	SetElementNumbering					*
 *									*
 * Description:	Set the element numbering status.			*
 ************************************************************************/

void SetElementNumbering (int value)
{
    if (DW_SetForeground (drawing, canvas -> element_color) == False)
	(void) DW_SetForeground (drawing, "black");

    if (DW_SetFont (drawing, canvas -> label_font) == False)
	(void) DW_SetFont (drawing, "fixed");

    attributes.visible = value;
    DW_SetAutoRedraw (drawing, False);
    (void) TreeSetIterator (problem.element_tree, setelementnum);
    (void) TreeIterate (problem.element_tree);
    DW_SetAutoRedraw (drawing, True);
}

static int RecolorNode (Item item)
{
   Node			n;
   FigureAttributes	attrib;
   Drawn		drawn;

   n = (Node) item;
   drawn = (Drawn) n -> aux;

   if (n -> force && !n -> force -> color.c_str()) 
       attrib.color = XtNewString(n -> force -> color.c_str()); 
   else if (!n -> constraint -> color.empty()) 
       attrib.color = XtNewString(n -> constraint -> color.c_str()); 
   else 
       attrib.color = XtNewString(canvas->node_color);

   if (drawn -> figure)
      DW_SetAttributes (drawing, drawn -> figure, DW_FigureColor, &attrib);
   if (drawn -> label)
      DW_SetAttributes (drawing, drawn -> label, DW_FigureColor, &attrib);

   XtFree(attrib.color);
   
   return 0;
}

static int RecolorElement (Item item)
{
   FigureAttributes	attrib;
   Element		e;
   Drawn		drawn;

   e = (Element) item;
   drawn = (Drawn) e -> aux;

   if (e -> numdistributed && !e -> distributed[1] -> color.empty()) 
       attrib.color = XtNewString(e -> distributed[1] -> color.c_str()); 
   else if (!e -> material -> color.empty()) 
       attrib.color = XtNewString(e -> material -> color.c_str()); 
   else 
       attrib.color = XtNewString(canvas -> element_color);

   if (drawn -> figure)
      DW_SetAttributes (drawing, drawn -> figure, DW_FigureColor, &attrib);
   if (drawn -> label)
      DW_SetAttributes (drawing, drawn -> label, DW_FigureColor, &attrib);
   
   XtFree(attrib.color);
   
   return 0;
} 

/************************************************************************
 * Function:	RecolorCanvas						*
 *									*
 * Description:	Resets the color attribute of all nodes and elements	*
 *		based on possibly new values of object coloring		*
 ************************************************************************/

void RecolorCanvas (void)
{
   DW_SetAutoRedraw (drawing, False);

   TreeSetIterator (problem.node_tree, RecolorNode);
   TreeIterate (problem.node_tree);

   TreeSetIterator (problem.element_tree, RecolorElement);
   TreeIterate (problem.element_tree);

   DW_SetAutoRedraw (drawing, True);

   return;   
}
