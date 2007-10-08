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
 * File:	Canvas.h						*
 *									*
 * Description:	This file contains the function and type declarations	*
 *		for the canvas dialog box.				*
 ************************************************************************/

# ifndef _Canvas_h
# define _Canvas_h

typedef struct canvas_dialog *CanvasDialog;

typedef struct canvas {
    float	xmin;
    float	xmax;
    float	ymin;
    float	ymax;
    float	scale;
    float	snap_size;
    float	grid_size;
    Boolean	snap;
    Boolean	grid;
    Boolean	node_numbers;
    Boolean	element_numbers;
    String	element_color;
    String	node_color;
    String	tool_color;
    String	tool_font;
    String	label_font;
} *Canvas;

extern Canvas	canvas;

CanvasDialog CanvasDialogCreate (Widget parent, Widget dw, String name, String title);

void CanvasDialogPopup (CanvasDialog canvasd);

void CanvasDialogSet (CanvasDialog canvasd);

# endif /* _Canvas_h */
