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
 * File:	appearance.h						*
 *									*
 * Description:	This file contains the function and type declarations	*
 *		relating to the appearance of the drawing area.		*
 ************************************************************************/

# ifndef _APPEARANCE_H
# define _APPEARANCE_H

#ifdef __cplusplus
extern "C" {
#endif 

/*----------------------------------------------------------------------*/

# define RECTANGLE 0
# define POLYLINE  1
# define TEXT      2
# define ARC       3

typedef struct {
    float x;
    float y;
} FigInfoPair;

typedef struct {
    int		 type;
    float	 x;
    float	 y;
    float	 width;
    float	 height;
    float	 start;
    float	 length;
    char	*color;
    char	*text;
    char	*font;
    unsigned	 num_points;
    FigInfoPair	*points;
} FigInfo;

typedef struct {
    int      node_numbers;
    int      element_numbers;
    int      snap;
    int      grid;
    float    snap_size;
    float    grid_size;
    float    x_min;
    float    x_max;
    float    y_min;
    float    y_max;
    float    scale;
    int      x_pos;
    int      y_pos;
    int      width;
    int      height;
    char    *node_color;
    char    *element_color;
    char    *label_font;
    char    *tool_color;
    char    *tool_font;
    unsigned num_figures;
    FigInfo *figures;
} Appearance;

extern Appearance appearance;

/*!
  Initializes the appearance structure, first freeing any existing
  information.
 */
void InitAppearance(void);

/*----------------------------------------------------------------------*/

#ifdef __cplusplus
}
#endif 

# endif /* _APPEARANCE_H */
