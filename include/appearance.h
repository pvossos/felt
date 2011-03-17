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

#include <string>
#include <vector>
#include "appearanceinp.h"

/*----------------------------------------------------------------------*/

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
    std::string color;
    std::string text;
    std::string font;
    std::vector<FigInfoPair> points;
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
    std::string node_color;
    std::string element_color;
    std::string label_font;
    std::string tool_color;
    std::string tool_font;
    std::vector<FigInfo> figures;
} Appearance;

extern Appearance appearance;

/*!
  Initializes the appearance structure, first freeing any existing
  information.
 */
void InitAppearance(void);

/*----------------------------------------------------------------------*/

# endif /* _APPEARANCE_H */
