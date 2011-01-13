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
 * File:	draw3d.h						*
 ************************************************************************/

# ifndef _DRAW3d_H
# define _DRAW3d_H

void Setup3D (double min_x, double max_x, double min_y, double max_y, double min_z, double max_z);
void Convert3Dto2D (double x, double y, double z, float xdiff, float ydiff, float *xt, float *yt);

# endif /* _DRAW3d_H */
