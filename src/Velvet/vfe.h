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
 * File:	vfe.h							*
 *									*
 * Description:	This file contains the type and function declarations	*
 *		for the visual representation of the finite element	*
 *		structures.						*
 ************************************************************************/

# ifndef _VFE_H
# define _VFE_H

# include "fe.h"
# include "Item.h"

/*----------------------------------------------------------------------*/

typedef enum {
    DrawnElement, DrawnNode
} DrawnType;

typedef struct drawn {
    DrawnType type;
    Figure    figure;
    Figure    label;
    unsigned  ref_count;
} *Drawn;


void DrawProblem (double z);

typedef int  (*MaterialDestructor) (Material);

void DestroyProblem (MaterialDestructor material_op);

void SetNodeNumbering (int value);

void SetElementNumbering (int value);

void RecolorCanvas (void);

/*----------------------------------------------------------------------*/

# endif /* _VFE_H */
