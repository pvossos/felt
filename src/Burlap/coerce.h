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
 * File:	coerce.h						*
 *									*
 * Description:	This file contains the function declarations for	*
 *		coercing data from one type to another.			*
 ************************************************************************/

# ifndef _COERCE_H
# define _COERCE_H
# include "descriptor.h"		/* descriptor type definition */

# define CollapseMatrix(d)\
	(D_Type (d) == T_Matrix || D_Type (d) == T_MatrixPtr ?\
	CoerceData (d, T_Double) : (d))

# define CollapseArray(d)\
	(D_Type (d) == T_Array ? CoerceData (d, T_Null) : CollapseMatrix (d))

descriptor *CoerceData (descriptor *d, int type);

descriptor *CoerceToArray (descriptor *d, int type);

# endif /* _COERCE_H */
