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

/*****************************************************************************
 *
 * File:	misc.h
 *
 * Description:	contains prototypes of the element convenience routines
 *		found in element.c
 *
 *****************************************************************************/

# ifndef _MISC_H
# define _MISC_H
# include "proto.h"

extern double   ElementLength PROTO ((Element, unsigned));
extern double   ElementArea PROTO ((Element, unsigned));
extern unsigned	GaussPoints PROTO ((unsigned, double **, double **));
extern Matrix	PlaneStressD PROTO ((Element));
extern Matrix 	PlaneStrainD PROTO ((Element));
extern Matrix 	AxisymmetricD PROTO ((Element));
extern Matrix 	IsotropicD PROTO ((Element));
extern void     MultiplyAtBA PROTO ((Matrix, Matrix, Matrix));
extern void 	ResolveHingeConditions PROTO ((Element));
extern void	SetupStressMemory PROTO ((Element));
extern void	AllocationError PROTO ((Element, char *));
extern void	SetEquivalentForceMemory PROTO ((Element));
extern Matrix   ZeroRowCol PROTO ((Matrix, unsigned));

# endif /* _MISC_H */
