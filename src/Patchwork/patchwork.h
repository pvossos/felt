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
 * File:	patchwork.h
 * 
 * Description:	contains header information for the conversion routines 
 *
 ****************************************************************************/

# ifndef _PATCHWORK_H
# define _PATCHWORK_H

int InitializeProblem(void);

Node AddNode(double x, double y, double z, Constraint constraint, Force force);

Element AddElement(Definition defn, Node *nodes, Material material, 
                   Distributed *distributed, unsigned numdistributed);

int ReadDXFFile (char *name);
int WriteDXFFile (char *name);
int ReadGraphFile (char *filename);
int WriteGraphFile (char *filename);

int WriteLoganFile(char *name);

int WriteOoglFile (char *filename);

# endif /* _PATCHWORK_H */
