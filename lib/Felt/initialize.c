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

/************************************************************************
 * File:	initialize.c						*
 *									*
 * Description:	This file contains the function for adding the default	*
 *		element definitions.					*
 ************************************************************************/

# include "definition.h"

extern struct definition trussDefinition;
extern struct definition beamDefinition;
extern struct definition beam3dDefinition;
extern struct definition timoshenkoDefinition;
extern struct definition iso2d_PlaneStrainDefinition;
extern struct definition iso2d_PlaneStressDefinition;
extern struct definition quad_PlaneStrainDefinition;
extern struct definition quad_PlaneStressDefinition;
extern struct definition CSTPlaneStrainDefinition;
extern struct definition CSTPlaneStressDefinition;
extern struct definition htkDefinition;
extern struct definition brickDefinition;
extern struct definition springDefinition;
extern struct definition ctgDefinition;
extern struct definition rodDefinition;
extern struct definition axisymmetricDefinition;

# ifdef CONTRIB
extern struct definition PLTGZ4Definition;
# endif

void add_all_definitions ( )
{
    AddDefinition (&trussDefinition);
    AddDefinition (&beamDefinition);
    AddDefinition (&beam3dDefinition);
    AddDefinition (&iso2d_PlaneStrainDefinition);
    AddDefinition (&iso2d_PlaneStressDefinition);
    AddDefinition (&quad_PlaneStrainDefinition);
    AddDefinition (&quad_PlaneStressDefinition);
    AddDefinition (&CSTPlaneStrainDefinition);
    AddDefinition (&CSTPlaneStressDefinition);
    AddDefinition (&timoshenkoDefinition);
    AddDefinition (&htkDefinition);
    AddDefinition (&brickDefinition);
    AddDefinition (&springDefinition);
    AddDefinition (&ctgDefinition);
    AddDefinition (&rodDefinition);
    AddDefinition (&axisymmetricDefinition);

# ifdef CONTRIB
    AddDefinition (&PLTGZ4Definition);
# endif
}
