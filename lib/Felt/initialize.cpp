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
 * File:	initialize.c						*
 *									*
 * Description:	This file contains the function for adding the default	*
 *		element definitions.					*
 ************************************************************************/

# include "definition.h"

extern void trussInit();
extern void beamInit();
extern void beam3dInit();
extern void timoshenkoInit();
extern void iso2d_PlaneStrainInit();
extern void iso2d_PlaneStressInit();
extern void quad_PlaneStrainInit();
extern void quad_PlaneStressInit();
extern void CSTPlaneStrainInit();
extern void CSTPlaneStressInit();
extern void htkInit();
extern void brickInit();
extern void springInit();
extern void ctgInit();
extern void rodInit();
extern void axisymmetricInit();

# ifdef CONTRIB
extern void PLTGZ4Init();
# endif

void
add_all_definitions(void)
{
    trussInit();
    beamInit();
    beam3dInit();
    iso2d_PlaneStrainInit();
    iso2d_PlaneStressInit();
    quad_PlaneStrainInit();
    quad_PlaneStressInit();
    CSTPlaneStrainInit();
    CSTPlaneStressInit();
    timoshenkoInit();
    htkInit();
    brickInit();
    springInit();
    ctgInit();
    rodInit();
    axisymmetricInit();

# ifdef CONTRIB
    PLTGZ4Init();
# endif
}
