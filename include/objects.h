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
 * File:	objects.h						*
 *									*
 * Description:	This file contains the function declarations for the	*
 *		operations on the various objects.			*
 ************************************************************************/

# ifndef _OBJECTS_H
# define _OBJECTS_H
# include "fe.h"

#ifdef __cplusplus
extern "C" {
#endif 

/*----------------------------------------------------------------------*/

/*!
Assigns a force given as a piece of code to a force structure.
*/
void AssignForce(Force force, DOF dof, Code expr, char *text);

/*!
  Assigns a spectrum given as a piece of code to a force	
  structure.						
*/
void AssignSpectrum(Force force, DOF dof, Code expr, char *text);

/*!
  Assigns a boundary condition given as a piece of code to the
  boundary value part of a constraint structure (Tx=, Ty=, Tz=, Rx=,
  Ry=, Rz=).
 */
void AssignConstraint(Constraint constraint, DOF dof, Code expr, char *text, int symbol);

/*----------------------------------------------------------------------*/

#ifdef __cplusplus
}
#endif 

# endif /* _OBJECTS_H */
