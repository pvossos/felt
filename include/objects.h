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
 * File:	objects.h						*
 *									*
 * Description:	This file contains the function declarations for the	*
 *		operations on the various objects.			*
 ************************************************************************/

# ifndef _OBJECTS_H
# define _OBJECTS_H
# include "fe.h"
# include "Tree.h"

extern Distributed CreateDistributed  PROTO ((char *, unsigned));
extern void        DestroyDistributed PROTO ((Distributed));

extern Force       CreateForce        PROTO ((char *));
extern void        DestroyForce       PROTO ((Force));
extern void	   AssignForce	      PROTO ((Force, DOF, Code, char *));
extern void	   AssignSpectrum     PROTO ((Force, DOF, Code, char *));

extern Constraint  CreateConstraint   PROTO ((char *));
extern void        DestroyConstraint  PROTO ((Constraint));
extern void	   AssignConstraint   PROTO ((Constraint, DOF, Code, char *, int));

extern Material    CreateMaterial     PROTO ((char *));
extern void        DestroyMaterial    PROTO ((Material));

extern Node        CreateNode         PROTO ((unsigned));
extern void        DestroyNode        PROTO ((Node));

extern Element     CreateElement      PROTO ((unsigned, Definition));
extern void        DestroyElement     PROTO ((Element));

extern LoadCase	   CreateLoadCase     PROTO ((char *));
extern void        DestroyLoadCase    PROTO ((LoadCase));

# endif /* _OBJECTS_H */
