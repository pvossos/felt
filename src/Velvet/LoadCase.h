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
 * File:	LoadCase.h							*
 *									*
 * Description:	This file contains the public function and type		*
 *		declarations for the loadcase dialog box.			*
 ************************************************************************/

# ifndef _LoadCase_h
# define _LoadCase_h

# include "problem.h"
# include "fe.h"
# include "Tree.h"

/*----------------------------------------------------------------------*/

typedef struct loadcase_dialog *LoadCaseDialog;

LoadCaseDialog LoadCaseDialogCreate (Widget parent, String name, String title);

void LoadCaseDialogPopup (LoadCaseDialog loadcased);

void LoadCaseDialogUpdate (LoadCaseDialog loadcased, Tree tree, Problem::ForceSet *force_tree, Problem::DistributedSet *load_tree);

LoadCase LoadCaseDialogActive (LoadCaseDialog loadcased);

void LoadCaseDialogDisplay (LoadCaseDialog loadcased, LoadCase loadcase);

/*----------------------------------------------------------------------*/

# endif /* _LoadCase_h */
