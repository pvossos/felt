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
 * File:	fefunc.h						*
 *									*
 * Description:	This file contains the function declarations for the	*
 *		finite element intrinsic functions.			*
 ************************************************************************/

# ifndef _FEFUNC_H
# define _FEFUNC_H
# include "proto.h"			/* function declarations */

extern int area_func   PROTO ((int));
extern int felt_func   PROTO ((int));
extern int length_func PROTO ((int));
extern int volume_func PROTO ((int));

extern int add_definition_func	  PROTO ((int));
extern int remove_definition_func PROTO ((int));

extern int assemble_func	     PROTO ((int));
extern int clear_nodes_func	     PROTO ((int));
extern int compute_modes_func	     PROTO ((int));
extern int compute_stresses_func     PROTO ((int));
extern int construct_forces_func     PROTO ((int));
extern int find_dofs_func	     PROTO ((int));
extern int global_dof_func	     PROTO ((int));
extern int integrate_hyperbolic_func PROTO ((int));
extern int integrate_parabolic_func  PROTO ((int));
extern int local_dof_func	     PROTO ((int));
extern int remove_constrained_func   PROTO ((int));
extern int renumber_nodes_func	     PROTO ((int));
extern int restore_numbers_func	     PROTO ((int));
extern int set_up_func		     PROTO ((int));
extern int solve_displacements_func  PROTO ((int));
extern int zero_constrained_func     PROTO ((int));

# endif /* _FEFUNC_H */
