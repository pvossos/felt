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

#ifdef __cplusplus
extern "C" {
#endif 

/*----------------------------------------------------------------------*/

int area_func   (int n);
int felt_func   (int n);
int length_func (int n);
int volume_func (int n);

int add_definition_func	  (int n);
int remove_definition_func (int n);

int assemble_func	     (int n);
int clear_nodes_func	     (int n);
int compute_modes_func	     (int n);
int compute_stresses_func     (int n);
int construct_forces_func     (int n);
int find_dofs_func	     (int n);
int global_dof_func	     (int n);
int integrate_hyperbolic_func (int n);
int integrate_parabolic_func  (int n);
int local_dof_func	     (int n);
int remove_constrained_func   (int n);
int renumber_nodes_func	     (int n);
int restore_numbers_func	     (int n);
int set_up_func		     (int n);
int solve_displacements_func  (int n);
int zero_constrained_func     (int n);

/*----------------------------------------------------------------------*/

#ifdef __cplusplus
}
#endif 

# endif /* _FEFUNC_H */
