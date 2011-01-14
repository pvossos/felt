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
 * File:	functab.h						*
 *									*
 * Description:	This file contains the table containing the names and	*
 *		addresses of each intrinsic function.			*
 ************************************************************************/

# ifndef _FUNCTAB_H
# define _FUNCTAB_H
# include "help.h"
# include "write.h"
# include "fefunc.h"
# include "mathfunc.h"
# include "miscfunc.h"
# include "predicate.h"
# include "matrixfunc.h"

#ifdef __cplusplus
extern "C" {
#endif 

/*----------------------------------------------------------------------*/

# define NumIntrinsics (sizeof (functab) / sizeof (*functab))

static struct {
    const char *name;
    int   num_args;
    int (*func) (int);
} functab [ ] = {

    /* Math library functions */

    {"abs",   1, fabs_func},
    {"ceil",  1, ceil_func},
    {"cos",   1, cos_func},
    {"exp",   1, exp_func},
    {"fabs",  1, fabs_func},
    {"floor", 1, floor_func},
    {"fmod",  2, fmod_func},
    {"hypot", 2, hypot_func},
    {"log",   1, log_func},
    {"log10", 1, log10_func},
    {"pow",   2, pow_func},
    {"sin",   1, sin_func},
    {"sqrt",  1, sqrt_func},
    {"tan",   1, tan_func},


    /* Matrix library functions */

    {"chol",    1, chol_func},
    {"cols",	1, cols_func},
    {"columns",	1, cols_func},
    {"compact",	1, compact_func},
    {"det",	1, det_func},
    {"eig",	2, eig_func},
    {"eye",	2, eye_func},
    {"inv",     1, inv_func},
    {"lu",	4, lu_func},
    {"norm",	2, norm_func},
    {"ones",	2, ones_func},
    {"qr",	3, qr_func},
    {"rand",	3, rand_func},
    {"rows",	1, rows_func},
    {"zeros",	2, zeros_func},
    {"zeroes",	2, zeros_func},


    /* Predicate functions */

    {"any?",	   1, anyp_func},
    {"compact?",   1, compactp_func},
    {"every?",	   1, everyp_func},
    {"matrix?",	   1, matrixp_func},
    {"null?",	   1, nullp_func},
    {"scalar?",	   1, scalarp_func},
    {"symmetric?", 1, symmetricp_func},


    /* Miscellaneous functions */

    {"concat",  2, concat_func},
    {"eval",	1, eval_func},
    {"exit",	1, exit_func},
    {"help",	1, help_func},
    {"history",	1, history_func},
    {"include",	1, include_func},
    {"load",	1, load_func},
    {"read",	0, read_func},
    {"reads",	0, reads_func},
    {"save",	1, save_func},
    {"system",	1, system_func},
    {"type",	1, type_func},
    {"write",  -1, write_func},
    {"writes", -1, writes_func},


    /* Finite element functions */

    {"area",   1, area_func},
    {"felt",   1, felt_func},
    {"length", 1, length_func},
    {"volume", 1, volume_func},

    {"add_definition",	  9, add_definition_func},
    {"remove_definition", 1, remove_definition_func},

    {"assemble",	     2, assemble_func},
    {"clear_nodes",	     0, clear_nodes_func},
    {"compute_modes",	     3, compute_modes_func},
    {"compute_stresses",     1, compute_stresses_func},
    {"construct_forces",     1, construct_forces_func},
    {"find_dofs",	     0, find_dofs_func},
    {"global_dof",	     2, global_dof_func},
    {"integrate_hyperbolic", 3, integrate_hyperbolic_func},
    {"integrate_parabolic",  2, integrate_parabolic_func},
    {"local_dof",	     2, local_dof_func},
    {"remove_constrained",   1, remove_constrained_func},
    {"renumber_nodes",	     0, renumber_nodes_func},
    {"restore_numbers",	     1, restore_numbers_func},
    {"set_up",		     2, set_up_func},
    {"solve_displacements",  2, solve_displacements_func},
    {"zero_constrained",     1, zero_constrained_func},
};

/*----------------------------------------------------------------------*/

#ifdef __cplusplus
}
#endif 

# endif /* _FUNCTAB_H */
