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
 * File:	matrixfunc.h						*
 *									*
 * Description:	This file contains the function declarations for the	*
 *		matrix-related intrinsic functions.			*
 ************************************************************************/

# ifndef _MATRIXFUNC_H
# define _MATRIXFUNC_H
# include "proto.h"			/* function declarations */

extern int chol_func	PROTO ((int));
extern int cols_func	PROTO ((int));
extern int compact_func	PROTO ((int));
extern int det_func	PROTO ((int));
extern int eig_func	PROTO ((int));
extern int eye_func	PROTO ((int));
extern int inv_func	PROTO ((int));
extern int lu_func	PROTO ((int));
extern int norm_func	PROTO ((int));
extern int ones_func	PROTO ((int));
extern int qr_func	PROTO ((int));
extern int rand_func	PROTO ((int));
extern int rows_func	PROTO ((int));
extern int zeros_func	PROTO ((int));

# endif /* _MATRIXFUNC_H */
