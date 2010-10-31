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

#ifdef __cplusplus
extern "C" {
#endif 

/*----------------------------------------------------------------------*/

int chol_func	(int n);
int cols_func	(int n);
int compact_func	(int n);
int det_func	(int n);
int eig_func	(int n);
int eye_func	(int n);
int inv_func	(int n);
int lu_func	(int n);
int norm_func	(int n);
int ones_func	(int n);
int qr_func	(int n);
int rand_func	(int n);
int rows_func	(int n);
int zeros_func	(int n);

/*----------------------------------------------------------------------*/

#ifdef __cplusplus
}
#endif 

# endif /* _MATRIXFUNC_H */
