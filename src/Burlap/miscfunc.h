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
 * File:	miscfunc.h						*
 *									*
 * Description:	This file contains the function declarations for	*
 *		various miscellaneous intrinsic functions.		*
 ************************************************************************/

# ifndef _MISCFUNC_H
# define _MISCFUNC_H
# include "proto.h"			/* function declarations */

extern int concat_func	PROTO ((int));
extern int eval_func	PROTO ((int));
extern int exit_func	PROTO ((int));
extern int history_func	PROTO ((int));
extern int include_func	PROTO ((int));
extern int load_func	PROTO ((int));
extern int read_func	PROTO ((int));
extern int reads_func	PROTO ((int));
extern int save_func	PROTO ((int));
extern int system_func	PROTO ((int));
extern int type_func	PROTO ((int));

# endif /* _MISCFUNC_H */
